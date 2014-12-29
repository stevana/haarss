{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveTraversable,
             DeriveFoldable, OverloadedStrings #-}

module Model where

import Prelude hiding (foldr, foldl)

import Control.Applicative
import Control.Monad
import Control.Lens hiding (below)
import Data.Foldable
import qualified Data.Sequence as Seq
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck hiding (resize)

-- XXX:
import qualified Data.ByteString as BS
import Data.Time
import System.Directory
import System.Locale
import Graphics.Vty.Prelude

import Config
import Constants
import Feed.Feed
import Feed.Annotated
import Model.Window

------------------------------------------------------------------------
-- * Datatypes

data Model = Model
  { _browsing    :: Browse
  , _downloading :: Int
  }
  deriving Eq

type Browse = Window' AnnFeed Focus

data Focus
  = TheFeed
      { _annFeed  :: AnnFeed
      }
  | TheItems
      { _annFeed  :: AnnFeed
      , _annItems :: Window AnnItem
      }
  | TheText
      { _annFeed  :: AnnFeed
      , _annItems :: Window AnnItem
      , _scroll   :: Int
      }
  deriving Eq

makeLenses ''Model
makeLenses ''Focus
makePrisms ''Focus

feeds :: Lens' Model (Window AnnFeed)
feeds = lens g s
  where
  g :: Model -> Window AnnFeed
  g m = m^.browsing & mapped %~ _annFeed

  s :: Model -> Window AnnFeed -> Model
  s m w = m & browsing.above         .~ w^.above
            & browsing.prev          .~ w^.prev
            & browsing.focus.annFeed .~ w^.focus
            & browsing.next          .~ w^.next
            & browsing.below         .~ w^.below

prop_feedsSetView :: Model -> Window AnnFeed -> Bool
prop_feedsSetView m w = w == (m & feeds .~ w)^.feeds

prop_feedsViewSet :: Model -> Bool
prop_feedsViewSet m = m == (m & feeds .~ m^.feeds)

prop_feedsSetSet :: Model -> Window AnnFeed -> Window AnnFeed -> Bool
prop_feedsSetSet m w1 w2 =
  ((m & feeds .~ w1) & feeds .~ w2) == (m & feeds .~ w2)

items :: Lens' Model (Window AnnItem)
items = lens g s
  where
  g :: Model -> Window AnnItem
  g m = case m^.browsing.focus of
    TheFeed  f      -> f^.feed.feedItems.to (makeWindow (m^.feeds.to size))
    TheItems _ is   -> is
    TheText  _ is _ -> is

  s :: Model -> Window AnnItem -> Model
  s m w = case m^.browsing.focus of
    TheFeed  f      -> m & browsing.focus .~ TheFeed (f & feed.feedItems .~
                                                            closeWindow w)
    TheItems f _    -> m & browsing.focus .~ TheItems f w
    TheText  f _ i  -> m & browsing.focus .~ TheText  f w i

-- Note that this is only true modulo not caring about the size.
prop_itemsSetView :: Model -> Window AnnItem -> Bool
prop_itemsSetView m w = closeWindow w == (m & items .~ w)^.items.to closeWindow

prop_itemsViewSet :: Model -> Bool
prop_itemsViewSet m = m == (m & items .~ m^.items)

prop_itemsSetSet :: Model -> Window AnnItem -> Window AnnItem -> Bool
prop_itemsSetSet m w1 w2 =
  ((m & items .~ w1) & items .~ w2) == (m & items .~ w2)

instance Show Model where
  show m | browsingFeeds m = unlines
    [ "Browsing feeds."
    , ""
    , m^.feeds.to show
    , ""
    , "Size   of feeds window: " ++ m^.feeds.to (show . size)
    , "Length of feeds window: " ++
        m^.feeds.to (show . length . closeWindow)
    , ""
    , "Downloading: " ++ m^.downloading.to show
    ]
  show m | browsingItems m = unlines
    [ "Browsing items."
    , ""
    , m^.items.to show
    , ""
    , "Size   of items window: " ++ m^.items.to (show . size)
    , "Length of items window: " ++
        m^.items.to (show . length . closeWindow)
    ]
  show m | otherwise       = unlines
    [ "Browsing the text."
    , ""
    , m^.browsing.focus.annItems.focus.item.itemDescription
        .be "(no desc)".to T.unpack
    , ""
    , "Scroll: " ++ m^.browsing.focus.scroll.to show
    ]

instance Show Focus where
  show (TheFeed f)       = show f
  show (TheItems _ is)   = show is
  show (TheText _ is s) = unlines
    [ "TheText: " ++ is^.focus.item.itemDescription.to show
    , ""
    , "Scroll: " ++ show s
    ]

instance Arbitrary Model where
  arbitrary = liftM2 Model arbitrary arbitrary

instance Arbitrary Focus where
  arbitrary = oneof
    [ liftM  TheFeed  arbitrary
    , liftM2 TheItems arbitrary arbitrary
    , liftM3 TheText  arbitrary arbitrary arbitrary
    ]

instance Serialize Model where
  put m = put $ m^.browsing.to (closeWindow' _annFeed)
  get   = makeModel <$> get

prop_serialisation :: NonEmptyList AnnFeed -> [Dir] -> Bool
prop_serialisation (NonEmpty fs) dirs =
  m'^.browsing.to (closeWindow' _annFeed) ==
  m ^.browsing.to (closeWindow' _annFeed)
  where
  moves :: [Model -> Model]
  moves = map move $ dirs ++ [Out, Out, Out]

  m :: Model
  m = foldl (\ih f -> f ih) (makeModel fs) moves

  m' :: Model
  m' = decode (encode m)^?!_Right

------------------------------------------------------------------------

makeModel :: [AnnFeed] -> Model
makeModel fs = Model (fmap TheFeed (makeWindow 20 fs)) 0

initialModel :: UTCTime -> Config -> Model
initialModel time cfg = makeModel (addOverviewFeed time fs)
  where
  fs :: [AnnFeed]
  fs = cfg^.urls & mapped %~ \url -> defAnnFeed $
         newEmptyFeed AtomKind
           & feedTitle      ?~ T.pack url
           & feedHome       .~ T.pack url
           & feedLastUpdate ?~ T.pack (formatTime defaultTimeLocale
                                 rfc822DateFormat time)

-- XXX: The magic -5...
resizeModel :: DisplayRegion -> Model -> Model
resizeModel sz m = m & feeds %~ resize (regionHeight sz - 5)

------------------------------------------------------------------------
-- * Movement

data Dir = Up | Down | In | Out | Top | Bot
  deriving (Show, Eq, Enum)

instance Arbitrary Dir where
  arbitrary = Test.QuickCheck.elements [Up, Down, In , Out, Top, Bot]

moveWin :: Dir -> Window a -> Window a
moveWin Up   = up
moveWin Down = down
moveWin Top  = top
moveWin Bot  = bot
moveWin _    = id

moveWin' :: Dir -> (a -> b) -> (b -> a) -> Window' a b -> Window' a b
moveWin' d f g = fmap f . moveWin d . fmap g

moveBrowse :: Dir -> Browse -> Browse
moveBrowse d fz = case fz^.focus of

  TheFeed  f      -> case d of
    In  -> let is = f^.feed.feedItems
           in if not (null is)
              then fz & focus .~ TheItems f (makeWindow (size fz - 2) is)
              else fz
    Out -> fz
    _   -> moveWin' d TheFeed _annFeed fz

  TheItems f iz   -> case d of
    In  -> fz & focus .~ TheText  f (iz & focus.isRead .~ True) 0
    Out -> fz & focus .~ TheFeed (f & feed.feedItems .~ closeWindow iz)
    _   -> fz & focus .~ TheItems f (moveWin d iz)

  TheText  f iz s -> case d of
    In  -> fz & focus .~ TheItems f iz
    Out -> fz & focus .~ TheItems f iz
    _   -> fz & focus .~ TheText  f (moveWin d iz & focus.isRead .~ True) s

move :: Dir -> (Model -> Model)
move d m = m & browsing %~ moveBrowse d

------------------------------------------------------------------------

browsingFeeds :: Model -> Bool
browsingFeeds m = m^.browsing.focus & has _TheFeed

browsingItems :: Model -> Bool
browsingItems m = m^.browsing.focus & has _TheItems

getFeedUrl :: Config -> Model -> Maybe String
getFeedUrl cfg m = cfg^.urls^? ix (i - 1)
  where
  i = m^.browsing.prev.to  Seq.length +
      m^.browsing.above.to Seq.length

getItemUrl :: Model -> Maybe Text
getItemUrl m = m^.browsing.focus.annItems.focus.item.itemTitle

------------------------------------------------------------------------

-- XXX: This won't work for the overview feed.
toggleReadStatus :: Model -> Model
toggleReadStatus m = m & browsing.focus.annItems.focus.isRead %~ not

-- XXX: Magic string...
makeAllAsRead :: Model -> Model
makeAllAsRead m
  | browsingFeeds m
  = m & feeds.both.feed.feedItems.traverse.isRead .~ True

  | browsingItems m &&
    m^.feeds.focus.feed.feedTitle == Just "(New headlines)"
  = m & feeds.both.feed.feedItems.traverse.isRead .~ True
      & items.both.isRead .~ True

  | otherwise = m & items.both.isRead .~ True

feedDownloaded :: AnnFeed -> Model -> Model
feedDownloaded f m =
  m & browsing.focus.annFeed %~ flip Feed.Annotated.merge f
    & downloading            .~ 0

feedsDownloaded :: (UTCTime, [AnnFeed]) -> Model -> Model
feedsDownloaded (time, fs) m =
  m & feeds .~ makeWindow (m^.feeds.to size)
                 (addOverviewFeed time (mergeFeeds
                   (m^.feeds.to (drop 1 . closeWindow))
                   fs))

search :: Text -> Model -> Model
search t m | browsingFeeds m = m & feeds %~ findFirst (matchFeed t)
           | browsingItems m = m & items %~ findFirst (matchItem t)
           | otherwise       = m
  where
  matchFeed :: Text -> AnnFeed -> Bool
  matchFeed t' f = t' `matchText` (f^.feed.feedTitle)

  matchItem :: Text -> AnnItem -> Bool
  matchItem t' i = t' `matchText` (i^.item.itemTitle)

  matchText :: Text -> Maybe Text -> Bool
  matchText _  Nothing    = False
  matchText t' (Just t'') = T.toCaseFold t' `T.isInfixOf` T.toCaseFold t''

------------------------------------------------------------------------

-- XXX: Move to Model.Serialise, add writeModel
-- XXX: Use: getAppUserDataDirectory
readSavedModel :: Config -> IO Model
readSavedModel cfg = do
  modelPath <- getModelPath
  exists    <- doesFileExist modelPath
  time      <- getCurrentTime

  if not exists
    then return $ initialModel time cfg
    else do
      em <- decode <$> BS.readFile modelPath
      case em of
        Left _  -> error $ "readSavedModel: failed to restore saved model."
        Right m -> return m
