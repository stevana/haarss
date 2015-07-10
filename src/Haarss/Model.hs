{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Haarss.Model where

import           Prelude              hiding (foldl, foldr)

import           Control.Applicative
import           Control.Lens         hiding (below, (<.>))
import           Control.Monad
import           Data.Foldable
import           Data.Hashable
import           Data.IntMap.Strict   (IntMap)
import qualified Data.IntMap.Strict   as M
import           Data.Maybe           (fromMaybe)
import           Data.Serialize
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Lens
import           Test.Framework       hiding (resize)

-- XXX:
import qualified Data.ByteString      as BS
import           Data.Time
import           Graphics.Vty.Prelude
import           System.Directory
import           System.FilePath
#if __GLASGOW_HASKELL__ < 710
import           System.Locale (defaultTimeLocale, rfc822DateFormat)
#endif

import           Haarss.Config
import           Haarss.Feed.Annotated
import           Haarss.Interface
import           Haarss.Model.Window

import           Yeast.Feed

------------------------------------------------------------------------
-- * Datatypes

data Model = Model
  { _browsing      :: Browse
  , _downloading   :: Int
  , _prompt        :: Maybe (Prompt, String)
  , _displayRegion :: DisplayRegion
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
      , _scroll   :: [ScrollDir]
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

modelItems :: Lens' Model (Window AnnItem)
modelItems = lens g s
  where
  g :: Model -> Window AnnItem
  g m = case m^.browsing.focus of
    TheFeed  f      -> if f^.feed.items.to null
                       then error "items: Feed has no items."
                       else f^.feed.items.to
                              (makeWindow (m^.feeds.to size))
    TheItems _ is   -> is
    TheText  _ is _ -> is

  s :: Model -> Window AnnItem -> Model
  s m w = case m^.browsing.focus of
    TheFeed  f      -> m & browsing.focus .~ TheFeed (f & feed.items .~
                                                            closeWindow w)
    TheItems f _    -> m & browsing.focus .~ TheItems f w
    TheText  f _ i  -> m & browsing.focus .~ TheText  f w i

-- Note that this is only true modulo not caring about the size.
prop_itemsSetView :: Model -> Window AnnItem -> Bool
prop_itemsSetView m w =
  closeWindow w == (m & modelItems .~ w)^.modelItems.to closeWindow

prop_itemsViewSet :: Model -> Bool
prop_itemsViewSet m
    -- XXX: This is bad -- see error thrown in @items@.
  | m^.feeds.focus.feed.items.to length == 0 = True
  | otherwise =  m == (m & modelItems .~ m^.modelItems)

prop_itemsSetSet :: Model -> Window AnnItem -> Window AnnItem -> Bool
prop_itemsSetSet m w1 w2 =
  ((m & modelItems .~ w1) & modelItems .~ w2) == (m & modelItems .~ w2)

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
    , m^.modelItems.to show
    , ""
    , "Size   of items window: " ++ m^.modelItems.to (show . size)
    , "Length of items window: " ++
        m^.modelItems.to (show . length . closeWindow)
    ]
  show m                   = unlines
    [ "Browsing the text."
    , ""
    , m^.browsing.focus.annItems.focus.item.description
        .to (fromMaybe "(no desc)").to T.unpack
    , ""
    , "Scroll: " ++ m^.browsing.focus.scroll.to show
    ]

instance Show Focus where
  show (TheFeed f)       = show f
  show (TheItems _ is)   = show is
  show (TheText _ is s) = unlines
    [ "TheText: " ++ is^.focus.item.description.to show
    , ""
    , "Scroll: " ++ show s
    ]

instance Arbitrary Model where
  arbitrary = liftM4 Model arbitrary arbitrary arbitrary arbitrary

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
makeModel fs = Model (fmap TheFeed (makeWindow 20 fs)) 0 Nothing (0, 0)

initialModel :: UTCTime -> Config -> Model
initialModel time cfg = makeModel (addOverviewFeed time fs)
  where
  fs :: [AnnFeed]
  fs = cfg^.entries & mapped %~ \(_, u, is) -> AnnFeed
    { _feed    = emptyFeed AtomKind
                   & title    ?~ T.pack u
                   & feedHome ?~ T.pack u
                   & date     ?~ T.pack (formatTime defaultTimeLocale
                                   rfc822DateFormat time)
                   & items    .~ []
    , _alias   = Nothing
    , _history = []
    , _ignore  = is
    }

-- XXX: The magic -5...
-- XXX: We should probably resize items window size as well, but @items@
-- is partial so we can't do it straightforwardly...
resizeModel :: DisplayRegion -> Model -> Model
resizeModel sz m = m & displayRegion .~ sz
                     & feeds         %~ resize (regionHeight sz - 5)

------------------------------------------------------------------------
-- * Movement

moveWin :: Dir -> Window a -> Window a
moveWin Up   = up
moveWin Down = down
moveWin Top  = top
moveWin Bot  = bot
moveWin _    = id

moveWin' :: Dir -> (a -> b) -> (b -> a) -> Window' a b -> Window' a b
moveWin' d f g = fmap f . moveWin d . fmap g

moveBrowse :: Dir -> Int -> Browse -> Browse
moveBrowse d h fz = case fz^.focus of

  TheFeed  f      -> case d of
    In  -> let is = f^.feed.items
           in if not (null is)
              then fz & focus .~ TheItems f (makeWindow (h - 7) is)
              else fz
    Out -> fz
    _   -> moveWin' d TheFeed _annFeed fz

  TheItems f iz   -> case d of
    In  -> fz & focus .~ TheText  f (iz & focus.isRead .~ True) []
    Out -> fz & focus .~ TheFeed (f & feed.items .~ closeWindow iz)
    _   -> fz & focus .~ TheItems f (moveWin d iz)

  TheText  f iz _ -> case d of
    In  -> fz & focus .~ TheItems f iz
    Out -> fz & focus .~ TheItems f iz
    _   -> fz & focus .~ TheText  f (moveWin d iz & focus.isRead .~ True) []

move :: Dir -> Model -> Model
move d m = m & browsing %~ moveBrowse d (m^.displayRegion.to regionHeight)

------------------------------------------------------------------------

browsingFeeds :: Model -> Bool
browsingFeeds m = m^.browsing.focus & has _TheFeed

browsingItems :: Model -> Bool
browsingItems m = m^.browsing.focus & has _TheItems

getFeedUrl :: Model -> String
getFeedUrl m = m^.feeds.focus.feed.feedHome.to (fromMaybe "").unpacked

-- XXX: Magic drop 1...
getFeedUrls :: Model -> [String]
getFeedUrls m = m^.feeds.to closeWindow.to (drop 1) & mapped %~
  \f -> f^.feed.feedHome.to (fromMaybe "").unpacked

getItemUrl :: Model -> Maybe String
getItemUrl m = m^.browsing.focus.annItems.focus.item.link.to (fmap T.unpack)

------------------------------------------------------------------------

update :: Op o -> Cmd o -> Resp o -> Model -> Model
update Move          d   ()  m = move d m
update UpdateFeeds   _   tfs m = feedsDownloaded tfs m
update OpenUrl       _   ()  m = m
update MarkAllAsRead _   ()  m = markAllAsRead m
update MarkAsRead    ()  ()  m =
  -- XXX: This won't work for the overview feed.
  m & browsing.focus.annItems.focus.isRead %~ not
update OpenPrompt    p   ()  m = m & prompt ?~ (p, "")
update PutPrompt     c   ()  m = m & prompt._Just._2 %~ (++ [c])
update DelPrompt     ()  ()  m = m & prompt._Just._2 %~
                                       \s -> if null s
                                             then ""
                                             else init s
update CancelPrompt  ()  ()  m = m & prompt .~ Nothing
update ClosePrompt   ()  ()  m = case m^.prompt of
  Just (SearchPrompt, s) -> search (T.pack s) m'
  Just (RenameFeed,   s) -> m' & browsing.focus.annFeed.alias ?~ T.pack s
  Just (AddFeed,      s) -> addFeed s m'
  _                      -> m'
  where
  m' = m & prompt .~ Nothing
update RemoveFeed    ()   () m = m & feeds %~ remove
update Rearrange     Up   () m = m & feeds %~ rearrangeUp
update Rearrange     Down () m = m & feeds %~ rearrangeDown
update Scroll        d    () m = m & browsing.focus.scroll %~ cons d

update Rearrange     _    _  _ = error "update: Impossible"
update Search        _    _  _ = error "update: Impossible"
update Resize        _    _  _ = error "update: Impossible"
update Quit          _    _  _ = error "update: Impossible"

------------------------------------------------------------------------

feedback :: Feedback -> Model -> Model
feedback (Downloading n) m = m & downloading .~ n
feedback FeedDownloaded  m = m & downloading -~ 1

------------------------------------------------------------------------

-- XXX: Magic string...
markAllAsRead :: Model -> Model
markAllAsRead m
  | browsingFeeds m
  = m & feeds.both.feed.items.traverse.isRead .~ True

  | browsingItems m &&
    m^.feeds.focus.feed.title == Just "(New headlines)"
  = m & feeds.both.feed.items.traverse.isRead .~ True
      & modelItems.both.isRead .~ True

  -- XXX: we should update overview feed also...
  | otherwise = m & modelItems.both.isRead .~ True
                  & feeds.focus.feed.items.traverse.isRead .~ True

updateOverviewFeed :: UTCTime -> Window AnnFeed -> Window AnnFeed
updateOverviewFeed time w =
  replace 0 (makeOverview time (drop 1 (closeWindow w))) w

feedsDownloaded :: (UTCTime, [AnnFeed]) -> Model -> Model
-- XXX: Overview feed specific...
feedsDownloaded (time, [f]) m | m^.feeds.to (length . closeWindow) > 2 =
  if browsingItems m
     then m' & modelItems .~ makeWindow (m'^.modelItems.to size)
                               (mergeItems (f^.ignore)
                                  (m^.modelItems.to closeWindow)
                                  (m'^.feeds.focus.feed.items))
     else m'
  where
  m'  = m & feeds.focus %~ flip mergeFeed f
          & feeds %~ updateOverviewFeed time

feedsDownloaded (time, fs)  m =
  m & feeds .~ makeWindow (m^.feeds.to size)
                 (addOverviewFeed time (mergeFeeds
                   (m^.feeds.to (drop 1 . closeWindow))
                   fs))

search :: Text -> Model -> Model
search t m | browsingFeeds m = m & feeds      %~ findFirst (matchFeed t)
           | browsingItems m = m & modelItems %~ findFirst (matchItem t)
           | otherwise       = m
  where
  matchFeed :: Text -> AnnFeed -> Bool
  matchFeed t' f = t' `matchText` (f^.alias <|> f^.feed.title)

  matchItem :: Text -> AnnItem -> Bool
  matchItem t' i = t' `matchText` (i^.item.title)

  matchText :: Text -> Maybe Text -> Bool
  matchText _  Nothing    = False
  matchText t' (Just t'') = T.toCaseFold t' `T.isInfixOf` T.toCaseFold t''

addFeed :: String -> Model -> Model
addFeed url m = m & feeds %~ add f
  where
  f :: AnnFeed
  f = emptyAnnFeed & feed.title    ?~ T.pack url
                   & feed.feedHome ?~ T.pack url

------------------------------------------------------------------------

loadModel :: Config -> IO Model
loadModel cfg = do
  modelPath <- getAppUserDataDirectory $ "haarss" </> "model"
  exists    <- doesFileExist modelPath
  time      <- getCurrentTime

  if not exists
    then return $ initialModel time cfg
    else do
      em <- decode <$> BS.readFile modelPath
      case em of
        Left _  -> error "loadModel: failed to restore saved model."
        Right m -> return $ updateModel m
  where
  updateModel :: Model -> Model
  updateModel m = m & feeds .~
    (makeWindow (m^.feeds.to size) $ flip map (cfg^.entries) $ \e ->
      case M.lookup (e^._2.to hash) im of
        Nothing -> emptyAnnFeed & feed.title     ?~ e^._2.packed
                                & feed.feedHome  ?~ e^._2.packed
                                & alias          .~ (e^._1 & mapped
                                                           %~ T.pack)
                                & ignore         .~ (e^._3)
        Just f  -> f & alias  .~ (e^._1 & mapped %~ T.pack)
                     & ignore .~ (e^._3))
    where
    im :: IntMap AnnFeed
    im = M.fromList $ map (\f -> (f^.feed.feedHome.to hash, f))
       $ m^.feeds.to closeWindow

saveModel :: [AnnFeed] -> IO ()
saveModel fs = do
  modelPath <- getAppUserDataDirectory $ "haarss" </> "model"
  BS.writeFile modelPath $ encode fs
  copyFile modelPath $ modelPath <.> "backup"
