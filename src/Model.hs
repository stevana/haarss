{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable,
             DeriveTraversable, RecordWildCards #-}

module Model where

import Control.Applicative
import Control.Lens
import Data.Foldable (Foldable)
import Data.Serialize
import Test.QuickCheck

import Constants
import Config
import Feed.Feed
import Feed.Annotated

-- XXX:
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)

----------------------------------------------------------------------

data Zip a = Zip
  { _prev :: [a]
  , _curr :: a
  , _next :: [a]
  }
  deriving (Functor, Foldable, Traversable)

makeLenses ''Zip

data Dir = Up | Down | In | Out | Top | Bot
  deriving (Show, Eq, Ord)

makeZip :: [a] -> Zip a
makeZip [] = error "makeZip: empty"
makeZip (x : xs) = Zip [] x xs

closeZip :: Zip a -> [a]
closeZip (Zip xs z ys) = reverse xs ++ z : ys

prop_makeClose :: Eq a => NonEmptyList a -> Bool
prop_makeClose (NonEmpty xs) = closeZip (makeZip xs) == xs

moveZip :: Dir -> Zip a -> Zip a
moveZip Up   z@(Zip [] _ _)      = z
moveZip Up   (Zip xs c ys)       = Zip (take (length xs - 1) xs) (last xs) (c : ys)
moveZip Down (Zip xs c (y : ys)) = Zip (xs ++ [c]) y ys
moveZip Top  z@(Zip xs c ys)     = case xs of
                                     []      -> z
                                     x : xs' -> Zip [] x (xs' ++ c : ys)
moveZip Bot  z@(Zip xs c ys)     = case ys of
                                     []    -> z
                                     _ : _ -> Zip (xs ++ c : take (length ys - 1) ys)
                                                  (last ys) []
moveZip _    z                   = z

------------------------------------------------------------------------

type Scroll = Int

data Browsing
  = TheFeeds (Zip AnnFeed)
  | TheItems (Zip AnnFeed) (Zip AnnItem)
  | TheText  (Zip AnnFeed) (Zip AnnItem) AnnItem Scroll

makePrisms ''Browsing

feeds :: Lens' Browsing (Zip AnnFeed)
feeds k (TheFeeds fs)       = TheFeeds <$> k fs
feeds k (TheItems fs is)    = (\fs' -> TheItems fs' is)    <$> k fs
feeds k (TheText fs is i s) = (\fs' -> TheText fs' is i s) <$> k fs

move' :: Dir -> (Browsing -> Browsing)
move' In  b@(TheFeeds fs)     = if null is
                                   then b
                                   else TheItems fs (makeZip is)
  where
  is = fs^.curr.feed.feedItems
move' Out b@(TheFeeds _)      = b
move' dir (TheFeeds fs)       = TheFeeds (moveZip dir fs)
move' In  (TheItems fs is)    = TheText fs (is & curr.isRead .~ True)
                                           (is^.curr) 0
move' Out (TheItems fs is)    = TheFeeds (fs & curr.feed.feedItems .~
                                           closeZip is)
move' dir (TheItems fs is)    = TheItems fs (moveZip dir is)
move' In  (TheText fs is _ _) = TheItems fs is
move' Out (TheText fs is _ _) = TheItems fs is
move' dir (TheText fs is _ _) = TheText fs (is' & curr.isRead .~ True)
                                           (is'^.curr) 0
  where
  is' = moveZip dir is

type Height = Int

data VtyStuff
  = ForFeeds Position Height
  | ForItems Position Position Height
  | ForText  Position Position Height

initialVtyStuff :: VtyStuff
initialVtyStuff = ForFeeds (Position 0 0) 0

height :: Lens' VtyStuff Height
height k (ForFeeds p h)   = ForFeeds p   <$> k h
height k (ForItems p q h) = ForItems p q <$> k h
height k (ForText p q h)  = ForText p q  <$> k h

position :: Lens' VtyStuff Position
position k (ForFeeds p h)   = (\p' -> ForFeeds p' h)   <$> k p
position k (ForItems p q h) = (\q' -> ForItems p q' h) <$> k q
position k (ForText p q h)  = (\q' -> ForText p q' h)  <$> k q

data Model = Model
  { _browsing    :: Browsing
  , _downloading :: Int
  , _vty         :: VtyStuff
  }

data Position = Position
  { _window :: Int
  , _cursor :: Int
  }

makeLenses ''Position
makeLenses ''Model

browsingFeeds :: Model -> Bool
browsingFeeds m = m^.browsing & has _TheFeeds

moveVty :: Dir -> Int -> (VtyStuff -> VtyStuff)
moveVty Top _    v = v & position.cursor .~ 0
                       & position.window .~ 0
moveVty Bot tot  v = v & position.cursor .~ min tot (v^.height)
                       & position.window .~ max 0 (tot - v^.height)
moveVty Up _     v
  | v^.position.cursor == 0         = v & position.window %~ max 0 . pred
  | otherwise                       = v & position.cursor -~ 1
moveVty Down tot v
  | v^.position.cursor == tot       = v
  | v^.position.cursor == v^.height = v & position.window %~
      if v^.position.window + v^.height < tot
         then succ
         else id
  | otherwise                       = v & position.cursor +~ 1
moveVty In  _ (ForFeeds p h)        = ForItems p (Position 0 0) h
moveVty Out _ v@(ForFeeds _ _)      = v
moveVty In  _ (ForItems p q h)      = ForText p q h
moveVty Out _ (ForItems p _ h)      = ForFeeds p h
moveVty In  _ (ForText p q h)       = ForItems p q h
moveVty Out _ (ForText p q h)       = ForItems p q h

move :: Dir -> (Model -> Model)
move dir m = m' & vty %~ moveVty dir (m'^.browsing.to total)
  where
  m' = m & browsing %~ move' dir

  -- This is really total-1.
  total :: Browsing -> Int
  total = max 0 . pred . length . closeZip . things
    where
    things :: Browsing -> Zip (Either AnnFeed AnnItem)
    things (TheFeeds fs)      = fmap Left fs
    things (TheItems _ is)    = fmap Right is
    things (TheText _ is _ _) = fmap Right is

-- XXX: Does this work?!
lthings :: Lens' Browsing (Zip (Either AnnFeed AnnItem))
lthings k (TheFeeds fs) = TheFeeds <$> fmap (either id undefined)
                                   <$> k (fmap Left fs)
lthings k (TheItems fs is) =
  (\is' -> TheItems fs is') <$> fmap (either undefined id)
                            <$> k (fmap Right is)
lthings k (TheText fs is i s) =
  (\is' -> TheText fs is' i s) <$> fmap (either undefined id)
                               <$> k (fmap Right is)

------------------------------------------------------------------------

getItemUrl :: Model -> Maybe Text
getItemUrl m =
  m^.browsing._TheItems._2.link <|>
  m^.browsing._TheText._2.link
  where
  link = curr.item.itemLink
{-
getItemUrl m = case m^.browsing of
  TheFeeds _       -> Nothing
  TheItems _ is    -> is^.curr.item.itemLink.to (Just . T.unpack)
  TheText  _ _ i _ -> i^.item.itemLink. to (Just . T.unpack)
-}

search :: Text -> Model -> Model
search t m = case m^.browsing of
  TheFeeds fs    -> case searchZip (matchFeed t) fs 0 of
    Nothing       -> m
    Just (fs', d) -> let total = length (closeZip fs) - 1
                     in m & browsing .~ TheFeeds fs'
                          & vty %~ \v -> iterate (moveVty Down total) v !! d
  TheItems fs is -> case searchZip (matchItem t) is 0 of
    Nothing       -> m
    Just (is', d) -> let total = length (closeZip is) - 1
                     in m & browsing .~ TheItems fs is'
                          & vty %~ \v -> iterate (moveVty Down total) v !! d
  TheText {..}   -> m
  where
  matchFeed :: Text -> AnnFeed -> Bool
  matchFeed t' f = t' `matchText` (f^.feed.feedTitle)

  matchItem :: Text -> AnnItem -> Bool
  matchItem t' i = t' `matchText` (i^.item.itemTitle)

  matchText :: Text -> Maybe Text -> Bool
  matchText _  Nothing    = False
  matchText t' (Just t'') = T.toCaseFold t' `T.isInfixOf` T.toCaseFold t''

searchZip :: (a -> Bool) -> Zip a -> Int -> Maybe (Zip a, Int)
searchZip f z d | z^.next.to null = Nothing
                | z^.curr.to f    = Just (z, d)
                | otherwise       = searchZip f (moveZip Down z) (d + 1)

------------------------------------------------------------------------

instance Show Model where
  show m = unlines
    [ "Browsing: " ++ m^.browsing.to show
    , ""
    , "VtyStuff: " ++ m^.vty.to show
    , ""
    , "Downloading: " ++ m^.downloading.to show
    ]

instance Show Browsing where
  show (TheFeeds fs)     = "TheFeeds." ++ show fs
  show (TheItems _ is)   = "TheItems." ++ show is
  show (TheText _ _ i s) = unlines
    [ "TheText: " ++ i^.item.itemDescription.to show
    , ""
    , "Scroll: " ++ show s
    ]

indent :: [String] -> [String]
indent = (:) "\n" . map ("  " ++)

instance Show a => Show (Zip a) where
  show z = unlines $ indent
    [ "prev: " ++ z^.prev.to show
    , "curr: " ++ z^.curr.to show
    , "next: " ++ z^.next.to show
    ]

instance Show VtyStuff where
  show v = unlines $ indent
    [ "window: " ++ v^.position.window.to show
    , "cursor: " ++ v^.position.cursor.to show
    , "height: " ++ v^.height.to show
    ]

------------------------------------------------------------------------

makeModel :: [AnnFeed] -> Model
makeModel fs = Model (TheFeeds (makeZip fs)) 0 initialVtyStuff

initialModel :: Config -> Model
initialModel cfg = makeModel fs
  where
  fs :: [AnnFeed]
  fs = cfg^.urls & mapped %~ \url -> defaultAnn $
         newEmptyFeed AtomKind & feedTitle ?~ T.pack url
                               & feedHome  .~ T.pack url

-- XXX: Better name? Explain magic 6.
setHeight :: Int -> Model -> Model
setHeight h m = m & vty.height .~ h - 6

------------------------------------------------------------------------

-- XXX: Move to Model.Serialise, add writeModel

readSavedModel :: Config -> IO Model
readSavedModel cfg = do
  modelPath <- getModelPath
  exists    <- doesFileExist modelPath

  if not exists
    then return $ initialModel cfg
    else do
      em <- decode <$> BS.readFile modelPath
      case em of
        Left _  -> error $ "readSavedModel: failed to restore saved model."
        Right m -> return m

------------------------------------------------------------------------

instance Serialize Model where
  put m = put $ m^.browsing._TheFeeds.to closeZip
  get   = makeModel <$> get
