{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable,
             DeriveTraversable, DeriveGeneric #-}

module Model where

import Control.Applicative
import Control.Lens
import Data.Foldable (Foldable)
import GHC.Generics (Generic)

import Constants
import Config
import Feeds

-- XXX:
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.Directory (doesFileExist)

----------------------------------------------------------------------

data Zip a = Zip
  { _prev :: [a]
  , _curr :: a
  , _next :: [a]
  }
  deriving (Functor, Foldable, Traversable, Generic)

makeLenses ''Zip

data Dir = Up | Down | In | Out | Top | Bot
  deriving (Show, Eq, Ord)

makeZip :: [a] -> Zip a
makeZip [] = error "makeZip: empty"
makeZip (x : xs) = Zip [] x xs

closeZip :: Zip a -> [a]
closeZip (Zip xs z ys) = reverse xs ++ z : ys

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
  deriving Generic

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
  deriving Generic

initialVtyStuff :: VtyStuff
initialVtyStuff = ForFeeds (Position 0 0) 0

height :: Lens' VtyStuff Height
height k (ForFeeds p h)   = ForFeeds p <$> k h
height k (ForItems p q h) = ForItems p q <$> k h

position :: Lens' VtyStuff Position
position k (ForFeeds p h)   = (\p' -> ForFeeds p' h) <$> k p
position k (ForItems p q h) = (\q' -> ForItems p q' h) <$> k q

data Model = Model
  { _browsing    :: Browsing
  , _downloading :: Int
  , _vty         :: VtyStuff
  }
  deriving Generic

data Position = Position
  { _window :: Int
  , _cursor :: Int
  }
  deriving Generic

makeLenses ''Position
makeLenses ''Model

browsingFeeds :: Model -> Bool
browsingFeeds m = maybe False (const True) $ m ^? browsing._TheFeeds

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
moveVty In  _ v@(ForItems _ _ _)    = v
moveVty Out _ (ForItems p _ h)      = ForFeeds p h

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

------------------------------------------------------------------------

getItemUrl :: Model -> Maybe String
getItemUrl m = case m^.browsing of
  TheFeeds _       -> Nothing
  TheItems _ is    -> is^.curr.item.itemLink.to (Just . T.unpack)
  TheText  _ _ i _ -> i^.item.itemLink. to (Just . T.unpack)

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
         newEmptyFeed AtomKind & feedTitle .~ T.pack url
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
instance Serialize Browsing where
instance Serialize VtyStuff where
instance Serialize Position where
instance Serialize a => Serialize (Zip a) where
