{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveFunctor,
             DeriveFoldable, DeriveTraversable #-}

module Model where

import Data.Monoid
import Data.List
import Data.Foldable (Foldable)

import Control.Monad.Reader
import Control.Monad.State

import Control.Lens hiding (below)
import qualified Data.Text as T

import Config
import Feeds

----------------------------------------------------------------------

data Zip a = Zip
  { _prev :: [a]
  , _curr :: a
  , _next :: [a]
  }
  deriving (Show, Read, Functor, Foldable, Traversable)

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

data VtyInfo = VtyInfo
  { _above    :: Integer
  , _position :: Integer
  , _below    :: Integer
  , _maxRows  :: Integer
  }
  deriving (Show, Read)

makeLenses ''VtyInfo

emptyInfo :: VtyInfo
emptyInfo = VtyInfo 0 0 0 0

data Model = Model
  { _feeds       :: Zip AnnFeed
  , _info        :: VtyInfo
  , _viewing     :: View
  , _downloading :: Int
  }
  deriving (Show, Read)

data View
  = FeedsView
  | ItemsView
      { _items    :: Zip AnnItem
      , _viewText :: Bool
      }
  deriving (Show, Read)

makeLenses ''Model
makeLenses ''View
makePrisms ''View

pretty :: Model -> String
pretty m =
  prettyZip (m^.feeds) (show . _feedTitle)
  ++
  unlines
    [ ""
    , "Viewing: " ++ m^.viewing.to prettyView
    , ""
    , "Above: "    ++ m^.info.above.to show
    , "Position: " ++ m^.info.position.to show
    , "Below: "    ++ m^.info.below.to show
    ]
  where
  prettyZip :: Zip a -> (a -> String) -> String
  prettyZip z s = unlines
    [ "Prev: " ++ z^.prev.traverse.to s
    , "Curr: " ++ z^.curr.to s
    , "Next: " ++ z^.next.traverse.to s
    ]

  prettyView :: View -> String
  prettyView FeedsView            = "FeedsView"
  prettyView (ItemsView is False) = "ItemsView \n\n" ++
                                       prettyZip is (show . _itemTitle . _item)
  prettyView (ItemsView is True)  = "TextView"

viewIso :: Iso' View (Either () (Zip AnnItem, Bool))
viewIso = iso t f
  where
  t FeedsView        = Left ()
  t (ItemsView is b) = Right (is, b)

  f (Left _)        = FeedsView
  f (Right (is, b)) = ItemsView is b

isFeedsView :: Model -> Bool
isFeedsView (Model _ _ FeedsView _) = True
isFeedsView _                       = False

------------------------------------------------------------------------

makeModel :: [AnnFeed] -> Integer -> Model
makeModel []       _    =
  Model (Zip [] (defaultAnn $ newEmptyFeed AtomKind) []) emptyInfo FeedsView 0
makeModel (f : fs) rows =
  Model (Zip [] f                       fs) i         FeedsView 0
  where
  i = emptyInfo & below   .~ rows - 5
                & maxRows .~ rows - 5

emptyFeed :: Feed
emptyFeed = newEmptyFeed AtomKind

dummyFeeds :: Int -> [Feed]
dummyFeeds = reverse . go
  where
  go :: Int -> [Feed]
  go 0 = []
  go n = feed : go (n - 1)
    where
    feed = addDummyItems (emptyFeed & feedTitle .~ "Feed " <> T.pack (show n)) 15

addDummyItems :: Feed -> Int -> Feed
addDummyItems feed = foldr addItem feed . reverse . go
  where
  go 0 = []
  go n = mkItem n : go (n - 1)
    where
    mkItem m = newEmptyItem
             & itemTitle       .~ ("Item " <> T.pack (show m))
             & itemDescription .~ T.pack (concat $ replicate 10 "blah blah")

initialModel :: Config -> Integer -> Model
initialModel cfg rows = makeModel fs rows
  where
  fs :: [AnnFeed]
  fs = cfg^.urls & mapped %~ \url -> defaultAnn $ flip addDummyItems 1 $
         newEmptyFeed AtomKind & feedTitle .~ T.pack url
                               & feedHome  .~ T.pack url

------------------------------------------------------------------------

moveModel :: Dir -> (Model -> Model)
moveModel dir m = case m^.viewing of

  FeedsView -> case dir of
    In  -> m & viewing .~ ItemsView (m^.feeds.curr.feedItems.to makeZip) False
    Out -> error "exiting" -- XXX
    _   -> m & feeds %~ moveZip dir

  ItemsView is txt -> case dir of
    In  -> m & viewing .~ ItemsView (is & curr.isRead .~ True) (not txt)
    Out -> if txt
              then m & viewing .~ ItemsView is False
              else m & viewing .~ FeedsView
                     & feeds.curr.feedItems .~ closeZip is
    _   -> m & viewing.items %~ moveZip dir

moveVtyInfo :: Dir -> (Model -> Model)
moveVtyInfo dir m = case m^.viewing of

  FeedsView -> case dir of
    Down -> m & info.position +~ 1

move :: Dir -> (Model -> Model)
move = moveModel

------------------------------------------------------------------------
{-

move :: Dir -> (Model -> Model)
move = execState . move'

-- XXX: remove the bool?
move' :: Dir -> State Model Bool
move' dir = do
  Model fs i v dl <- get
  case v of
    FeedsView -> case dir of
      Down -> case moveZip Down fs of
        Nothing  -> return False
        Just fs' -> do
          feeds .= fs'
          let pos' = i^.position + 1
          info.position .= pos'
          when (pos' > i^.below) $ do
            info.below .= pos'
            info.above += 1
          return False

      Up -> case moveZip Up fs of
        Nothing  -> return False
        Just fs' -> do
          feeds .= fs'
          let pos' = i^.position - 1
          info.position .= pos'
          when (pos' < i^.above) $ do
            info.above .= pos'
            info.below -= 1
          return False

      Top -> case moveZip Top fs of
        Nothing  -> return False
        Just fs' -> do
          feeds .= fs'
          info.position .= 0
          info.above .= 0
          info.below .= i^.maxRows
          return False

      Bot -> case moveZip Bot fs of
        Nothing  -> return False
        Just fs' -> do
          feeds .= fs'
          let pl = fs'^.prev.to genericLength
          info.position .= pl
          info.above .= pl - i^.maxRows
          info.below .= pl
          return False

      Out -> return True

      In -> do
        put $ Model fs i (ItemsView (makeZip (fs^.curr.feedItems)) False) dl
        return False

    ItemsView is si -> case dir of
      Out -> if si
                then do
                  put $ Model fs i (ItemsView is False) dl
                  return False
                else do
                  put $ Model (fs & curr.feedItems .~ closeZip is) i FeedsView dl
                  return False

      In -> do
        put $ Model fs i (ItemsView (is & curr.isRead .~ True) (not si)) dl
        return False

      Down -> case moveZip Down is of
        Nothing  -> return False
        Just is' -> do
          viewing.items .= is'
          let pos' = i^.position + 1
          info.position .= pos'
          when (pos' > i^.below) $ do
            info.below .= pos'
            info.above += 1
          return False

      Up -> case moveZip Up is of
        Nothing  -> return False
        Just is' -> do
          viewing.items .= is'
          let pos' = i^.position - 1
          info.position .= pos'
          when (pos' < i^.above) $ do
            info.above .= pos'
            info.below -= 1
          return False

      -- XXX: move Top as for feeds in case si is false!
      Top -> case moveZip Top is of
        Nothing  -> return False
        Just is' -> do
          viewing.items .= is'
          info.position .= 0
          info.above .= 0
          info.below .= i^.maxRows
          return False

      Bot -> case moveZip Bot is of
        Nothing  -> return False
        Just is' -> do
          viewing.items .= is'
          let pl = is'^.prev.to genericLength
          info.position .= pl
          info.above .= pl - i^.maxRows
          info.below .= pl
          return False

-}