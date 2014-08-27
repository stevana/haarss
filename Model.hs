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

import Feeds

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

moveZip :: Dir -> Zip a -> Maybe (Zip a)
moveZip Up   (Zip [] _ _) = Nothing
moveZip Up   (Zip xs z ys) = Just $ Zip (take (length xs - 1) xs) (last xs) (z : ys)
moveZip Down (Zip xs z (y : ys)) = Just $ Zip (xs ++ [z]) y ys
moveZip Top  (Zip xs z ys)       = case xs of
                                     []      -> Nothing
                                     x : xs' -> Just $ Zip [] x (xs' ++ z : ys)
moveZip Bot  (Zip xs z ys)       = case ys of
                                     []    -> Nothing
                                     _ : _ -> Just $ Zip (xs ++ z : take (length ys - 1) ys)
                                                         (last ys) []
moveZip _    _                   = Nothing

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
  { _feeds   :: Zip AnnFeed
  , _info    :: VtyInfo
  , _viewing :: View
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

viewIso :: Iso' View (Either () (Zip AnnItem, Bool))
viewIso = iso t f
  where
  t FeedsView        = Left ()
  t (ItemsView is b) = Right (is, b)

  f (Left _)        = FeedsView
  f (Right (is, b)) = ItemsView is b

isFeedsView :: Model -> Bool
isFeedsView (Model _ _ FeedsView) = True
isFeedsView _                     = False

{-
instance Show Model where
  show (Model fs i v) = unlines
    [ "prev:  " ++ T.unpack (fs^.prev.traversed.feedTitle)
    , "curr:  " ++ fs^.curr.feedTitle.to T.unpack
    , "next:  " ++ fs^.next.traversed.feedTitle.to T.unpack
    , "above: " ++ show (i^.above)
    , "pos:   " ++ show (i^.position)
    , "below: " ++ show (i^.below)
    ]
-}

------------------------------------------------------------------------

makeModel :: [AnnFeed] -> Integer -> Model
makeModel []       _    =
  Model (Zip [] (defaultAnn $ newEmptyFeed AtomKind) []) emptyInfo FeedsView
makeModel (f : fs) rows =
  Model (Zip [] f                       fs) i         FeedsView
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

initialModel :: Integer -> Model
initialModel = makeModel $ map defaultAnn (slashdotFeed : dummyFeeds 100)

slashdotFeed, undeadlyFeed :: Feed' [Item]
slashdotFeed
  = newEmptyFeed AtomKind
  & feedTitle .~ "/."
  & feedHome  .~ "http://rss.slashdot.org/Slashdot/slashdot"

undeadlyFeed
  = newEmptyFeed AtomKind
  & feedTitle .~ "Undeadly"
  & feedHome  .~ "http://undeadly.org/cgi?action=rss"

------------------------------------------------------------------------

move :: Dir -> (Model -> Model)
move = execState . move'

-- XXX: remove the bool?
move' :: Dir -> State Model Bool
move' dir = do
  Model fs i v <- get
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
        put $ Model fs i $ ItemsView (makeZip (fs^.curr.feedItems)) False
        return False

    ItemsView is si -> case dir of
      Out -> if si
                then do
                  put $ Model fs i $ ItemsView is False
                  return False
                else do
                  put $ Model (fs & curr.feedItems .~ closeZip is) i FeedsView
                  return False

      In -> do
        put $ Model fs i $ ItemsView (is & curr.isRead .~ True) (not si)
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
