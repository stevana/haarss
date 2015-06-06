{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -F -pgmF htfpp  #-}

module Haarss.Feed.Annotated where

import           Control.Applicative
import           Control.Monad
import           Control.Lens
import           Data.Hashable
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as M
import           Data.List           (nub, sort, (\\))
import           Data.Maybe
import           Data.Serialize
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time
import           GHC.Generics        (Generic)
import           Test.QuickCheck     hiding (Failure, Success)
import           Test.Framework      hiding (Failure, Success, elements)

import           Haarss.Feed.Feed
import           Haarss.Fetching.History

------------------------------------------------------------------------

data AnnItem = AnnItem
  { _item   :: Item
  , _isRead :: Bool
  }
  deriving (Eq, Generic)

makeLenses ''AnnItem

defAnnItem :: Item -> AnnItem
defAnnItem i = AnnItem i False

instance Show AnnItem where
  show i = i^.item.itemTitle.to (maybe "(no item title)" T.unpack)

data Ignore = Title | Description | Date
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data AnnFeed = AnnFeed
  { _feed    :: Feed' [AnnItem]
  , _alias   :: Maybe Text
  , _history :: [History]
  , _ignore  :: [Ignore]
  }
  deriving (Eq, Generic)

makeLenses ''AnnFeed

newEmptyAnnFeed :: AnnFeed
newEmptyAnnFeed = AnnFeed
  { _feed    = newEmptyFeed AtomKind & feedItems.traverse %~ defAnnItem
  , _alias   = Nothing
  , _history = []
  , _ignore  = []
  }

defAnnFeed :: Feed -> AnnFeed
defAnnFeed f = newEmptyAnnFeed
  & feed .~ (f & feedItems.traverse %~ defAnnItem)

instance Show AnnFeed where
  show f = f^.feed.feedTitle.to (maybe "(no title)" T.unpack)

-- (Multiples of same feed will cause problems.)
mergeFeeds :: [AnnFeed] -> [AnnFeed] -> [AnnFeed]
mergeFeeds old new = flip map new $ \n ->
  case M.lookup (hash (n^.feed.feedHome)) m of
    Nothing -> n
    Just o  -> mergeFeed o n
  where
  m :: IntMap AnnFeed
  m = M.fromList $ map (\o -> (hash (o^.feed.feedHome), o)) old

mergeFeed :: AnnFeed -> AnnFeed -> AnnFeed
mergeFeed old new
  | new^.history^?_head._Failure & isJust
  = old & history %~ \h -> prune (new^.history ++ h)

  | otherwise
  = new & feed.feedItems .~ mergeItems (old^.ignore)
                                       (old^.feed.feedItems)
                                       (new^.feed.feedItems)
        & alias          .~ old^.alias
        & history        %~ (\h -> prune (h ++ old^.history))
        & ignore         .~ old^.ignore
  where
  prune = take 10

mergeItems :: [Ignore] -> [AnnItem] -> [AnnItem] -> [AnnItem]
mergeItems is old new = flip map new $ \n ->
  case M.lookup (hashItem n) m of
    Nothing -> n
    Just o  -> n & isRead .~ o^.isRead
  where
  hashItem :: AnnItem -> Int
  hashItem i
    = foldr1 hashWithSalt
    . map hashIgnore
    . (\is' -> if null is' then is else is') -- We can't ignore everything.
    $ sort (enumFrom minBound) \\ sort (nub is)
    where
    hashIgnore :: Ignore -> Int
    hashIgnore Title       = hash $ i^.item.itemTitle
    hashIgnore Description = hash $ i^.item.itemDescription
    hashIgnore Date        = hash $ i^.item.itemDate

  m :: IntMap AnnItem
  m = M.fromList $ map (\o -> (hashItem o, o)) old

-- | The function for merging items is idempotent.
prop_mergeItemsIdempotent :: [AnnItem] -> Bool
prop_mergeItemsIdempotent is = mergeItems [] is is == is

prop_isReadKept :: [AnnItem] -> Bool
prop_isReadKept is = andOf (traverse.isRead) $
  mergeItems [] (is & mapped.isRead .~ True) is

------------------------------------------------------------------------

makeOverview :: UTCTime -> [AnnFeed] -> AnnFeed
makeOverview time fs = newEmptyAnnFeed
    & feed    .~ (newEmptyFeed AtomKind
      & feedTitle       ?~ "(New headlines)"
      & feedDescription ?~ "(New headlines)"
      & feedLastUpdate  ?~ T.pack rfc822Time
      & feedItems       .~ is)
    & history .~ [Success time]
    where
    rfc822Time = formatTime defaultTimeLocale rfc822DateFormat time

    is :: [AnnItem]
    is = is' & mapped %~ \(i, mt) -> i & item.itemTitle %~ \t ->
      mconcat [Just "(", mt, Just ") ", t]
      where
      is' :: [(AnnItem, Maybe Text)]
      is' = fs & concatMapOf folded (\f -> zip
        (f^.feed.feedItems^..folded.filtered (not . _isRead))
        (repeat (f^.alias <|> f^.feed.feedTitle)))

addOverviewFeed :: UTCTime -> [AnnFeed] -> [AnnFeed]
addOverviewFeed time fs = makeOverview time fs : fs

-- | The overview feed should have as many items as there are unread
-- items in the feeds from which the overview was created.
prop_overviewLength :: UTCTime -> [AnnFeed] -> Bool
prop_overviewLength time fs =
  overview^.feed.feedItems.to length == sumOf folded fs'
  where
  fs' :: [Int]
  fs' = fs^..folded.feed.feedItems & mapped %~ length . filter (not._isRead)

  overview :: AnnFeed
  overview = head $ addOverviewFeed time fs

------------------------------------------------------------------------

instance Serialize Ignore   where
instance Serialize AnnFeed  where
instance Serialize AnnItem  where

instance Arbitrary Ignore where
  arbitrary = Test.QuickCheck.elements (enumFrom minBound :: [Ignore])

instance Arbitrary AnnFeed where
  arbitrary = liftM4 AnnFeed arbitrary arbitrary arbitrary arbitrary

instance Arbitrary AnnItem where
  arbitrary = AnnItem <$> arbitrary <*> arbitrary
