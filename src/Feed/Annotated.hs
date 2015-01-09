{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Feed.Annotated where

import           Control.Applicative
import           Control.Lens
import           Data.List           (find)
import           Data.Maybe
import           Data.Monoid
import           Data.Serialize
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time
import           GHC.Generics        (Generic)
import           System.Locale
import           Test.QuickCheck     hiding (Failure, Success)

import           Feed.Feed
import           Fetching.History

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

data AnnFeed = AnnFeed
  { _feed    :: Feed' [AnnItem]
  , _alias   :: Maybe Text
  , _history :: [History]
  }
  deriving (Eq, Generic)

makeLenses ''AnnFeed

newEmptyAnnFeed :: AnnFeed
newEmptyAnnFeed = AnnFeed
  { _feed    = newEmptyFeed AtomKind & feedItems.traverse %~ defAnnItem
  , _alias   = Nothing
  , _history = []
  }

defAnnFeed :: Feed -> AnnFeed
defAnnFeed f = newEmptyAnnFeed
  & feed .~ (f & feedItems.traverse %~ defAnnItem)

instance Show AnnFeed where
  show f = f^.feed.feedTitle.to (maybe "(no title)" T.unpack)

-- XXX: Needs to be more flexible in the future; can't assume that the
-- old and new feeds will be as many and positioned the same...
mergeFeeds :: [AnnFeed] -> [AnnFeed] -> [AnnFeed]
mergeFeeds = zipWith merge

merge :: AnnFeed -> AnnFeed -> AnnFeed
merge old new
  | new^.history^?_head._Failure & isJust
  = old & history %~ \h -> prune (new^.history ++ h)

  | otherwise
  = new & feed.feedItems .~ mergeItems (old^.feed.feedItems)
                                       (new^.feed.feedItems)
        & alias          .~ old^.alias
        & history        %~ \h -> prune (h ++ old^.history)
  where
  prune = take 10

-- XXX: O(new^old)...
mergeItems :: [AnnItem] -> [AnnItem] -> [AnnItem]
mergeItems old new = map (\n -> keepOldAnn (n^.item)) new
  where
  keepOldAnn :: Item -> AnnItem
  keepOldAnn n =
    -- XXX: Compare only (hashes of) titles and descriptions?
    case find (\o -> n == o^.item) old of
      Nothing -> AnnItem n False
      Just o  -> AnnItem n (o^.isRead)

-- | The function for merging items is idempotent.
prop_mergeItemsIdempotent :: [AnnItem] -> Bool
prop_mergeItemsIdempotent is = mergeItems is is == is

prop_isReadKept :: [AnnItem] -> Bool
prop_isReadKept is = andOf (traverse.isRead) $
  mergeItems (is & mapped.isRead .~ True) is

------------------------------------------------------------------------

addOverviewFeed :: UTCTime -> [AnnFeed] -> [AnnFeed]
addOverviewFeed time fs = overview : fs
  where
  rfc822Time = formatTime defaultTimeLocale rfc822DateFormat time

  overview :: AnnFeed
  overview = newEmptyAnnFeed
    & feed    .~ (newEmptyFeed AtomKind
      & feedTitle       ?~ "(New headlines)"
      & feedDescription ?~ "(New headlines)"
      & feedLastUpdate  ?~ T.pack rfc822Time
      & feedItems       .~ is)
    & history .~ [Success time]
    where
    is :: [AnnItem]
    is = is' & mapped %~ \(i, mt) -> i & item.itemTitle %~ \t ->
      mconcat [Just "(", mt, Just ") ", t]
      where
      is' :: [(AnnItem, Maybe Text)]
      is' = fs & concatMapOf folded (\f -> zip
        (f^.feed.feedItems^..folded.filtered (not . _isRead))
        (repeat (f^.feed.feedTitle)))

-- | The overview feed should have as many items as there are unread
-- items in the feeds from which the overview was created.
prop_overviewLength :: UTCTime -> [AnnFeed] -> Bool
prop_overviewLength time fs =
  overview^.feed.feedItems.to length == sumOf folded fs'
  where
  fs' :: [Int]
  fs' = fs^..folded.feed.feedItems & mapped %~ length . filter _isRead

  overview :: AnnFeed
  overview = head $ addOverviewFeed time fs

------------------------------------------------------------------------

instance Serialize AnnFeed  where
instance Serialize AnnItem  where

instance Arbitrary AnnFeed where
  arbitrary = AnnFeed <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AnnItem where
  arbitrary = AnnItem <$> arbitrary <*> arbitrary
