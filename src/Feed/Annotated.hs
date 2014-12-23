{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric,
             StandaloneDeriving #-}

module Feed.Annotated where

import Control.Applicative
import Control.Lens
import Data.List (find)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Serialize
import GHC.Generics (Generic)
import Test.QuickCheck

import Feed.Feed
import Fetching.History

------------------------------------------------------------------------

data AnnItem = AnnItem
  { _item   :: Item
  , _isRead :: Bool
  }
  deriving Generic

makeLenses ''AnnItem

defAnnItem :: Item -> AnnItem
defAnnItem i = AnnItem i False

instance Show AnnItem where
  show i = i^.item.itemTitle.to (maybe "(no item title)" T.unpack)

data AnnFeed = AnnFeed
  { _feed    :: Feed' [AnnItem]
  , _history :: [History]
  }
  deriving Generic

makeLenses ''AnnFeed

defAnnFeed :: Feed -> AnnFeed
defAnnFeed f = AnnFeed (f & feedItems.traverse %~ defAnnItem) []

instance Show AnnFeed where
  show f = f^.feed.feedTitle.to (maybe "(no title)" T.unpack)

-- XXX: Needs to be more flexible in the future; can't assume that the
-- old and new feeds will be as many and positioned the same...
mergeFeeds :: [AnnFeed] -> [AnnFeed] -> [AnnFeed]
mergeFeeds = zipWith merge

merge :: AnnFeed -> AnnFeed -> AnnFeed
merge old new = new
  & feed.feedItems .~ mergeItems (old^.feed.feedItems)
                                 (new^.feed.feedItems)
  & history %~ \[h] -> take 10 (h : old^.history)

-- XXX: O(new^old)...
mergeItems :: [AnnItem] -> [AnnItem] -> [AnnItem]
mergeItems old new = map (\n -> keepOldAnn (n^.item)) new
  where
  keepOldAnn :: Item -> AnnItem
  keepOldAnn n =
    case find (\o -> n^.itemTitle == o^.item.itemTitle) old of
      Nothing -> AnnItem n False
      Just o  -> AnnItem n (o^.isRead)

------------------------------------------------------------------------

addOverviewFeed :: Text -> [AnnFeed] -> [AnnFeed]
addOverviewFeed time fs = overview : fs
  where
  overview :: AnnFeed
  overview = AnnFeed
    { _feed    = newEmptyFeed AtomKind
      & feedTitle       ?~ "(New headlines)"
      & feedDescription ?~ "(New headlines)"
      & feedLastUpdate  ?~ time
      & feedItems       .~ is
    , _history = []
    }
    where
    is :: [AnnItem]
    is = is' & mapped %~ \(i, mt) -> i & item.itemTitle %~ \t ->
      mconcat [Just "(", mt, Just ") ", t]
      where
      is' :: [(AnnItem, Maybe Text)]
      is' = fs & concatMapOf folded (\f -> zip
        (f^.feed.feedItems) (cycle [f^.feed.feedTitle]))

------------------------------------------------------------------------

deriving instance Eq AnnFeed
deriving instance Eq AnnItem

instance Serialize AnnFeed  where
instance Serialize AnnItem  where

instance Arbitrary AnnFeed where
  arbitrary = AnnFeed <$> arbitrary <*> arbitrary

instance Arbitrary AnnItem where
  arbitrary = AnnItem <$> arbitrary <*> arbitrary
