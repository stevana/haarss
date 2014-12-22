{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module Feed.Annotated where

import Control.Lens
import Data.List (find)
import qualified Data.Text as T
import Data.Serialize
import GHC.Generics (Generic)

import Feed.Feed
import Fetching.History

------------------------------------------------------------------------

data AnnItem = AnnItem
  { _item   :: Item
  , _isRead :: Bool
  }
  deriving Generic

makeLenses ''AnnItem

instance Show AnnItem where
  show i = i^.item.itemTitle.to (maybe "(no item title)" T.unpack)

data AnnFeed = AnnFeed
  { _feed    :: Feed' [AnnItem]
  , _history :: [History]
  }
  deriving Generic

makeLenses ''AnnFeed

instance Show AnnFeed where
  show f = f^.feed.feedTitle.to (maybe "(no title)" T.unpack)

mergeFeeds :: [AnnFeed] -> [AnnFeed] -> [AnnFeed]
mergeFeeds = zipWith merge

merge :: AnnFeed -> AnnFeed -> AnnFeed
merge old new = new & feed.feedItems .~ mergeItems (old^.feed.feedItems)
                                                   (new^.feed.feedItems)
                    & history %~ (++ old^.history)

-- XXX: O(new^old)...
mergeItems :: [AnnItem] -> [AnnItem] -> [AnnItem]
mergeItems old new = map (\n -> keepOldAnn (n^.item) old) new
  where
  keepOldAnn :: Item -> [AnnItem] -> AnnItem
  keepOldAnn n old' = case find (\o -> n == o^.item) old' of
    Nothing -> AnnItem n False
    Just o  -> AnnItem n (o^.isRead)

defaultAnn :: Feed -> AnnFeed
defaultAnn f = AnnFeed (f & feedItems.traverse %~ flip AnnItem False) []

------------------------------------------------------------------------

instance Serialize AnnFeed  where
instance Serialize AnnItem  where
