{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, TemplateHaskell,
    OverloadedStrings, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feeds where

import Data.Char (isSpace)
import Data.List
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)
import Data.Serialize
import Data.Text.Encoding

import qualified Text.Feed.Types as Feed
import qualified Text.Feed.Query as Feed

import Fetching.History

------------------------------------------------------------------------
-- Plan

{-

* The following can wait (feed is a bit shit, but not a problem).

    + bytestring to XML via attoparsec?
       - xml-conduit (uses atto, lens interface provided via xml-lens)
       - taggy + taggy-lens? Isn't as used as xml-conduit...

    + XML to feed-like interface (but using Text and lenses)?

         parseFeed :: Response ByteString -> Either Error Feed
         parseFeed = responseBody . to (decodeUtf8With lenientDecode)
                   . undefined

         main :: IO ()
         main = get url >>= print `fmap` parseFeed

    + Encoding... (don't bother with this until we do our own parsing?!)

    + 'R', 'R' results in empty feed... Problem is that parser doesn't
      set feedHome correctly! Fix parser? (using config meanwhile)

-}

------------------------------------------------------------------------

type URL  = Text
type Date = Text

data Feed' is = Feed
  { _feedKind        :: FeedKind
  , _feedTitle       :: Text
  , _feedHome        :: URL
  , _feedHTML        :: URL
  , _feedDescription :: Text
  , _feedLastUpdate  :: Date
  , _feedItems       :: is
  }
  deriving (Eq, Functor, Generic)

data FeedKind = AtomKind | RSS1Kind | RSS2Kind
  deriving (Eq, Enum, Generic)

makeLenses ''Feed'

instance Show (Feed' is) where
  show f = f^.feedTitle.to T.unpack

data Item = Item
  { _itemTitle       :: Text
  , _itemLink        :: URL
  , _itemDate        :: Date
  , _itemFeedLink    :: URL
  , _itemDescription :: Text
  }
  deriving (Eq, Ord, Generic)

makeLenses ''Item

type Feed = Feed' [Item]

newEmptyFeed :: FeedKind -> Feed
newEmptyFeed kind = Feed kind "" "" "" "" "" []

newEmptyItem :: Item
newEmptyItem = Item "" "" "" "" ""

addItem :: Item -> Feed -> Feed
addItem item feed = feed & feedItems %~ cons item

------------------------------------------------------------------------

data AnnItem = AnnItem
  { _item   :: Item
  , _isRead :: Bool
  }
  deriving Generic

makeLenses ''AnnItem

instance Show AnnItem where
  show i = i^.item.itemTitle.to T.unpack

data AnnFeed = AnnFeed
  { _feed    :: Feed' [AnnItem]
  , _history :: [History]
  }
  deriving Generic

makeLenses ''AnnFeed

instance Show AnnFeed where
  show f = f^.feed.feedTitle.to T.unpack

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

------------------------------------------------------------------------

-- XXX: This is just hack while we find/write a better feed library...

whenEmpty :: String -> String -> String
whenEmpty s e | all isSpace s = e
              | otherwise     = s

convert :: Feed.Feed -> Feed
convert f = newEmptyFeed (kind f)
  & feedTitle       .~
                       T.pack (whenEmpty (Feed.getFeedTitle f)
                                         "(no title)")
  & feedHome        .~ T.pack (maybe "" id $ Feed.getFeedHome f)
  & feedHTML        .~ T.pack (maybe "" id $ Feed.getFeedHTML f)
  & feedDescription .~ T.pack (maybe "" id $ Feed.getFeedDescription f)
  & feedLastUpdate  .~ T.pack (maybe "" id $ Feed.getFeedLastUpdate f)
  & feedItems       .~ map convertItems (Feed.feedItems f)
  where
  kind :: Feed.Feed -> FeedKind
  kind (Feed.AtomFeed _)  = AtomKind
  kind (Feed.RSS1Feed _)  = RSS1Kind
  kind (Feed.RSSFeed _)   = RSS2Kind
  kind (Feed.XMLFeed _)   = AtomKind -- ?

convertItems :: Feed.Item -> Item
convertItems i = newEmptyItem
  & itemTitle       .~ T.pack (maybe "" id $ Feed.getItemTitle i)
  & itemLink        .~ T.pack (maybe "" id $ Feed.getItemLink i)
  & itemDate        .~ T.pack (maybe "" id $ Feed.getItemDate i)
  & itemFeedLink    .~ T.pack (maybe "" id $ Feed.getItemFeedLink i)
  & itemDescription .~ T.pack (maybe "" id $ Feed.getItemDescription i)

defaultAnn :: Feed -> AnnFeed
defaultAnn f = AnnFeed (f & feedItems.traverse %~ flip AnnItem False) []

------------------------------------------------------------------------

instance Serialize FeedKind where
instance Serialize Item     where
instance Serialize AnnFeed  where
instance Serialize AnnItem  where
instance Serialize a => Serialize (Feed' a) where

instance Serialize Text where
  put txt = put $ encodeUtf8 txt
  get     = fmap decodeUtf8 get
