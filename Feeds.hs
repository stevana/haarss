{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, TemplateHaskell,
    OverloadedStrings, DeriveGeneric #-}

module Feeds where

import Test.QuickCheck
import Data.List
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)
import Data.Serialize
import Data.Text.Encoding

import qualified Text.Feed.Types as Feed
import qualified Text.Feed.Query as Feed

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
  deriving (Eq, Show, Read, Functor, Generic)

data FeedKind = AtomKind | RSS1Kind | RSS2Kind
  deriving (Eq, Show, Read, Enum, Generic)

makeLenses ''Feed'

data Item = Item
  { _itemTitle       :: Text
  , _itemLink        :: URL
  , _itemDate        :: Date
  , _itemFeedLink    :: URL
  , _itemDescription :: Text
  }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''Item

type Feed = Feed' [Item]

newEmptyFeed :: FeedKind -> Feed
newEmptyFeed kind = Feed kind "" "" "" "" "" []

newEmptyItem :: Item
newEmptyItem = Item "" "" "" "" ""

addItem :: Item -> Feed -> Feed
addItem item feed = feed & feedItems %~ cons item

------------------------------------------------------------------------

{-
instance Comonad Feed' where
  extract feed  = feed^.feedItems
  extend k feed = feed & feedItems .~ k feed
-}

instance Arbitrary FeedKind where
  arbitrary = Test.QuickCheck.elements [AtomKind, RSS1Kind, RSS2Kind]

-- instance Arbitrary Text where
--   arbitrary = pure $ T.pack "apa"
--
-- instance Arbitrary is => Arbitrary (Feed' is) where
--   arbitrary = Feed <$> arbitrary <*> arbitrary <*> arbitrary
--                    <*> arbitrary <*> arbitrary <*> arbitrary
--                    <*> arbitrary

-- XXX: Checking this gives some linking error...
-- prop_comonad1 :: Feed' [Int] -> Bool
-- prop_comonad1 feed = extend extract feed == feed

------------------------------------------------------------------------

data AnnItem = AnnItem
  { _isRead :: Bool
  , _item   :: Item
  }
  deriving (Show, Read, Generic)

makeLenses ''AnnItem

type AnnFeed = Feed' [AnnItem]

mergeFeeds :: [AnnFeed] -> [AnnFeed] -> [AnnFeed]
mergeFeeds = zipWith merge

merge :: AnnFeed -> AnnFeed -> AnnFeed
merge old new = new & feedItems .~ mergeItems (old^.feedItems)
                                              (new^.feedItems)

-- XXX: O(new^old)...
mergeItems :: [AnnItem] -> [AnnItem] -> [AnnItem]
mergeItems old new = map (\n -> keepOldAnn (n^.item) old) new
  where
  keepOldAnn :: Item -> [AnnItem] -> AnnItem
  keepOldAnn n old' = case find (\o -> n == o^.item) old' of
    Nothing -> AnnItem False       n
    Just o  -> AnnItem (o^.isRead) n

------------------------------------------------------------------------

-- XXX: This is just hack while we find/write a better feed library...

convert :: Feed.Feed -> Feed
convert feed = newEmptyFeed (kind feed)
  & feedTitle       .~ T.pack (Feed.getFeedTitle feed)
  & feedHome        .~ T.pack (maybe "" id $ Feed.getFeedHome feed)
  & feedHTML        .~ T.pack (maybe "" id $ Feed.getFeedHTML feed)
  & feedDescription .~ T.pack (maybe "" id $ Feed.getFeedDescription feed)
  & feedLastUpdate  .~ T.pack (maybe "" id $ Feed.getFeedLastUpdate feed)
  & feedItems       .~ map convertItems (Feed.feedItems feed)
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
defaultAnn feed = feed & feedItems.traverse %~ AnnItem False

------------------------------------------------------------------------

instance Serialize FeedKind where
instance Serialize Item where
instance Serialize AnnItem where
instance Serialize a => Serialize (Feed' a) where

instance Serialize Text where
  put txt = put $ encodeUtf8 txt
  get     = fmap decodeUtf8 get
