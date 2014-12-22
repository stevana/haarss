{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable,
    TypeSynonymInstances, TemplateHaskell, OverloadedStrings,
    DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feed.Feed where

import Data.Foldable (Foldable)
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)
import Data.Serialize
import Data.Text.Encoding

------------------------------------------------------------------------

type URL  = Text
type Date = Text

data Feed' is = Feed
  { _feedKind        :: FeedKind
  , _feedTitle       :: Maybe Text
  , _feedHome        :: URL
  , _feedHTML        :: URL
  , _feedDescription :: Maybe Text
  , _feedLastUpdate  :: Maybe Date
  , _feedItems       :: is
  }
  deriving (Functor, Foldable, Traversable, Generic)

data FeedKind = AtomKind | RSS1Kind | RSS2Kind
  deriving (Enum, Generic)

makeLenses ''Feed'

be :: (Contravariant f, Profunctor p) =>
       a -> Optical' p p f (Maybe a) a
be x = to (maybe x id)

instance Show (Feed' is) where
  show f = f^.feedTitle.to (maybe "(no title)" T.unpack)

data Item = Item
  { _itemTitle       :: Maybe Text
  , _itemLink        :: Maybe URL
  , _itemDate        :: Maybe Date
  , _itemFeedLink    :: Maybe URL
  , _itemDescription :: Maybe Text
  }
  deriving (Eq, Ord, Generic)

makeLenses ''Item

type Feed = Feed' [Item]

newEmptyFeed :: FeedKind -> Feed
newEmptyFeed kind = Feed kind Nothing "" "" Nothing Nothing []

newEmptyItem :: Item
newEmptyItem = Item Nothing Nothing Nothing Nothing Nothing

addItem :: Item -> Feed -> Feed
addItem item feed = feed & feedItems %~ cons item

------------------------------------------------------------------------

instance Serialize FeedKind where
instance Serialize Item     where
instance Serialize a => Serialize (Feed' a) where

instance Serialize Text where
  put txt = put $ encodeUtf8 txt
  get     = fmap decodeUtf8 get