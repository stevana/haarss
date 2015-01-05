{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feed.Feed where

import           Control.Applicative
import           Control.Lens
import           Data.Foldable       (Foldable)
import           Data.Maybe
import           Data.Serialize
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding  (decodeUtf8, encodeUtf8)
import           GHC.Generics        (Generic)
import           Test.QuickCheck

------------------------------------------------------------------------

type URL  = String
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
be x = to (fromMaybe x)

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

deriving instance Eq a => Eq (Feed' a)
deriving instance Eq FeedKind

instance Serialize FeedKind where
instance Serialize Item     where
instance Serialize a => Serialize (Feed' a) where

instance Serialize Text where
  put txt = put $ encodeUtf8 txt
  get     = fmap decodeUtf8 get

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary FeedKind where
  arbitrary = Test.QuickCheck.elements (enumFrom AtomKind)

instance Arbitrary a => Arbitrary (Feed' a) where
  arbitrary = Feed <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Item where
  arbitrary = Item <$> arbitrary <*> arbitrary <*> arbitrary <*>
                       arbitrary <*> arbitrary
