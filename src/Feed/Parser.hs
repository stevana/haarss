{-# LANGUAGE OverloadedStrings #-}

module Feed.Parser (parseFeed) where

import Control.Exception (SomeException)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Text.XML (parseText, def)
import Text.XML.Lens

import Feed.Feed

------------------------------------------------------------------------

parseFeed :: ByteString -> Either SomeException Feed
parseFeed bs = do
  doc <- parseText def (decodeUtf8With lenientDecode bs)
  fromXML doc

fromXML :: Document -> Either SomeException Feed
fromXML doc = case doc^.root.localName of
  "RDF"  -> return $ fromRSS1 doc
  "rss"  -> return $ fromRSS2 doc
  "feed" -> return $ fromAtom doc
  _      -> fail   $ error "fromXML: unknown feed kind."

fromRSS1 :: Document -> Feed
fromRSS1 doc = newEmptyFeed RSS1Kind
  & feedTitle       .~ doc^?root./ell "channel"./ell "title".text
  & feedHome        .~ ""
  & feedHTML        .~ ""
  & feedDescription .~ doc^?root.ell "channel"./ell "description".text
  & feedLastUpdate  .~ doc^?root.ell "channel"./ell "date".text
  & feedItems       .~ doc^..root./ell "item" & mapped.mapped %~ toItem
  where
  toItem :: Element -> Item
  toItem e = newEmptyItem
    & itemTitle       .~ e^?entire.ell "title".text
    & itemLink        .~ e^?entire.ell "link".text
    & itemDate        .~ Nothing
    & itemFeedLink    .~ Nothing
    & itemDescription .~ e^?entire.ell "description".text

fromRSS2 :: Document -> Feed
fromRSS2 doc = newEmptyFeed RSS2Kind
  & feedTitle       .~ doc^?root./el "channel"./el "title".text
  & feedHome        .~ ""
  & feedHTML        .~ ""
  & feedDescription .~ doc^?root.el "channel"./el "description".text
  & feedLastUpdate  .~ doc^?root.el "channel"./el "date".text
  & feedItems       .~ doc^..root./el "channel"./el "item"
                         & mapped.mapped %~ toItem
  where
  toItem :: Element -> Item
  toItem e = newEmptyItem
    & itemTitle       .~ e^?entire.el "title".text
    & itemLink        .~ e^?entire.el "link".text
    & itemDate        .~ e^?entire.el "pubDate".text
    & itemFeedLink    .~ Nothing
    & itemDescription .~ e^?entire.el "description".text

fromAtom :: Document -> Feed
fromAtom doc = newEmptyFeed AtomKind
  & feedTitle       .~ doc^?root./ell "title".text
  & feedHome        .~ ""
  & feedHTML        .~ ""
  & feedDescription .~ Nothing
  & feedLastUpdate  .~ doc^?root.ell "updated".text
  & feedItems       .~ doc^..root./ell "entry" & mapped.mapped %~ toItem
  where
  toItem :: Element -> Item
  toItem e = newEmptyItem
    & itemTitle       .~ e^?entire.ell "title".text
    & itemLink        .~ e^?entire.ell "link".attr "href"
    & itemDate        .~ e^?entire.ell "published".text
    & itemFeedLink    .~ Nothing
    & itemDescription .~ e^?entire.ell "summary".text

------------------------------------------------------------------------
-- XXX: Debugging

ppFeed :: Feed -> String
ppFeed f = unlines
  [ f^.feedTitle.to show
  , f^.feedDescription.to show
  , f^.feedLastUpdate.to show
  , f^.feedItems & over each ppItem & ppList id
  ]

ppItem :: Item -> String
ppItem i = unlines
  [ i^.itemTitle.to show
  , i^.itemLink.to show
  , i^.itemDate.to show
  , i^.itemDescription.to (take 20 . show)
  ]

ppList :: (a -> String) -> [a] -> String
ppList pp xs = "[" ++ go xs
  where
  go []       = "]"
  go (x : xs) = pp x ++ ",\n" ++ go xs