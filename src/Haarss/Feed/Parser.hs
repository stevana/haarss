{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Haarss.Feed.Parser (parseFeed) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception       (SomeException)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Text.Lazy.Encoding (decodeLatin1, decodeUtf8')
import           Data.Text.Lens          (unpacked)
import           Text.XML                (def, parseText)
import           Text.XML.Lens

import           Haarss.Feed.Feed

-- XXX: Debug
-- import qualified Data.ByteString.Lazy as BS

------------------------------------------------------------------------

parseFeed :: ByteString -> Either SomeException Feed
parseFeed bs = do
  let t = either (const (decodeLatin1 bs)) id (decodeUtf8' bs)
  doc <- parseText def t
  deepseq doc $ fromXML doc

fromXML :: Document -> Either SomeException Feed
fromXML doc = case doc^.root.localName of
  "RDF"  -> return $ fromRSS1 doc
  "rss"  -> return $ fromRSS2 doc
  "feed" -> return $ fromAtom doc
  _      -> fail   $ error "fromXML: unknown feed kind."

process :: Text -> Text
process = (\ts -> if length ts > 1
                     then ts^._head.to T.strip.to (`T.append` " [...]")
                     else ts^._head.to T.strip)
        . filter (not . T.null)
        . T.lines

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
    & itemTitle       .~ (e^?entire.ell "title".text & mapped %~ process)
    & itemLink        .~ e^?entire.ell "link".text.to T.strip.unpacked
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
    & itemTitle       .~ (e^?entire.el "title".text & mapped %~ process)
    & itemLink        .~ e^?entire.el "link".text.to T.strip.unpacked
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
    & itemTitle       .~ (e^?entire.ell "title".text & mapped %~ process)
    & itemLink        .~ e^?entire.ell "link".attr "href".to T.strip.unpacked
    & itemDate        .~ (e^?entire.ell "published".text <|>
                          e^?entire.ell "updated".text)
    & itemFeedLink    .~ Nothing
    & itemDescription .~ e^?entire.ell "summary".text <>
                         e^?entire.ell "content".text.to removeHtml
    where
    removeHtml :: Text -> Text
    removeHtml = go False . T.strip
      where
      go :: Bool -> Text -> Text
      go False (T.uncons -> Nothing)       = T.empty
      go False (T.uncons -> Just ('<', t)) = go True  t
      go False (T.uncons -> Just (c, t))   = c `T.cons` go False t
      go True  (T.uncons -> Nothing)       = T.empty
      go True  (T.uncons -> Just ('>', t)) = go False  t
      go True  (T.uncons -> Just (_, t))   = go True t
      go _     (T.uncons -> _)             = error "Impossible"


------------------------------------------------------------------------
-- XXX: Debugging

{-
main = do
  bs <- BS.readFile "/tmp/master.atom"
  case parseFeed bs of
    Left  e -> putStrLn $ show e
    Right f -> putStrLn $ ppFeed f


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
  , i^.itemDescription.to show
  ]

ppList :: (a -> String) -> [a] -> String
ppList pp xs = "[" ++ go xs
  where
  go []        = "]"
  go (x : xs') = pp x ++ ",\n" ++ go xs'
-}
