{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Haarss.Feed.Parser (parseFeed) where

import           Control.DeepSeq
import           Control.Exception       (SomeException)
import           Control.Lens            hiding (element)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Text.XML
import           Text.XML.Cursor

import           Haarss.Feed.Feed

-- XXX: Debug
-- import qualified Data.ByteString.Lazy as BS

------------------------------------------------------------------------

parseFeed :: ByteString -> Either SomeException Feed
parseFeed bs = do
  doc <- parseLBS (def { psDecodeEntities = decodeHtmlEntities }) bs
  deepseq doc $ fromXML (fromDocument doc)
                        (nameLocalName (elementName (documentRoot doc)))

fromXML :: Cursor -> Text -> Either SomeException Feed
fromXML c "RDF"  = return $ fromRSS RSS1Kind c itemsRSS1
fromXML c "rss"  = return $ fromRSS RSS2Kind c itemsRSS2
fromXML c "feed" = return $ fromAtom c
fromXML _ _      = fail   $ error "fromXML: unknown feed kind."

process :: Text -> Text
process = T.strip
        . (\ts -> if length ts > 1
                     then ts^._head.to (`T.append` " [...]")
                     else ts^._head)
        . filter (not . T.null)
        . T.lines

concatMaybe :: Monoid m => [m] -> Maybe m
concatMaybe ts = case ts of
  []  -> Nothing
  _:_ -> Just (mconcat ts)

fromRSS :: FeedKind -> Cursor -> (Cursor -> [Item]) -> Feed
fromRSS k c is = Feed
  { _feedKind        = k
  , _feedTitle       = concatMaybe $ c $//
                         element "channel" &/ element "title" &// content
  , _feedHome        = ""
  , _feedHTML        = ""
  , _feedDescription = concatMaybe $ c $//
                         element "channel" &/
                         element "description" &// content
  , _feedLastUpdate  = concatMaybe $ c $//
                         element "channel" &/ element "date" &// content
  , _feedItems       = is c
  }

itemsRSS1 :: Cursor -> [Item]
itemsRSS1 c = c $// element "item" >=> toItem
  where
  toItem :: Cursor -> [Item]
  toItem c' = return $ Item
    { _itemTitle       = concatMaybe $ c' $// element "title" &// content
    , _itemLink        = concatMaybe $ c' $// element "link"
                                          &// map T.unpack . content
    , _itemDate        = Nothing
    , _itemFeedLink    = Nothing
    , _itemDescription = concatMaybe $ c' $//
                           element "description" &// content
    }

itemsRSS2 :: Cursor -> [Item]
itemsRSS2 c = c $// element "channel" &/ element "item" >=> toItem
  where
  toItem :: Cursor -> [Item]
  toItem c' = return $ Item
    { _itemTitle       = concatMaybe $ c' $// element "title" &// content
    , _itemLink        = concatMaybe $ c' $// element "link"
                                          &// map T.unpack . content
    , _itemDate        = concatMaybe $ c' $// element "pubDate"
                                          &// content
    , _itemFeedLink    = Nothing
    , _itemDescription = concatMaybe $ c' $//
                           element "description" &// content
    }

fromAtom :: Cursor -> Feed
fromAtom c = Feed
  { _feedKind        = AtomKind
  , _feedTitle       = concatMaybe $ c $// element "title" &// content
  , _feedHome        = ""
  , _feedHTML        = ""
  , _feedDescription = Nothing
  , _feedLastUpdate  = concatMaybe $ c $// element "updated" &// content
  , _feedItems       = c $// element "entry" >=> toItem
  }
  where
  toItem :: Cursor -> [Item]
  toItem c' = return $ Item
    { _itemTitle       = concatMaybe $ c' $// element "title" &// content
    , _itemLink        = concatMaybe $ c' $// element "link"
                                          &// map T.unpack . attribute "href"
    , _itemDate        = concatMaybe $
                           (c' $// element "published" &// content) ++
                           (c' $// element "updated"   &// content)
    , _itemFeedLink    = Nothing
    , _itemDescription = fmap removeHtml $ concatMaybe $
                           (c' $// element "summary" &// content) ++
                           (c' $// element "content" &// content)
    }
    where
    removeHtml :: Text -> Text
    removeHtml = go False . T.strip
      where
      go :: Bool -> Text -> Text
      go False (T.uncons -> Nothing)       = T.empty
      go False (T.uncons -> Just ('<', t)) = go True  t
      go False (T.uncons -> Just (x, t))   = x `T.cons` go False t
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
    Left  _ -> putStrLn "error"
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
