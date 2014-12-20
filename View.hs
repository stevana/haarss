{-# LANGUAGE OverloadedStrings #-}

module View where

import Data.Monoid
import Control.Lens hiding (pre)
import Data.Char
import Data.List
import Graphics.Vty
import Graphics.Vty.Prelude
import Text.HTML.TagSoup

import Data.Text (Text)
import qualified Data.Text as T
import Model
import Feeds

------------------------------------------------------------------------

defaultStatus :: String
defaultStatus = " Press 'h' for help."

debug :: Bool
debug = False

render ::  (Model, String) -> Image
render (m, buf) =
  if debug
  then
    separator
    <->
    drawList T.pack (lines (show m))
  else
    bar " haarss 0.1"
    <->
    separator
    <->
    paddedBody
    <->
    separator
    <->
    bar (T.pack (status (m^.downloading) buf))
    where
    body       = drawModel m
    paddedBody = vertCat $ body : replicate emptyRows (char defAttr ' ')
      where
      -- XXX: why is +1 needed here?
      emptyRows :: Int
      emptyRows = max 0 $ m^.vty.height + 1 - imageHeight body
    status :: Int -> String -> String
    status 0  ""  = defaultStatus
    --- status 0 b    = " Input:" ++ b
    status n ""   = " Downloading (" ++ show n ++ " feed" ++ (if n == 1
                                                             then ""
                                                             else "s") ++ " to go)"
    status _   _  = "status: bad state"

bar :: Text -> Image
bar t = text' standoutAttr t <|> charFill standoutAttr ' ' 100 1

separator :: Image
separator = char defAttr ' '

view :: Vty -> (Model, String) -> IO ()
view vty ms = do
  update vty $ picForImage $ render ms

------------------------------------------------------------------------

drawZip :: Zip a -> (a -> Image) -> (a -> Image) -> Int -> Int -> Image
drawZip z e f w h =
  drawList'' e aboveCursor
  <->
  z^.curr.to f
  <->
  drawList'' e (z^.next.to (take (h - length aboveCursor)))
  where
  aboveCursor = z^.prev.to (drop w)

unread :: AnnFeed -> Text
unread f = f^.feedItems.to
  (T.pack . (\s -> if s == "0" then "" else "(" ++ s ++ " new)") .
    show . length . filter ((== False) . _isRead))

status :: AnnFeed -> Image
status f = char defAttr ' '

feedImage :: AnnFeed -> Image
feedImage f = horizCat
  [ status f
  , f^.feedTitle.to (text' defAttr)
  , char defAttr ' '
  , text' defAttr (unread f)
  ]

itemImage :: AnnItem -> Image
itemImage i = char defAttr ' ' <|> i^.item.itemTitle.to (text' defAttr)

focusedFeedImage :: AnnFeed -> Image
focusedFeedImage f = horizCat
  [ f^.feedTitle.to focusedText
  , char standoutAttr ' '
  , text' standoutAttr (unread f)
  ]

focusedItemImage :: AnnItem -> Image
focusedItemImage i = i^.item.itemTitle.to focusedText

focusedText :: Text -> Image
focusedText t = char standoutAttr ' ' <|> text' standoutAttr t
  -- Need the width here to do it properly...
  -- <|> charFill standoutAttr ' ' 100 1



drawModel :: Model -> Image
drawModel m =
  let p = m^.vty.position.window
      w = 50 -- XXX: m^.vty.width - 3...
      h = m^.vty.height
  in
  case m^.browsing of
    TheFeeds fs      -> drawZip fs feedImage focusedFeedImage p h
    TheItems _ is    -> drawZip is itemImage focusedItemImage p h
    TheText  _ _ i s -> drawList id (i^.item.itemDescription.to
                          (fmt' (min 50 w)))
------------------------------------------------------------------------


standoutAttr, boldAttr, standout_boldAttr, underline_attr :: Attr
standoutAttr = defAttr `withStyle` standout
boldAttr     = defAttr `withStyle` bold
standout_boldAttr = standoutAttr `withStyle` bold
underline_attr = defAttr `withStyle` underline

{-
title :: AnnItem -> T.Text
-- title = maybe "(No title)" T.unpack . _itemTitle
title = _itemTitle . _item

desc :: Feed' a -> T.Text
desc feed | feed^.feedDescription /= T.empty = feed^.feedDescription
          | otherwise                        = feed^.feedTitle

-- (' ' : maybe (getFeedTitle (fs^.curr)) (\desc -> if null desc then error "a" else error "b") (getFeedDescription (fs^.curr)))



-- XXX: since feed description (+ separator) was added to ShowItem, we
-- probably need to display fewer feeds, i.e. drop 2 nex?
drawModel :: Model -> DisplayRegion -> Image
drawModel (Model fs i FeedsView _ _) sz = case visible fs (i^.above) (regionHeight sz) of
  (pre, feed, nex) ->
    drawList (\f -> T.unpack $ f^.feedTitle <> " " <> showUnread f) pre
    <->
    string standoutAttr (T.unpack $ ' ' `T.cons` feed^.feedTitle <> " " <> showUnread feed)
    <->
    drawList (\f -> T.unpack $ f^.feedTitle <> " " <> showUnread f) nex
drawModel (Model fs i (ItemsView is False) _ _) sz = case visible is (i^.above) (regionHeight sz) of
  (pre, feed, nex) ->
    string boldAttr (T.unpack $ T.cons ' ' $ desc (fs^.curr))
    <->
    separator
    <->
    drawList' (map (\it -> if it^.isRead then defAttr else boldAttr) pre) (T.unpack . title) pre
    <->
    string (if feed^.isRead then standoutAttr else standout_boldAttr)
           (T.unpack $ ' ' `T.cons` title feed)
    <->
    drawList' (map (\it -> if it^.isRead then defAttr else boldAttr) nex) (T.unpack . title) nex
drawModel (Model fs _ (ItemsView is True) _ _) sz =
  string boldAttr (T.unpack $ ' ' `T.cons` desc (fs^.curr))
  <->
  separator
  <->
  string defAttr (T.unpack $ T.center (fromEnum (regionWidth sz)) ' '
                                       (is^.curr.item.itemTitle))
  <->
  separator
    -- XXX: what to do when desc is longer than rows? space to scroll
    -- down (1 screen), u (up half screen), see man page of less(1).
  <->
  drawList id lns
  where
  lns :: [String]
  lns = fmt (min 50 (toInteger (regionWidth sz) - 3)) $
     removeHtml $ T.unpack $ is^.curr.item.itemDescription

visible :: Zip a -> Int -> Int -> ([a], a, [a])
visible (Zip pr cu ne) ab rows
  = (pr', cu, take (rows - length pr' - 5) ne)
  where
  pr' = drop ab pr
-}

drawList'' :: (a -> Image) -> [a] -> Image
drawList'' f = vertCat . map f

drawList :: (a -> T.Text) -> [a] -> Image
drawList s xs = drawList' (replicate (length xs) defAttr) s xs

drawList' :: [Attr] -> (a -> T.Text) -> [a] -> Image
drawList' attrs r xs = vertCat $
  flip map (zip xs attrs) $ \(x, attr) ->
    text' attr $ T.singleton ' ' `T.append` r x

fmt :: Int -> String -> [String]
fmt maxLen = map unwords . go 0 [] . words
  where
  go :: Int -> [String] -> [String] -> [[String]]
  go _      acc []       = [reverse acc]
  go rowLen acc (w : ws)
    | rowLen + wl + 1 > maxLen = reverse acc : go wl [w] ws
    | otherwise                = go (rowLen + wl + 1) (w : acc) ws
    where
    wl = length w

fmt' :: Int -> T.Text -> [T.Text]
fmt' maxLen = map T.unwords . go 0 [] . T.words
  where
  go :: Int -> [T.Text] -> [T.Text] -> [[T.Text]]
  go _      acc []       = [reverse acc]
  go rowLen acc (w : ws)
    | rowLen + wl + 1 > maxLen = reverse acc : go wl [w] ws
    | otherwise                = go (rowLen + wl + 1) (w : acc) ws
    where
    wl = T.length w

{-
------------------------------------------------------------------------

-- XXX: how do we save newlines? fmt's words/unwords mangles them...
removeHtml :: String -> String
removeHtml
  = innerText
  -- . replace (TagOpen "p" [])  (TagText " NEWLINE ")
  -- . replace (TagOpen "br" []) (TagText " NEWLINE ")
  . parseTags

replace :: Eq a => a -> a -> [a] -> [a]
replace _   _   []       = []
replace old new (x : xs) | x == old  = new : replace old new xs
                         | otherwise = x   : replace old new xs


------------------------------------------------------------------------

prop_fmt :: Integer -> String -> Bool
prop_fmt i s = filter (not . isSpace) (concat (fmt i s)) ==
               filter (not . isSpace) (concat (words s))

prop_replace :: Int -> [Int] -> Bool
prop_replace x xs = replace x x xs == xs
-}
