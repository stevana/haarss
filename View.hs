{-# LANGUAGE OverloadedStrings #-}

module View where

import Data.Monoid
import Control.Lens hiding (pre)
import Control.Concurrent.STM
import Data.Char
import Data.List
import Graphics.Vty
import Text.HTML.TagSoup

import qualified Data.Text as T
import Model
import Feeds

------------------------------------------------------------------------

defaultStatus :: String
defaultStatus = " Press 'h' for help."

debug :: Bool
debug = False

render ::  DisplayRegion -> Int -> (Model, String) -> Image
render sz count (m, buf) =
  if debug
  then
    separator
    <->
    drawList id (lines (show m))
    <->
    separator
    <->
    -- string def_attr ("Statusbar:" ++ status buf mcount)
    string def_attr ("Statusbar: " ++ show count)
  else
    bar " haarss 0.1"
    <->
    separator
    <->
    paddedBody
    <->
    separator
    <->
    bar (status count buf)
    where
    bar        = string (def_attr `with_style` standout)
    body       = drawModel m sz
    paddedBody = vert_cat $ body : replicate emptyRows (char def_attr ' ')
      where
      emptyRows :: Int
      emptyRows = max 0 $ fromEnum (region_height sz) - 4 - fromEnum (image_height body)
    status :: Int -> String -> String
    status 0  ""  = defaultStatus
    status 0 b    = " Input:" ++ b
    status n ""   = " Downloading (" ++ show n ++ " feed" ++ (if n == 1
                                                             then ""
                                                             else "s") ++ " to go)"
    status _   _  = "status: bad state"

separator :: Image
separator = char def_attr ' '

view :: Vty -> TVar Int -> (Model, String) -> IO ()
view vty vcount ms = do
  sz <- display_bounds $ terminal vty
  n <- readTVarIO vcount
  -- stat <- readIORef status
  -- i <- atomically $ tryReadTMVar count
  -- let stat' = stat ++ "(count = " ++ show i ++ ")"
  update vty $ pic_for_image $ render sz n ms


standout_attr, bold_attr, standout_bold_attr, underline_attr :: Attr
standout_attr = def_attr `with_style` standout
bold_attr = def_attr `with_style` bold
standout_bold_attr = standout_attr `with_style` bold
underline_attr = def_attr `with_style` underline

title :: AnnItem -> T.Text
-- title = maybe "(No title)" T.unpack . _itemTitle
title = _itemTitle . _item

desc :: Feed' a -> T.Text
desc feed | feed^.feedDescription /= T.empty = feed^.feedDescription
          | otherwise                        = feed^.feedTitle

-- (' ' : maybe (getFeedTitle (fs^.curr)) (\desc -> if null desc then error "a" else error "b") (getFeedDescription (fs^.curr)))


showUnread :: AnnFeed -> T.Text
showUnread feed = feed^.feedItems.to
  (T.pack . (\s -> if s == "0" then "" else "(" ++ s ++ " new)") .
    show . length . filter ((== False) . _isRead))

-- XXX: since feed description (+ separator) was added to ShowItem, we
-- probably need to display fewer feeds, i.e. drop 2 nex?
drawModel :: Model -> DisplayRegion -> Image
drawModel (Model fs i FeedsView) sz = case visible fs (i^.above) (toInteger $ region_height sz) of
  (pre, feed, nex) ->
    drawList (\f -> T.unpack $ f^.feedTitle <> " " <> showUnread f) pre
    <->
    string standout_attr (T.unpack $ ' ' `T.cons` feed^.feedTitle <> " " <> showUnread feed)
    <->
    drawList (\f -> T.unpack $ f^.feedTitle <> " " <> showUnread f) nex
drawModel (Model fs i (ItemsView is False)) sz = case visible is (i^.above) (toInteger $ region_height sz) of
  (pre, feed, nex) ->
    string bold_attr (T.unpack $ T.cons ' ' $ desc (fs^.curr))
    <->
    separator
    <->
    drawList' (map (\it -> if it^.isRead then def_attr else bold_attr) pre) (T.unpack . title) pre
    <->
    string (if feed^.isRead then standout_attr else standout_bold_attr)
           (T.unpack $ ' ' `T.cons` title feed)
    <->
    drawList' (map (\it -> if it^.isRead then def_attr else bold_attr) nex) (T.unpack . title) nex
drawModel (Model fs _ (ItemsView is True)) sz =
  string bold_attr (T.unpack $ ' ' `T.cons` desc (fs^.curr))
  <->
  separator
  <->
  string def_attr (T.unpack $ T.center (fromEnum (region_width sz)) ' '
                                       (is^.curr.item.itemTitle))
  <->
  separator
    -- XXX: what to do when desc is longer than rows? space to scroll
    -- down (1 screen), u (up half screen), see man page of less(1).
  <->
  drawList id lns
  where
  lns :: [String]
  lns = fmt (min 50 (toInteger (region_width sz) - 3)) $
     removeHtml $ T.unpack $ is^.curr.item.itemDescription

visible :: Zip a -> Integer -> Integer -> ([a], a, [a])
visible (Zip pr cu ne) ab rows
  = (pr', cu, take (fromInteger rows - length pr' - 5) ne)
  where
  pr' = drop (fromInteger ab) pr

drawList :: (a -> String) -> [a] -> Image
drawList s xs = drawList' (replicate (length xs) def_attr) s xs

drawList' :: [Attr] -> (a -> String) -> [a] -> Image
drawList' attrs r xs = vert_cat $
  flip map (zip xs attrs) $ \(x, attr) ->
    string attr $ (' ' :) $ r x

fmt :: Integer -> String -> [String]
fmt maxLen = map unwords . go 0 [] . words
  where
  go :: Integer -> [String] -> [String] -> [[String]]
  go _      acc []       = [reverse acc]
  go rowLen acc (w : ws)
    | rowLen + wl + 1 > maxLen = reverse acc : go wl [w] ws
    | otherwise                = go (rowLen + wl + 1) (w : acc) ws
    where
    wl = genericLength w

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
