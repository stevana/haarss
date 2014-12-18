{-# LANGUAGE OverloadedStrings #-}

module View where

import Data.Monoid
import Control.Lens hiding (pre)
import Data.Char
import Data.List
import Graphics.Vty
import Graphics.Vty.Prelude
import Text.HTML.TagSoup

import qualified Data.Text as T
import Model
import Feeds

------------------------------------------------------------------------

defaultStatus :: String
defaultStatus = " Press 'h' for help."

debug :: Bool
debug = False

render ::  DisplayRegion -> (Model, String) -> Image
render sz (m, buf) =
  if debug
  then
    separator
    <->
    drawList id (lines (pretty m))
    <->
    separator
    <->
    string defAttr ("Statusbar: " ++ m^.downloading.to show)
  else
    bar " haarss 0.1"
    <->
    separator
    <->
    paddedBody
    <->
    separator
    <->
    bar (status (m^.downloading) buf)
    where
    bar        = string (defAttr `withStyle` standout)
    body       = drawModel m sz
    paddedBody = vertCat $ body : replicate emptyRows (char defAttr ' ')
      where
      emptyRows :: Int
      emptyRows = max 0 $ fromEnum (regionHeight sz) - 4 - fromEnum (imageHeight body)
    status :: Int -> String -> String
    status 0  ""  = defaultStatus
    status 0 b    = " Input:" ++ b
    status n ""   = " Downloading (" ++ show n ++ " feed" ++ (if n == 1
                                                             then ""
                                                             else "s") ++ " to go)"
    status _   _  = "status: bad state"

separator :: Image
separator = char defAttr ' '

view :: Vty -> (Model, String) -> IO ()
view vty ms = do
  sz <- displayBounds $ outputIface vty
  -- stat <- readIORef status
  -- i <- atomically $ tryReadTMVar count
  -- let stat' = stat ++ "(count = " ++ show i ++ ")"
  update vty $ picForImage $ render sz ms


standout_attr, bold_attr, standout_bold_attr, underline_attr :: Attr
standout_attr = defAttr `withStyle` standout
bold_attr = defAttr `withStyle` bold
standout_bold_attr = standout_attr `withStyle` bold
underline_attr = defAttr `withStyle` underline

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
drawModel (Model fs i FeedsView _) sz = case visible fs (i^.above) (toInteger $ regionHeight sz) of
  (pre, feed, nex) ->
    drawList (\f -> T.unpack $ f^.feedTitle <> " " <> showUnread f) pre
    <->
    string standout_attr (T.unpack $ ' ' `T.cons` feed^.feedTitle <> " " <> showUnread feed)
    <->
    drawList (\f -> T.unpack $ f^.feedTitle <> " " <> showUnread f) nex
drawModel (Model fs i (ItemsView is False) _) sz = case visible is (i^.above) (toInteger $ regionHeight sz) of
  (pre, feed, nex) ->
    string bold_attr (T.unpack $ T.cons ' ' $ desc (fs^.curr))
    <->
    separator
    <->
    drawList' (map (\it -> if it^.isRead then defAttr else bold_attr) pre) (T.unpack . title) pre
    <->
    string (if feed^.isRead then standout_attr else standout_bold_attr)
           (T.unpack $ ' ' `T.cons` title feed)
    <->
    drawList' (map (\it -> if it^.isRead then defAttr else bold_attr) nex) (T.unpack . title) nex
drawModel (Model fs _ (ItemsView is True) _) sz =
  string bold_attr (T.unpack $ ' ' `T.cons` desc (fs^.curr))
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

visible :: Zip a -> Integer -> Integer -> ([a], a, [a])
visible (Zip pr cu ne) ab rows
  = (pr', cu, take (fromInteger rows - length pr' - 5) ne)
  where
  pr' = drop (fromInteger ab) pr

drawList :: (a -> String) -> [a] -> Image
drawList s xs = drawList' (replicate (length xs) defAttr) s xs

drawList' :: [Attr] -> (a -> String) -> [a] -> Image
drawList' attrs r xs = vertCat $
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
