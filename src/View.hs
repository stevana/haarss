{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module View where

import Control.Lens
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty
import Numeric (readHex)

import Feed.Feed
import Feed.Annotated
import Fetching.History
import Model

------------------------------------------------------------------------

standoutAttr, boldAttr, standoutBoldAttr :: Attr
standoutAttr     = defAttr      `withStyle` standout
boldAttr         = defAttr      `withStyle` bold
standoutBoldAttr = standoutAttr `withStyle` bold

bar :: Text -> Image
bar t = text' standoutAttr t <|> charFill standoutAttr ' ' (100 :: Int) 1

separator :: Image
separator = char defAttr ' '

attributed :: Attr -> Text -> Image
attributed attr t = char attr ' ' <|> text' attr t

normal :: Text -> Image
normal = attributed defAttr

focused :: Text -> Image
focused = attributed standoutAttr

boldly :: Text -> Image
boldly = attributed boldAttr

drawZip :: Zip a -> (a -> Image) -> (a -> Image) -> Int -> Int -> Image
drawZip z e f w h = vertCat
  [ drawList e aboveCursor
  , z^.curr.to f
  , drawList e (z^.next.to (take (h - length aboveCursor)))
  ]
  where
  aboveCursor = z^.prev.to (drop w)

drawList :: (a -> Image) -> [a] -> Image
drawList f = vertCat . map f

------------------------------------------------------------------------

fmt :: Int -> Text -> [Text]
fmt maxLen = map T.unwords . go 0 [] . T.words
  where
  go :: Int -> [Text] -> [Text] -> [[Text]]
  go _      acc []       = [reverse acc]
  go rowLen acc (w : ws)
    | rowLen + wl + 1 > maxLen = reverse acc : go wl [w] ws
    | otherwise                = go (rowLen + wl + 1) (w : acc) ws
    where
    wl = T.length w

removeHtml :: Text -> Text
removeHtml (T.uncons -> Nothing)       = T.empty
removeHtml (T.uncons -> Just ('<', t)) =
  case T.break (== '>') t & _2 %~ T.uncons of
    -- XXX: fmt removes the newlines...
    ("p",     Just ('>', t')) -> "\n\n" `T.append` removeHtml t'
    -- XXX: might want to drop everything inside the style tag...
    ("style", Just ('>', t')) -> removeHtml t'
    (_,       Just ('>', t')) -> removeHtml t'
    _                         -> T.empty
removeHtml (T.uncons -> Just ('&', t)) =
  case T.break (== ';') t & _2 %~ T.uncons of
    (code, Just (_, t')) -> decode code `T.append` removeHtml t'
    _                    -> T.empty
  where
  decode :: Text -> Text
  decode "lt"                            = T.singleton '<'
  decode "gt"                            = T.singleton '>'
  decode "amp"                           = T.singleton '&'
  decode "quot"                          = T.singleton '"'
  decode "ndash"                         = T.singleton '–'
  decode "mdash"                         = T.singleton '—'
  decode t@(T.unpack -> '#' : 'x' : hex) = fromHex hex t
  decode t@(T.unpack -> '#' : 'X' : hex) = fromHex hex t
  decode (T.unpack   -> '#' : dec)       = T.singleton $ chr $ read dec
  decode t                               = '&' `T.cons` t `T.snoc` ';'

  fromHex :: String -> Text -> Text
  fromHex s t = case readHex s of
    [(i, "")] -> T.singleton $ chr i
    _         -> '&' `T.cons` t `T.snoc` ';'
removeHtml (T.uncons -> Just (c, t)) = c `T.cons` removeHtml t
removeHtml (T.uncons -> _)           = error "Impossible."

unread :: AnnFeed -> Text
unread f = f^.feed.feedItems.to
  (T.pack . (\s -> if s == "0" then "" else "(" ++ s ++ " new)") .
    show . length . filter ((== False) . _isRead))

failedImage :: Attr -> AnnFeed -> Image
failedImage attr f
  | f^.history.to failed = char attr '!'
  | otherwise            = char attr ' '

feedImage :: AnnFeed -> Image
feedImage f = horizCat
  [ failedImage defAttr f
  , text' defAttr $ f^.feed.feedTitle.be "no title"
  , normal $ unread f
  ]

focusedFeedImage :: AnnFeed -> Image
focusedFeedImage f = horizCat
  [ failedImage standoutAttr f
  , text' standoutAttr $ f^.feed.feedTitle.be "no title"
  , focused $ unread f
  ]

itemImage :: AnnItem -> Image
itemImage i = attributed attr (i^.item.itemTitle.be "no title")
  where
  attr :: Attr
  attr | i^.isRead = defAttr
       | otherwise = boldAttr

focusedItemImage :: AnnItem -> Image
focusedItemImage i = horizCat
  [ attributed attr $ i^.item.itemTitle.be "no title"
  , charFill standoutAttr ' ' (100 :: Int) 1
  ]
  where
  attr :: Attr
  attr | i^.isRead = standoutAttr
       | otherwise = standoutBoldAttr

drawModel :: Model -> Image
drawModel m =
  let p = m^.vty.position.window
      w = 50 -- XXX: m^.vty.width - 3...
      h = m^.vty.height
  in
  case m^.browsing of

    TheFeeds fs      -> drawZip fs feedImage focusedFeedImage p h

    TheItems fs is   -> vertCat
      [ boldly (desc fs^.be "")
      , separator
      , drawZip is itemImage focusedItemImage p h
      ]

    TheText  fs _ i s -> vertCat
      [ boldly (desc fs^.be "")
      , separator
      -- XXX: use width instead of 80 below.
      , normal (T.center (80 - 1) ' ' (i^.item.itemTitle.be ""))
      , separator
      -- XXX: use height when scrolling?
      -- XXX: replace 50 below with height.
      , drawList (\t -> char defAttr ' ' <|> normal t) $
          i^.item.itemDescription.be
            "(no desc)".to (take 50 . drop s . fmt (min 50 w) . removeHtml)
      ]

  where
  desc :: Zip AnnFeed -> Maybe Text
  desc fs = asumOf both (fs^.curr.feed.feedDescription,
                        fs^.curr.feed.feedTitle)

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
    drawList (string defAttr) (lines (show m))
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
    paddedBody = vertCat $ body : replicate emptyRows separator
      where
      -- XXX: why is +1 needed here?

      emptyRows :: Int
      emptyRows = max 0 $ m^.vty.height + 1 - imageHeight body
    status :: Int -> String -> String
    status 0  ""  = defaultStatus
    --- status 0 b    = " Input:" ++ b
    status n ""   = " Downloading (" ++
      show n ++ " feed" ++ (if n == 1 then "" else "s") ++ " to go)"
    status _   _  = "status: bad state"

view :: Vty -> (Model, String) -> IO ()
view v = update v . picForImage . render
