{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module View (viewModel) where

import Control.Lens
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable
import Data.Sequence (Seq)
import Graphics.Vty hiding (resize)
import Graphics.Vty.Prelude
import Numeric (readHex)

import Feed.Feed
import Feed.Annotated
import Fetching.History
import Model
import Model.Window

------------------------------------------------------------------------

viewModel :: Vty -> Model -> IO ()
viewModel v m = do
  sz  <- displayBounds $ outputIface v
  update v $ picForImage $ render m sz

renderDebug :: Model -> DisplayRegion -> Image
renderDebug m sz = vertCat
  [ separator
  , drawList (string defAttr) (lines (show m))
  , string defAttr ""
  , string defAttr ("Height: " ++ show (regionHeight sz))
  ]

render ::  Model -> DisplayRegion -> Image
render m sz = vertCat
  [ bar " haarss 0.1"
  , separator
  , resizeHeight (regionHeight sz - 5) (drawModel m (regionHeight sz))
  , separator
  , bar (T.pack (status (m^.downloading)))
  , separator
  ]
  where
  status :: Int -> String
  status 0 = ""
  status n = " Downloading (" ++ show n ++ " feed" ++
               (if n == 1 then "" else "s") ++ " to go)"

drawModel :: Model -> Int -> Image
drawModel m h = case m^.browsing.focus of

  TheFeed _         -> drawWin (m^.feeds)
                               feedImage focusedFeedImage

  TheItems f is     -> vertCat
    [ boldly (desc f^.be "")
    , separator
    , drawWin is itemImage focusedItemImage
    ]

  TheText  f _ i s -> vertCat
    [ boldly (desc f^.be "")
    , separator
    , normal (T.center (w - 1) ' ' (i^.item.itemTitle.be ""))
    , separator
    , drawList (\t -> char defAttr ' ' <|> normal t) $
        i^.item.itemDescription.be
          "(no desc)".to (take h . drop s . fmt (min 60 w) . removeHtml)
    ]

  where
  w = 72
  desc :: AnnFeed -> Maybe Text
  desc f = asumOf both (f^.feed.feedDescription,
                        f^.feed.feedTitle)

------------------------------------------------------------------------

drawWin :: Window a -> (a -> Image) -> (a -> Image) -> Image
drawWin w e f = vertCat
  [ drawSeq e (w^.prev)
  , w^.focus.to f
  , drawSeq e (w^.next)
  ]

drawSeq :: (a -> Image) -> Seq a -> Image
drawSeq f = vertCat . toList . fmap f

drawList :: (a -> Image) -> [a] -> Image
drawList f = vertCat . map f

------------------------------------------------------------------------

standoutAttr, boldAttr, standoutBoldAttr :: Attr
standoutAttr     = defAttr      `withStyle` standout
boldAttr         = defAttr      `withStyle` bold
standoutBoldAttr = standoutAttr `withStyle` bold

bar :: Text -> Image
bar t = text' standoutAttr t <|> charFill standoutAttr ' ' (200 :: Int) 1

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
  decode "amp"                           = T.singleton '&'
  decode "gt"                            = T.singleton '>'
  decode "lt"                            = T.singleton '<'
  decode "mdash"                         = T.singleton '—'
  decode "nbsp"                          = T.singleton ' '
  decode "ndash"                         = T.singleton '–'
  decode "quot"                          = T.singleton '"'
  decode t@(T.unpack -> '#' : 'x' : hex) = fromHex hex t
  decode t@(T.unpack -> '#' : 'X' : hex) = fromHex hex t
  decode (T.unpack   -> '#' : dec)       = T.singleton $ chr $ read dec
  decode t                               = '&' `T.cons` t `T.snoc` ';'

  fromHex :: String -> Text -> Text
  fromHex s t = case readHex s of
    [(i, "")] -> T.singleton $ chr i
    _         -> '&' `T.cons` t `T.snoc` ';'
removeHtml (T.uncons -> Just (c, t))   = c `T.cons` removeHtml t
removeHtml (T.uncons -> _)             = error "Impossible."

-- XXX: Wants to be right aligned, is width needed?
unread :: AnnFeed -> Text
unread f = f^.feed.feedItems.to
  (T.pack . (\s -> if s == "0" then "" else "(" ++ s ++ " new)") .
    show . length . filter (not._isRead))

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
