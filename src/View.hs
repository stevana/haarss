{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module View (viewModel) where

import           Control.Lens
import           Data.Char                 (chr)
import           Data.Foldable             (toList)
import           Data.Sequence             (Seq)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Graphics.Vty              hiding (resize)
import           Graphics.Vty.Prelude
import           Network.HTTP.Types.Status
import           Numeric                   (readHex)

import           Feed.Annotated
import           Feed.Feed
import           Fetching.History
import           Interface
import           Model
import           Model.Window

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
  , resizeHeight (regionHeight sz - 5) (drawModel m sz)
  , separator
  , bar (T.pack (status (m^.downloading) (m^.prompt)))
  , separator
  ]
  where
  status :: Int -> Maybe (Prompt, String) -> String
  status 0 Nothing       = ""
  status n Nothing       = " Downloading (" ++ show n ++ " feed" ++
                             (if n == 1 then "" else "s") ++ " to go)"
  status 0 (Just (p, s)) = " " ++ show p ++ ": " ++ s
  status _ _             = error "Impossible."

drawModel :: Model -> DisplayRegion -> Image
drawModel m (w, h) = case m^.browsing.focus of

  TheFeed _         -> drawWin (m^.feeds)
                               (feedImage w defAttr)
                               (feedImage w standoutAttr)

  TheItems f is     -> vertCat
    [ attributed boldAttr (desc f^.be "")
    , separator
    , drawWin is (itemImage defAttr boldAttr h)
                 (itemImage standoutAttr standoutBoldAttr h)
    ]

  TheText  f is s -> vertCat
    [ attributed boldAttr (desc f^.be "")
    , separator
    , attributed defAttr
        (T.center (w - 1) ' ' (is^.focus.item.itemTitle.be ""))
    , separator
    , drawList (\t -> char defAttr ' ' <|> attributed defAttr t) $
        is^.focus.item.itemDescription.be
         "(no desc)".to (take h . drop s . fmt (min 60 w) . removeHtml)
    ]

  where
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
  where
  drawSeq :: (a -> Image) -> Seq a -> Image
  drawSeq f = vertCat . toList . fmap f

drawList :: (a -> Image) -> [a] -> Image
drawList f = vertCat . map f

failedImage :: Attr -> AnnFeed -> Image
failedImage attr f = char attr $ case f^.history of
  []                                                       -> ' '
  Success _                                            : _ -> ' '
  Failure _ (DownloadFailure (StatusCodeException' s)) : _
    | s == movedPermanently301                             -> '☈'
    | s == notModified304                                  -> ' '
    | s == notFound404                                     -> '✗'
    | s == forbidden403                                    -> '✋'
    | s == requestTimeout408                               -> '⌚'
    | s == internalServerError500                          -> '⚠'
    | otherwise                                            -> '⚡'
  Failure _ (DownloadFailure OtherException)           : _ -> '¿'
  Failure _ (ParseFailure _)                           : _ -> '✂'
  Failure _ TimeoutFailure                             : _ -> '⌛'
  hs | length hs == 10 && all isFailure hs                 -> '☠'
  Failure _ UnknownFailure                             : _ -> '?'
  where
  isFailure (Failure _ _) = True
  isFailure _             = False

feedImage :: Int -> Attr -> AnnFeed -> Image
feedImage w attr f = horizCat
  [ failedImage attr f
  , text' attr title
  , charFill attr ' ' (w - T.length title - T.length unread - 3) 1
  , attributed attr unread
  , charFill attr ' ' (1 :: Int) 1
  ]
  where
  title :: Text
  title = f^.feed.feedTitle.be "no title"

  unread :: Text
  unread = f^.feed.feedItems.to
    (T.pack . (\s -> if s == "0" then "" else s ++ " new") .
      show . length . filter (not._isRead))

itemImage :: Attr -> Attr -> Int -> AnnItem -> Image
itemImage r n h i = horizCat
  [ attributed attr (i^.item.itemTitle.be "no title")
  , charFill r ' '  h 1
  ]
  where
  attr :: Attr
  attr | i^.isRead = r
       | otherwise = n

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
