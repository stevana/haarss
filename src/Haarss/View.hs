{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Haarss.View (viewModel) where

import           Control.Lens
import           Data.Char                 (chr)
import           Data.Foldable             (toList)
import           Data.Sequence             (Seq)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Version              (showVersion)
import           Graphics.Vty              hiding (resize)
import           Network.HTTP.Types.Status
import           Numeric                   (readHex)
import           Paths_haarss              (version)

import           Haarss.Feed.Annotated
import           Haarss.Feed.Feed
import           Haarss.Fetching.History
import           Haarss.Interface
import           Haarss.Model hiding (update)
import           Haarss.Model.Window


------------------------------------------------------------------------

viewModel :: Vty -> Model -> IO ()
viewModel v = update v . picForImage . render

{-
renderDebug :: Model -> Image
renderDebug m = vertCat
  [ separator
  , drawList (string defAttr) (lines (show m))
  , string defAttr ""
  , string defAttr ("Height: " ++ show h)
  ]
  where
  (_, h) = m^.displayRegion
-}

render ::  Model -> Image
render m = vertCat
  [ bar $ T.pack $ " haarss " ++ showVersion version
  , separator
  , resizeHeight (h - 5) (drawModel m)
  , separator
  , bar (T.pack (status (m^.downloading) (m^.prompt)))
  , separator
  ]
  where
  (_, h) = m^.displayRegion

  status :: Int -> Maybe (Prompt, String) -> String
  status 0 Nothing       = ""
  status n Nothing       = " Downloading (" ++ show n ++ " feed" ++
                             (if n == 1 then "" else "s") ++ " to go)"
  status 0 (Just (p, s)) = " " ++ show p ++ ": " ++ s
  status _ _             = error "Impossible."

drawModel :: Model -> Image
drawModel m = case m^.browsing.focus of

  TheFeed _        -> drawWin (m^.feeds)
                              (feedImage w defAttr)
                              (feedImage w standoutAttr)

  TheItems f is    -> vertCat
    [ attributed boldAttr (desc f^.be "")
    , separator
    , drawWin is (itemImage defAttr boldAttr w)
                 (itemImage standoutAttr standoutBoldAttr w)
    ]

  TheText f is sds -> vertCat
    [ attributed boldAttr (desc f^.be "")
    , separator
    , attributed defAttr
        (T.center (w - 1) ' ' (is^.focus.item.itemTitle.be ""))
    , separator
    , drawList (\t -> char defAttr ' ' <|> attributed defAttr t) $
        is^.focus.item.itemDescription.be
         "(no desc)".to (scrollText sds . fmt (min 60 w) . removeHtml)
    ]

  where
  (w, h) = m^.displayRegion

  desc :: AnnFeed -> Maybe Text
  desc f = asumOf both (f^.feed.feedDescription,
                        f^.feed.feedTitle)

  -- XXX: Why do we need to add the blank line?
  scrollText :: [ScrollDir] -> [Text] -> [Text]
  scrollText sds ts = "" : drop scrollLines ts
    where
    scrollLines :: Int
    scrollLines = foldr (helper (h - 10) (length ts)) 0 sds
      where
      helper :: Int -> Int -> ScrollDir -> Int -> Int
      helper sz mx DownFull ih | ih + sz >= mx           = ih
                               | otherwise               = ih + sz
      helper sz mx DownHalf ih | ih + (sz `div` 2) >= mx = ih
                               | otherwise               = ih + sz `div` 2
      helper sz _  UpFull   ih | ih - sz <= 0            = 0
                               | otherwise               = ih - sz
      helper sz _  UpHalf   ih | ih - (sz `div` 2) <= 0  = 0
                               | otherwise               = ih - sz `div` 2

------------------------------------------------------------------------

drawWin :: Window a -> (a -> Image) -> (a -> Image) -> Image
drawWin w e f = vertCat
  [ drawSeq e (w^.prev)
  , w^.focus.to f
  , drawSeq e (w^.next)
  ]
  where
  drawSeq :: (a -> Image) -> Seq a -> Image
  drawSeq f' = vertCat . toList . fmap f'

drawList :: (a -> Image) -> [a] -> Image
drawList f = vertCat . map f

failedImage :: Attr -> AnnFeed -> Image
failedImage attr f = char attr $ case f^.history of
  []                                                       -> ' '
  Success _                                            : _ -> ' '
  hs | length hs >= 10 && all isFailure hs                 -> '☠'
  Failure _ (DownloadFailure (StatusCodeException' s)) : _
    | s == movedPermanently301                             -> 'M'
    | s == notModified304                                  -> ' '
    | s == notFound404                                     -> 'X'
    | s == forbidden403                                    -> 'F'
    | s == requestTimeout408                               -> 'T'
    | s == internalServerError500                          -> 'I'
    | otherwise                                            -> '!'
  Failure _ (DownloadFailure OtherException)           : _ -> '?'
  Failure _ (ParseFailure _)                           : _ -> 'P'
  Failure _ TimeoutFailure                             : _ -> '⌛'
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
  title = (asumOf both (f^.alias, f^.feed.feedTitle))^.be "no title"

  unread :: Text
  unread = f^.feed.feedItems.to
    (T.pack . (\s -> if s == "0" then "" else s ++ " new") .
      show . length . filter (not._isRead))

itemImage :: Attr -> Attr -> Int -> AnnItem -> Image
itemImage r n w i = horizCat
  [ attributed attr (i^.item.itemTitle.be "no title")
  , charFill r ' '  w 1
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
removeHtml = go ""
  where
  go :: String -> Text -> Text
  go acc (T.uncons -> Nothing)       = T.pack $ reverse acc
  go acc (T.uncons -> Just ('<', t)) =
    case T.break (== '>') t & _2 %~ T.uncons of
      -- XXX: fmt removes the newlines...
      ("p",     Just ('>', t')) -> go ('\n' : '\n' : acc) t'
      -- XXX: might want to drop everything inside the style tag...
      ("style", Just ('>', t')) -> go acc t'
      (_,       Just ('>', t')) -> go acc t'
      (tag,     _)              -> T.pack $ reverse acc ++ T.unpack tag
                                                        ++ "[>]"
  go acc (T.uncons -> Just ('&', t0)) =
    case T.break (== ';') t0 & _2 %~ T.uncons of
      (code, Just (';', t')) -> go (maybe ('&' : T.unpack code ++";"++ acc)
                                          (: acc)
                                          (decode code)) t'
      (code, _)              -> T.pack $ reverse acc
                                  ++ '&' : T.unpack code ++ "[;]"
    where
    decode :: Text -> Maybe Char
    decode "amp"                         = Just '&'
    decode "gt"                          = Just '>'
    decode "lt"                          = Just '<'
    decode "mdash"                       = Just '—'
    decode "nbsp"                        = Just ' '
    decode "ndash"                       = Just '–'
    decode "quot"                        = Just '"'
    decode (T.unpack -> '#' : 'x' : hex) = fromHex hex
    decode (T.unpack -> '#' : 'X' : hex) = fromHex hex
    decode (T.unpack   -> '#' : dec)     = Just $ chr $ read dec
    decode _                             = Nothing

    fromHex :: String -> Maybe Char
    fromHex s = case readHex s of
      [(i, "")] -> Just $ chr i
      _         -> Nothing
  go acc (T.uncons -> Just (c, t))   = go (c : acc) t
  go _   (T.uncons -> _)             = error "Impossible."
