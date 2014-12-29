{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RecursiveDo,
             DeriveDataTypeable #-}

module Main where

import Data.Typeable
import Data.Monoid
import qualified Data.ByteString as BS
import Data.Serialize
import qualified Data.Text as T
import Data.Time

import Control.Applicative
import Control.Lens
import Control.Exception
import Control.Monad
import System.Exit
import System.Process

import qualified Graphics.Vty as Vty

import FRP.Sodium
import FRP.Sodium.IO

import Constants
import Config
import Model
import Model.Window
import View
import Feed.Annotated
import Fetching

------------------------------------------------------------------------

data Command = Move Dir | Resize | Output String | UpdateFeed | UpdateFeeds
  | OpenUrl | MarkAllAsRead | ToggleReadStatus | Search
  deriving (Eq, Show)

------------------------------------------------------------------------

whenE :: Behavior Bool -> Event a -> Event a
whenE = flip gate

unlessE :: Behavior Bool -> Event a -> Event a
unlessE b f = whenE (not <$> b) f

key :: Char -> Vty.Event
key = modKey []

modKey :: [Vty.Modifier] -> Char -> Vty.Event
modKey ms = flip Vty.EvKey ms . Vty.KChar

enter :: Vty.Event
enter = Vty.EvKey Vty.KEnter []

------------------------------------------------------------------------

data SaveModel = SaveModel Model
  deriving (Typeable)

instance Show SaveModel where
  show _ = "SaveModel"

instance Exception SaveModel where

------------------------------------------------------------------------

main :: IO ()
main = do

  cfg   <- readConfig
  vty   <- Vty.mkVty =<< Vty.standardIOConfig
  sz  <- Vty.displayBounds $ Vty.outputIface vty
  model <- resizeModel sz <$> readSavedModel cfg

  (eEvent, pushEvent) <- sync newEvent
  sync $ setupReactive cfg vty model eEvent

  forever (Vty.nextEvent vty >>= sync . pushEvent)
    `catches` [ Handler (\(SaveModel model') -> do
                  Vty.shutdown vty
                  modelPath <- getModelPath
                  BS.writeFile modelPath $ encode model'
                  exitSuccess)
              , Handler (\e               -> do
                  Vty.shutdown vty
                  failure (show (e :: SomeException)))
              ]
              where
              failure err = do
                putStrLn $ "Unexpected error: " ++ err
                exitFailure

------------------------------------------------------------------------

setupReactive :: Config -> Vty.Vty -> Model -> Event Vty.Event -> Reactive ()
setupReactive config vty iniModel eEvent = do

  bMode <- hold True $ mconcat
             [ False <$ filterE (== key 'a') eEvent
             , True  <$ filterE (== enter)   eEvent
             ]
        :: Reactive (Behavior Bool)

  (eFeedDownloaded, pushFeedDownloaded) <- newEvent

  rec
    let eCommand :: Event Command
        eCommand = mconcat
          [ bindKey 'K' $ Move Top
          , bindKey 'k' $ Move Up
          , bindKey 'j' $ Move Down
          , bindKey 'J' $ Move Bot
          , bindKey 'l' $ Move In
          , bindKey 'q' $ Move Out
          , bindKey 'R'   UpdateFeed
          , bindKey 'r'   UpdateFeeds
          , bindKey 'o'   OpenUrl
          , bindKey 'm'   MarkAllAsRead
          , bindKey 'M'   ToggleReadStatus
          , bindKey '\t'  Search
          , Resize <$ filterE isResize eEvent `mappend`
                      filterE (== modKey [Vty.MCtrl] 'l') (whenE bMode eEvent)
          ]
          where
          bindKey :: Char -> Command -> Event Command
          bindKey char cmd = cmd <$ filterE (== key char)
            (whenE ((0 ==) . _downloading <$> bModel) $ whenE bMode eEvent)

          isResize (Vty.EvResize _ _) = True
          isResize _                  = False

    let eFeeds :: Event (UTCTime, [AnnFeed])
        eFeeds = executeAsyncIO $ const io <$> filterE (== UpdateFeeds) eCommand
          where
          io :: IO (UTCTime, [AnnFeed])
          io = do
            time <- getCurrentTime
            fs   <- downloadFeeds (config^.urls) (sync (pushFeedDownloaded ()))
            return (time, fs)

    -- XXX: Moving after update feed causes the wrong feed to be
    -- updated. This is currently "fixed" by not allowing any commands
    -- while downloading.

    -- XXX: Using syncIO instead of asyncIO causes thread blocked on MVar...
    let eFeed :: Event AnnFeed
        eFeed = executeAsyncIO $ snapshot
          (\_ model ->

            -- The getting the feed url for the overview feed will fail,
            -- in which case we simply return the overview feed.
            case getFeedUrl config model of
              Nothing  -> return (model^.browsing.focus.annFeed)
              Just url -> io url)
          (filterE (== UpdateFeed) eCommand)
          bModel
          where
          io :: String -> IO AnnFeed
          io url = head <$> downloadFeeds [url] (sync (pushFeedDownloaded ()))

    bModel <- let cmdSem :: Command -> (Model -> Model) -> Event (Model -> Model)
                  cmdSem cmd change = change <$ filterE (== cmd) eCommand

              in accum iniModel $ mconcat
                   [ cmdSem (Move Top)  $ move Top
                   , cmdSem (Move Up)   $ move Up
                   , cmdSem (Move Down) $ move Down
                   , cmdSem (Move Bot)  $ move Bot
                   , cmdSem (Move In)   $ move In
                   , cmdSem (Move Out)  $ \model -> if browsingFeeds model
                                                    then throw $ SaveModel model
                                                    else move Out model

                   -- XXX: Resize needs access to IO to get the new size...
                   , cmdSem Resize id

                   -- XXX: This won't work for the overview feed.
                   , cmdSem ToggleReadStatus toggleReadStatus

                   , cmdSem MarkAllAsRead makeAllAsRead

                   , cmdSem UpdateFeed $ \model ->
                       model & downloading .~ 1

                   , cmdSem UpdateFeeds $ \model ->
                       model & downloading .~ config^.urls.to length

                   , cmdSem Search $ search $ T.pack "agda" -- XXX

                     -- XXX: Get the time as well?
                     -- XXX: Can we derive this from feedsDownload?
                   , feedDownloaded <$> eFeed

                   -- XXX: this loses the current position (maybe not bad?)
                   , feedsDownloaded <$> eFeeds

                   , (\model -> model & downloading %~ pred) <$ eFeedDownloaded
                   ]
           :: Reactive (Behavior Model)

  let eOpenUrl :: Event (IO ())
      eOpenUrl = filterJust $ snapshot (const helper)
                                       (filterE (== OpenUrl) eCommand) bModel
        where
        helper :: Model -> Maybe (IO ())
        helper model = do
          url <- getItemUrl model
          return $ do
            _ <- createProcess (proc (config^.browser) [T.unpack url])
                   { std_err = CreatePipe }
            return ()

  _ <- listen (value bModel) (viewModel vty)
  _ <- listen eOpenUrl       id

  return ()

------------------------------------------------------------------------
{-
  let bInputBuffer :: Behavior String
      bInputBuffer
        = accumB "" $ (filterJust $ keys <$> whenE bMode eEvent)
                      `union`
                      (const " " <$ (filterE (== key 'a') $ unlessE bMode eEvent))
        where
        keys :: Vty.Event -> Maybe (String -> String)
        keys (Vty.EvKey Vty.KEnter     []) = Just (const [])
        keys (Vty.EvKey (Vty.KASCII c) []) = Just (++ [c])
        keys (Vty.EvKey Vty.KBS        []) = Just backSpace
          where
          backSpace xs = let xs' = reverse (drop 1 (reverse xs)) in -- XXX: inefficient...
                         if null xs' then " " else xs'
        keys _                             = Nothing

  let eReady = () <$ filterE (== enter) eEvent

  let eSend :: Event t String
      eSend = bInputBuffer <@ whenE bMode eReady

  let eCommand :: Event t Command
      eCommand = mapT keyBindings (unlessE bMode eEvent)

        `union` (Output <$> eSend)

    let bModel :: Behavior t Model
        bModel = accumB (initialModel (toInteger $ Vty.region_height sz))
               $ mapT commandSemantics eCommand
                 `union`
                 ((\(Output s) _ -> error "fix") <$> filterE isOutput eCommand)
          where
          isOutput (Output _) = True
          isOutput _          = False


  eModelBuffer <- changes $ (,) <$> bModel <*> bInputBuffer

  reactimate $ view vty status vcount <$> eModelBuffer

-}
