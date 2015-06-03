{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haarss.Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Time
import           Data.Typeable
import           FRP.Sodium
import           FRP.Sodium.IO
import qualified Graphics.Vty        as Vty
import           System.Exit
import           System.Process

import           Haarss.Config
import           Haarss.Fetching
import           Haarss.Interface
import           Haarss.Model
import           Haarss.Model.Window
import           Haarss.View

------------------------------------------------------------------------

main :: IO ()
main = do
  cfg                 <- loadConfig
  vty                 <- Vty.mkVty =<< Vty.standardIOConfig
  sz                  <- Vty.displayBounds $ Vty.outputIface vty
  model               <- resizeModel sz <$> loadModel cfg
  (eEvent, pushEvent) <- sync newEvent
  tid                 <- myThreadId

  sync $ setupReactive cfg vty sz model eEvent tid

  forever (Vty.nextEvent vty >>= sync . pushEvent)
    `catches` [ Handler (\Shutdown  -> do
                  Vty.shutdown vty
                  exitSuccess)
              , Handler (\e         -> do
                  Vty.shutdown vty
                  putStrLn $ "Unexpected error: " ++
                    show (e :: SomeException)
                  exitFailure)
              ]

data Shutdown = Shutdown
  deriving (Show, Typeable)

instance Exception Shutdown where

------------------------------------------------------------------------

setupReactive :: Config -> Vty.Vty -> Vty.DisplayRegion -> Model ->
                 Event Vty.Event -> ThreadId -> Reactive ()
setupReactive cfg vty sz initModel eEvent tid = do

  (eFeedback, pushFeedback) <- newEvent
                            :: Reactive (Event Feedback,
                                         Feedback -> Reactive ())

  eSize <- fmap updates $ hold sz $ filterJust $ fmap
             (\e -> case e of
                      Vty.EvResize h w -> Just (h, w)
                      _                -> Nothing) eEvent
        :: Reactive (Event Vty.DisplayRegion)

  rec
    eCmd <- filterJust <$>
              collectE (uncurry cmd) Normal (snapshot (,) eEvent bModel)
         :: Reactive (Event ExCmd)

    let eResp :: Event ExResp
        eResp = executeAsyncIO $ fmap
          (\(ExCmd o p) -> ExResp o p <$> resp o p) eCmd
          where
          resp :: Op o -> Cmd o -> IO (Resp o)
          resp UpdateFeeds   us = do
            sync $ pushFeedback $ Downloading (length us)
            time <- getCurrentTime
            fs   <- download us (sync $ pushFeedback FeedDownloaded)
                             (cfg^.proxy)
            return (time, fs)
          resp Move          _  = return ()
          resp OpenUrl       mu = case mu of
            Nothing -> return ()
            Just u  -> do
              -- We use createProcess, rather than say rawSystem, so we
              -- can redirect stderr and thus avoid having the terminal
              -- flooded by warnings from the browser.
              _ <- createProcess (proc (cfg^.browser) [u])
                     { std_err = CreatePipe }
              return ()
          resp MarkAllAsRead fs = do

            -- This will save the /previous/ state of the feeds, because
            -- we haven't yet marked them as read -- if we mark as read
            -- twice in a row we save the current state though.
            updateConfig cfg fs
            saveModel fs
            return ()
          resp MarkAsRead    () = return ()
          resp OpenPrompt    _  = return ()
          resp PutPrompt     _  = return ()
          resp DelPrompt     () = return ()
          resp CancelPrompt  () = return ()
          resp ClosePrompt   () = return ()
          resp Quit          fs = do
            updateConfig cfg fs
            saveModel fs
            throwTo tid Shutdown
            threadDelay 1000000 -- Wait for main thread to shut down...
          resp RemoveFeed    () = return ()
          resp Rearrange     _  = return ()
          resp Search        _  = return ()
          resp Scroll        _  = return ()
          resp Resize        _  = return ()

    bModel <- accum initModel $ mconcat
                [ (\(ExResp o p a) -> update o p a) <$> eResp
                , feedback                          <$> eFeedback
                , resizeModel                       <$> eSize
                ]
           :: Reactive (Behavior Model)

  _ <- listen (value bModel) (viewModel vty)

  return ()

------------------------------------------------------------------------

cmd :: Vty.Event -> Model -> Mode -> (Maybe ExCmd, Mode)
cmd _ m Normal | m^.downloading > 0 = (Nothing, Normal)
cmd e _ Normal | key 'K'   == e     = normal Move Top
cmd e _ Normal | key 'k'   == e     = normal Move Up
cmd e _ Normal | key 'j'   == e     = normal Move Down
cmd e _ Normal | key 'J'   == e     = normal Move Bot
cmd e _ Normal | key 'l'   == e     = normal Move In
cmd e _ Normal | enter     == e     = normal Move In
cmd e _ Normal | arrowUp   == e     = normal Move Up
cmd e _ Normal | arrowDown == e     = normal Move Down
cmd e m Normal | key 'q'   == e     = if browsingFeeds m
                                      then normal Quit
                                             (m^.feeds.to closeWindow)
                                      else normal Move Out
cmd e m Normal | key 'R'   == e     = normal UpdateFeeds
                                               [getFeedUrl m]
cmd e m Normal | key 'r'   == e     = normal UpdateFeeds (getFeedUrls m)
cmd e m Normal | key 'o'   == e     = normal OpenUrl (getItemUrl m)
cmd e m Normal | key 'm'   == e     = normal MarkAllAsRead
                                               (m^.feeds.to closeWindow)
cmd e _ Normal | key 'M'   == e     = normal MarkAsRead ()
cmd e _ Normal | key 'D'   == e     = normal RemoveFeed ()
cmd e m Normal | key 'c'   == e &&
                 browsingFeeds m    = input  OpenPrompt RenameFeed
cmd e m Normal | key 'a'   == e &&
                 browsingFeeds m    = input  OpenPrompt AddFeed
cmd e m Normal | key 'P'   == e &&
                 browsingFeeds m    = normal Rearrange Up
cmd e m Normal | key 'N'   == e &&
                 browsingFeeds m    = normal Rearrange Down
cmd e _ Normal | key '/'   == e     = input  OpenPrompt SearchPrompt
cmd e _ Normal | key '\t'  == e     = input  OpenPrompt SearchPrompt
cmd e _ Normal | key ' '   == e     = normal Scroll DownFull
cmd e _ Normal | key 'd'   == e     = normal Scroll DownHalf
cmd e _ Normal | key 'b'   == e     = normal Scroll UpFull
cmd e _ Normal | key 'u'   == e     = normal Scroll UpHalf
cmd _ _ Normal                      = (Nothing, Normal)

cmd e _ Input  | isKey e            = input  PutPrompt (getKey e)
cmd e _ Input  | backspace == e     = input  DelPrompt ()
cmd e _ Input  | esc       == e     = normal CancelPrompt ()
cmd e _ Input  | enter     == e     = normal ClosePrompt ()
cmd e _ Input  | ctrl 'g'  == e     = normal CancelPrompt ()
cmd _ _ Input                       = (Nothing, Input)

------------------------------------------------------------------------

normal, input :: Op o -> Cmd o -> (Maybe ExCmd, Mode)
normal o c = (Just $ ExCmd o c, Normal)
input  o c = (Just $ ExCmd o c, Input)

key, ctrl :: Char -> Vty.Event
key  c    = Vty.EvKey (Vty.KChar c) []
ctrl c    = Vty.EvKey (Vty.KChar c) [Vty.MCtrl]

arrowUp, arrowDown :: Vty.Event
arrowUp   = Vty.EvKey Vty.KUp    []
arrowDown = Vty.EvKey Vty.KDown  []

enter, backspace, esc :: Vty.Event
enter     = Vty.EvKey Vty.KEnter []
backspace = Vty.EvKey Vty.KBS    []
esc       = Vty.EvKey Vty.KEsc   []

isKey :: Vty.Event -> Bool
isKey (Vty.EvKey (Vty.KChar _) []) = True
isKey _                            = False

getKey :: Vty.Event -> Char
getKey (Vty.EvKey (Vty.KChar c) []) = c
getKey _                            = error "getKey"
