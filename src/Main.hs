{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString     as BS
import           Data.Monoid
import           Data.Serialize
import qualified Data.Text           as T
import           Data.Time
import           Data.Typeable

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           System.Exit
import           System.Process

import qualified Graphics.Vty        as Vty

import           FRP.Sodium
import           FRP.Sodium.IO

import           Config
import           Constants
import           Feed.Annotated
import           Fetching
import           Interface
import           Model
import           Model.Window
import           View

------------------------------------------------------------------------

data Save = Save [AnnFeed]
  deriving (Typeable)

instance Show Save where
  show _ = "Save"

instance Exception Save where

------------------------------------------------------------------------

main :: IO ()
main = do
  cfg                 <- readConfig
  vty                 <- Vty.mkVty =<< Vty.standardIOConfig
  sz                  <- Vty.displayBounds $ Vty.outputIface vty
  model               <- resizeModel sz <$> readSavedModel cfg
  (eEvent, pushEvent) <- sync newEvent
  tid                 <- myThreadId

  sync $ setupReactive cfg vty model eEvent tid

  forever (Vty.nextEvent vty >>= sync . pushEvent)
    `catches` [ Handler (\(Save fs) -> do
                  Vty.shutdown vty
                  modelPath <- getModelPath
                  BS.writeFile modelPath $ encode fs
                  exitSuccess)
              , Handler (\e              -> do
                  Vty.shutdown vty
                  putStrLn $ "Unexpected error: " ++
                    show (e :: SomeException)
                  exitFailure)
              ]

------------------------------------------------------------------------

key  c    = Vty.EvKey (Vty.KChar c) []
ctrl c    = Vty.EvKey (Vty.KChar c) [Vty.MCtrl]
enter     = Vty.EvKey Vty.KEnter []
backspace = Vty.EvKey Vty.KBS    []
esc       = Vty.EvKey Vty.KEsc   []

isKey (Vty.EvKey (Vty.KChar _) []) = True
isKey _                            = False

getKey (Vty.EvKey (Vty.KChar c) []) = c
getKey _                            = error "getKey"

setupReactive :: Config -> Vty.Vty -> Model -> Event Vty.Event ->
                 ThreadId -> Reactive ()
setupReactive cfg vty initModel eEvent tid = do

  (eFeedback, pushFeedback) <- newEvent
                            :: Reactive (Event Feedback,
                                         Feedback -> Reactive ())

  let normal, input :: Op o -> Cmd o -> (Maybe ExCmd, Mode)
      normal o c = (Just $ ExCmd o c, Normal)
      input  o c = (Just $ ExCmd o c, Input)

  let cmd :: Vty.Event -> Model -> Mode -> (Maybe ExCmd, Mode)
      cmd e m Normal | m^.downloading > 0 = (Nothing, Normal)
      cmd e _ Normal | key 'K' == e       = normal Move Top
      cmd e _ Normal | key 'k' == e       = normal Move Up
      cmd e _ Normal | key 'j' == e       = normal Move Down
      cmd e _ Normal | key 'J' == e       = normal Move Bot
      cmd e _ Normal | key 'l' == e       = normal Move In
      cmd e _ Normal | enter   == e       = normal Move In
      cmd e m Normal | key 'q' == e       = if browsingFeeds m
                                            then normal Quit
                                                (m^.feeds.to closeWindow)
                                            else normal Move Out
      cmd e m Normal | key 'R' == e       = normal UpdateFeeds
                                                  [getFeedUrl m]
      cmd e _ Normal | key 'r' == e       = normal UpdateFeeds (cfg^.urls)
      cmd e m Normal | key 'o' == e       = normal OpenUrl (getItemUrl m)
      cmd e _ Normal | key 'm' == e       = normal MarkAllAsRead ()
      cmd e _ Normal | key 'M' == e       = normal MarkAsRead ()
      cmd e _ Normal | key 'D' == e       = normal RemoveFeed ()
      cmd e m Normal | key 'a' == e &&
                       browsingFeeds m    = input  OpenPrompt AddFeed
      cmd e m Normal | key 'P' == e &&
                       browsingFeeds m    = normal Rearrange Up
      cmd e m Normal | key 'N' == e &&
                       browsingFeeds m    = normal Rearrange Down
      cmd e _ Normal | key '/' == e       = input  OpenPrompt SearchPrompt
      cmd e _ Normal | key '\t' == e      = input  OpenPrompt SearchPrompt
      cmd _ _ Normal                      = (Nothing, Normal)

      cmd e m Input  | isKey e             = input  PutPrompt (getKey e)
      cmd e m Input  | backspace == e      = input  DelPrompt ()
      cmd e m Input  | esc       == e      = normal CancelPrompt ()
      cmd e m Input  | enter     == e      = normal ClosePrompt ()
      cmd e m Input  | ctrl 'g'  == e      = normal CancelPrompt ()
      cmd _ m Input                        = (Nothing, Input)

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
            fs   <- downloadFeeds us (sync $ pushFeedback FeedDownloaded)
            return (time, fs)
          resp Move          _  = return ()
          resp OpenUrl       mu = case mu of
            Nothing  -> return ()
            Just url -> do

              -- We use createProcess, rather than say rawSystem, so we
              -- can redirect stderr and thus avoid having the terminal
              -- flooded by warnings from the browser.
              _ <- createProcess (proc (cfg^.browser) [url])
                     { std_err = CreatePipe }

              return ()
          resp MarkAllAsRead () = return ()
          resp MarkAsRead    () = return ()
          resp OpenPrompt    _  = return ()
          resp PutPrompt     _  = return ()
          resp DelPrompt     () = return ()
          resp CancelPrompt  () = return ()
          resp ClosePrompt   () = return ()
          resp Quit          fs = throwTo tid $ Save fs
          resp RemoveFeed    () = return ()
          resp Rearrange     _  = return ()

    let update :: Op o -> Cmd o -> Resp o -> Model -> Model
        update Move          d   ()  m = move d m
        update UpdateFeeds   _   tfs m = feedsDownloaded tfs m
        update OpenUrl       _   ()  m = m
        update MarkAllAsRead ()  ()  m = markAllAsRead m
        update MarkAsRead    ()  ()  m = markAsRead m
        update OpenPrompt    p   ()  m = m & prompt ?~ (p, "")
        update PutPrompt     c   ()  m = m & prompt._Just._2 %~ (++ [c])
        update DelPrompt     ()  ()  m = m & prompt._Just._2 %~
                                               \s -> if null s
                                                     then ""
                                                     else init s
        update CancelPrompt  ()  ()  m = m & prompt .~ Nothing
        update ClosePrompt   ()  ()  m = case m^.prompt of
          Just (SearchPrompt, s) -> search (T.pack s) m'
          Just (AddFeed,      s) -> addFeed s m'
          _                      -> m'
          where
          m' = m & prompt .~ Nothing
        update RemoveFeed    ()   () m = removeFeed m
        update Rearrange     Up   () m = rearrangeUpModel m
        update Rearrange     Down () m = rearrangeDownModel m

    let feedback :: Feedback -> Model -> Model
        feedback (Downloading n) m = m & downloading .~ n
        feedback FeedDownloaded  m = m & downloading -~ 1

    bModel <- accum initModel $ mconcat
                [ (\(ExResp o p a) -> update o p a) <$> eResp
                , feedback                          <$> eFeedback
                ]
           :: Reactive (Behavior Model)

  _ <- listen (value bModel) (viewModel vty)

  return ()
