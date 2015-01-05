{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, PatternSynonyms,
             GADTs, RecursiveDo #-}

module Main where

import Data.Typeable
import Data.Monoid
import qualified Data.ByteString as BS
import Data.Serialize
import qualified Data.Text as T
import Data.Time

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Lens
import System.Exit
import System.Process

import qualified Graphics.Vty as Vty

import FRP.Sodium
import FRP.Sodium.IO

import Constants
import Config
import Interface
import Model
import Model.Window
import View
import Feed.Annotated
import Fetching

------------------------------------------------------------------------

data SaveModel = SaveModel [AnnFeed]
  deriving (Typeable)

instance Show SaveModel where
  show _ = "SaveModel"

instance Exception SaveModel where

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
    `catches` [ Handler (\(SaveModel fs) -> do
                  Vty.shutdown vty
                  modelPath <- getModelPath
                  BS.writeFile modelPath $ encode fs
                  exitSuccess)
              , Handler (\e              -> do
                  Vty.shutdown vty
                  failure (show (e :: SomeException)))
              ]
              where
              failure err = do
                putStrLn $ "Unexpected error: " ++ err
                exitFailure

------------------------------------------------------------------------

pattern Key c       = ModKey [] c
pattern ModKey ms c = Vty.EvKey (Vty.KChar c) ms
pattern Enter       = Vty.EvKey Vty.KEnter []
pattern BS          = Vty.EvKey Vty.KBS    []
pattern Esc         = Vty.EvKey Vty.KEsc   []

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
      cmd _          m _      | m^.downloading > 0 = (Nothing, Normal)
      cmd (Key 'K')  m Normal = normal Move Top
      cmd (Key 'k')  m Normal = normal Move Up
      cmd (Key 'j')  m Normal = normal Move Down
      cmd (Key 'J')  m Normal = normal Move Bot
      cmd (Key 'l')  m Normal = normal Move In
      cmd Enter      m Normal = normal Move In
      cmd (Key 'q')  m Normal = if browsingFeeds m
                                then normal Quit (m^.feeds.to closeWindow)
                                else normal Move Out
      cmd (Key 'R')  m Normal = normal UpdateFeed  (getFeedUrl m)
      cmd (Key 'r')  m Normal = normal UpdateFeeds (cfg^.urls)
      cmd (Key 'o')  m Normal = normal OpenUrl (getItemUrl m)
      cmd (Key 'm')  m Normal = normal MarkAllAsRead ()
      cmd (Key 'M')  m Normal = normal MarkAsRead ()
      cmd (Key 'D')  m Normal = normal RemoveFeed ()
      cmd (Key 'a')  m Normal = input  OpenPrompt AddFeed
      cmd (Key '/')  m Normal = input  OpenPrompt SearchPrompt
      cmd (Key '\t') m Normal = input  OpenPrompt SearchPrompt
      cmd _          m Normal = (Nothing, Normal)
      cmd (Key c)    m Input  = input  PutPrompt c
      cmd BS         m Input  = input  DelPrompt ()
      cmd Esc        m Input  = normal CancelPrompt ()
      cmd Enter      m Input  = normal ClosePrompt ()

  rec
    eCmd <- filterJust <$>
              collectE (uncurry cmd) Normal (snapshot (,) eEvent bModel)
         :: Reactive (Event ExCmd)

    let eResp :: Event ExResp
        eResp = executeAsyncIO $ fmap
          (\(ExCmd o p) -> ExResp o p <$> resp o p) eCmd
          where
          resp :: Op o -> Cmd o -> IO (Resp o)
          resp UpdateFeed  url  = do
            sync $ pushFeedback $ Downloading 1
            [f] <- downloadFeeds [url]
                     (sync $ pushFeedback FeedDownloaded)
            return $ Just f
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
          resp Quit          fs = throwTo tid $ SaveModel fs
          resp RemoveFeed    () = return ()

    let update :: Op o -> Cmd o -> Resp o -> Model -> Model
        update Move          d   ()  m = move d m
        update UpdateFeed    _   mf  m = case mf of
          Nothing -> m
          Just f  -> feedDownloaded f m
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
        update RemoveFeed    ()  () m = removeFeed m

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
