{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RecursiveDo,
             DeriveDataTypeable #-}

-- (New headlines), create a new feed with all unread items, where the
-- titles of the items are prefixed by the feed title
--   + should be easy to create, but how do we update isRead for original feeds?

-- Scroll long text

-- search... see filter example:
--  https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana-threepenny/src/CRUD.hs

-- Extract content of an article using ML:
--   http://newspaper.readthedocs.org/en/latest/
--   https://github.com/codelucas/newspaper

-- Configurable keys? Can we do everything inside Reader? Or just send
-- in a map to setupNetwork?!

-- cmdargs package to handle command line stuff? Like what?

-- use zipper from control.lens?!

------------------------------------------------------------------------

-- Sodium vs banana

-- * whenE is better than gate (unlessE)
-- * Sodium needs non-gui examples
-- * (a)syncIO stuff is very good in sodium
-- * snapshot makes more sense than apply.
-- * monad instance rather than union (mappend) and unions (mconcat)

-- * network part and pointing out that fromPoll/addHandler are for
--   input and reactimate for output, is very helpful.

-- * value is useful, e.g. view model

-- * listen a bit unintuitive name? Maybe just me, because i use it to print?

-- * consider making a clear distinction between primitive operations
-- and derived operations (separate module with all kinds of useful stuff?)

-- * fromPoll in sodium?

------------------------------------------------------------------------

module Main where

import Data.Typeable
import Data.Monoid
import qualified Data.ByteString as BS
import Data.Serialize

import Control.Applicative
import Control.Lens hiding (view)
import Control.Exception
import Control.Monad
import System.Exit
import System.Process

import qualified Graphics.Vty as Vty
import Graphics.Vty.Prelude

import FRP.Sodium
import FRP.Sodium.IO

import Constants
import Config
import Model
import View
import Fetching
import Feeds

------------------------------------------------------------------------

data Command = Move Dir | Redraw | Output String | UpdateFeed | UpdateFeeds
  | OpenUrl | MarkAllAsRead | ToggleReadStatus
  deriving (Eq, Show)

------------------------------------------------------------------------

whenE :: forall a. Behavior Bool -> Event a -> Event a
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

  cfg <- readConfig

  vty  <- Vty.mkVty =<< Vty.standardIOConfig
  size <- Vty.displayBounds $ Vty.outputIface vty
  (eEvent, pushEvent) <- sync newEvent

  model <- setHeight (regionHeight size) <$> readSavedModel cfg

  modelPath <- getModelPath

  sync $ setupReactive cfg vty model eEvent

  forever (Vty.nextEvent vty >>= sync . pushEvent)
    `catches` [ Handler (\(SaveModel model') -> do
                  Vty.shutdown vty
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

  let eCommand :: Event Command
      eCommand = mconcat
        [ bindKey 'K' $ Move Top
        , bindKey 'k' $ Move Up
        , bindKey 'j' $ Move Down
        , bindKey 'J' $ Move Bot
        , bindKey 'l' $ Move In
        , bindKey 'q' $ Move Out
        , bindKey 'R' $ UpdateFeed
        , bindKey 'r' $ UpdateFeeds
        , bindKey 'o' $ OpenUrl
        , bindKey 'm' $ MarkAllAsRead
        , bindKey 'M' $ ToggleReadStatus
        , Redraw <$ filterE (== modKey [Vty.MCtrl] 'l') (whenE bMode eEvent)

        -- XXX: redraw shouldn't be here. it needs to update the model with the new maxRows.

        , Redraw <$ filterE isResize eEvent
        ]
        where
        bindKey :: Char -> Command -> Event Command
        bindKey char cmd = cmd <$ filterE (== key char) (whenE bMode eEvent)

        isResize (Vty.EvResize _ _) = True
        isResize _                  = False

  (eFeedDownloaded, pushFeedDownloaded) <- newEvent

  let eFeeds :: Event [AnnFeed]
      eFeeds = executeAsyncIO $ const io <$> filterE (== UpdateFeeds) eCommand
        where
        io :: IO [AnnFeed]
        io = downloadFeeds (config^.urls) (sync (pushFeedDownloaded ()))

  rec
    let eFeed :: Event AnnFeed
        eFeed = executeAsyncIO $ snapshot (\_ model -> io (getFeedUrl model))
                                          (filterE (== UpdateFeed) eCommand)
                                          bModel
          where
          getFeedUrl :: Model -> String
          getFeedUrl m = config^.urls^?! ix (m^.browsing.feeds.prev.to length)

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
                   , cmdSem Redraw id

                   , cmdSem ToggleReadStatus $
                       browsing._TheItems._2.curr.isRead %~ not

                   , cmdSem MarkAllAsRead $ \model ->
                       if browsingFeeds model
                       then model & browsing.feeds.traverse.feed.feedItems.traverse.isRead .~ True
                       else model & browsing._TheItems._2   .traverse.isRead .~ True

                   , cmdSem UpdateFeed $ \model ->
                       model & downloading .~ 1

                   , cmdSem UpdateFeeds $ \model ->
                       model & downloading .~ config^.urls.to length

                   , (\f model ->
                       model & browsing.feeds.curr %~ flip Feeds.merge f) <$> eFeed

                   -- XXX: this loses the current position (maybe not bad?)
                   , (\fs model -> model & browsing.feeds .~ makeZip
                         (mergeFeeds (closeZip (model^.browsing.feeds)) fs)) <$> eFeeds
                         -- mergeFeeds (closeZip $ model^.feeds) fs) <$> eFeeds

                         -- makeModel feeds (model^.info.maxRows) <$> eFeeds

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
            _ <- createProcess (proc (config^.browser) [url])
                   { std_err = CreatePipe }
            return ()

  _ <- listen (value bModel) (\model -> view vty (model, ""))
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
