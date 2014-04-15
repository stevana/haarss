{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RecursiveDo, DeriveDataTypeable #-}

-- (New headlines), create a new feed with all unread items, where the
-- titles of the items are prefixed by the feed title
--   + should be easy to create, but how do we update isRead for original feeds?

-- Scroll long text

-- Encoding... (don't bother with this until we do our own parsing?!)

-- search... see filter example:
--  https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana-threepenny/src/CRUD.hs

-- 'R', 'R' results in empty feed... Problem is that parser doesn't set
-- feedHome correctly! Fix parser? (using config meanwhile)

-- https... see conduit-http? pipes? also use conduit/pipes for fetching/parsing?

-- Store last modified in AnnFeed, only fetch the body if needed?

-- Extract content of an article using ML:
--   http://newspaper.readthedocs.org/en/latest/
--   https://github.com/codelucas/newspaper

-- Configurable keys? Can we do everything inside Reader? Or just send
-- in a map to setupNetwork?!

-- cmdargs package to handle command line stuff? Like what?

-- use zipper from control.lens?!
-- Use binary package to save/restore the model? (if we need the speed?)

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

import Prelude hiding (catch)

import Data.Typeable
import Data.Maybe
import Data.Either
import Data.List (genericLength)
import Data.Monoid

import System.IO
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

import Control.Lens hiding (view)
import qualified Data.Text as T
import Data.Text.Lens
import Control.Exception
import Control.Monad
import Data.Char
import qualified Data.Map as M
import System.Exit
import System.Process

import qualified Text.Feed.Types as Feed
import qualified Text.Feed.Query as Feed

import qualified Graphics.Vty as Vty

import FRP.Sodium
import FRP.Sodium.IO

import Model
import View
import Fetching
import Feeds

------------------------------------------------------------------------

-- Write config to file on exit? Lens. Parser for config files?
data Config = Config
  { _browser :: String
  , _urls    :: [String]  -- Should probably contain more things, like
                          -- nick name for feed, filter..

  -- , shortcuts :: M.Map Vty.Key Command
  -- , colours   :: M.Map Widget Colour
  -- , language  :: [Lang] -- one for each feed to mine the body of the text?!
  }
  deriving Read

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
  { _browser = "firefox"
  , _urls    = [slashdot, undeadly]
  }
  where
  undeadly = "http://undeadly.org/cgi?action=rss"
  slashdot = "http://rss.slashdot.org/Slashdot/slashdot"

data Command = Move Dir | Redraw | Output String | UpdateFeed | UpdateFeeds
  | OpenUrl | MarkAllAsRead | ToggleReadStatus
  deriving (Eq, Show)

------------------------------------------------------------------------

whenE = flip gate

unlessE :: Behavior Bool -> Event a -> Event a
unlessE b f = whenE (not <$> b) f

key       = modKey []
modKey ms = flip Vty.EvKey ms . Vty.KASCII
enter     = Vty.EvKey Vty.KEnter []

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                [(x, rest)] | all isSpace rest -> Just x
                _                              -> Nothing

------------------------------------------------------------------------

data SaveModel = SaveModel Model
  deriving (Show, Typeable)

instance Exception SaveModel where

------------------------------------------------------------------------

main :: IO ()
main = do
  mconfig <- readMaybe <$> readFile "haarss.config"
  let config = maybe defaultConfig id mconfig

  vty <- Vty.mkVty
  (eEvent, pushEvent) <- sync newEvent

  count <- newTVarIO 0
  size <- Vty.display_bounds $ Vty.terminal vty

  -- XXX: clean this up...
  mmodel <- join . fmap readMaybe <$> safeReadFile "haarss.save"
  let iniModel = maybe (initialModel (toInteger $ Vty.region_height size)) id mmodel

  sync $ setupReactive config vty iniModel count eEvent

  forever (Vty.next_event vty >>= sync . pushEvent)
    `catches` [ Handler (\(SaveModel model) -> do
                  Vty.shutdown vty
                  writeFile "haarss.save" $ show model
                  exitSuccess)
              , Handler (\e               -> do
                  Vty.shutdown vty
                  failure (show (e :: SomeException)))
              ]
              where
              failure err = do
                putStrLn $ "Unexpected error: " ++ err
                exitFailure

safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile fp = do
  s <- readFile fp `catch` (\(e :: SomeException) -> return "")
  return $ if null s then Nothing else Just s

------------------------------------------------------------------------

setupReactive :: Config -> Vty.Vty -> Model -> TVar Int ->
                 Event Vty.Event -> Reactive ()
setupReactive config vty iniModel count eEvent = do

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

  -- XXX: Used to update the statusbar, better name?.
  (eTick, pushTick) <- newEvent

  let eFeeds :: Event [AnnFeed]
      eFeeds = executeAsyncIO $ const io <$> filterE (== UpdateFeeds) eCommand
        where
        io :: IO [AnnFeed]
        io = do
          tid <- forkIO $ forever $ do
            sync $ pushTick ()
            threadDelay 100000
          feeds <- fetchFeeds' (config^.urls) count
          killThread tid
          return $ map (defaultAnn . convert) feeds

  rec
    let eFeed :: Event AnnFeed
        eFeed = executeAsyncIO $ snapshot (\_ model -> io (getFeedUrl model))
                                          (filterE (== UpdateFeed) eCommand)
                                          bModel
          where
          getFeedUrl :: Model -> String
          getFeedUrl (Model fs _ _) = config^.urls^?! ix (fs^.prev.to length)

          io :: String -> IO AnnFeed
          io url = do
            tid <- forkIO $ forever $ do
              sync $ pushTick ()
              threadDelay 100000
            atomically $ writeTVar count 1
            feed <- fetchFeed' url
            atomically $ writeTVar count 0
            killThread tid
            return $ defaultAnn $ convert feed


    bModel <- let cmdSem :: Command -> (Model -> Model) -> Event (Model -> Model)
                  cmdSem cmd change = change <$ filterE (== cmd) eCommand

              in accum iniModel $ mconcat
                   [ cmdSem (Move Top)  $ move Top
                   , cmdSem (Move Up)   $ move Up
                   , cmdSem (Move Down) $ move Down
                   , cmdSem (Move Bot)  $ move Bot
                   , cmdSem (Move In)   $ move In
                   , cmdSem (Move Out)  $ \model -> if isFeedsView model
                                                    then throw $ SaveModel model
                                                    else move Out model
                   , cmdSem Redraw id

                   , cmdSem ToggleReadStatus $
                       viewing._ItemsView._1.curr.isRead %~ not

                   , cmdSem MarkAllAsRead $ \model ->
                       if isFeedsView model
                       then model & feeds.traverse.feedItems.traverse.isRead .~ True
                       else model & viewing._ItemsView._1   .traverse.isRead .~ True

                   , (\feed  model ->
                       model & feeds.curr %~ flip Feeds.merge feed) <$> eFeed

                   -- XXX: this loses the current position (maybe not bad?)
                   , (\fs model -> model & feeds .~ makeZip
                         (mergeFeeds (closeZip (model^.feeds)) fs)) <$> eFeeds
                         -- mergeFeeds (closeZip $ model^.feeds) fs) <$> eFeeds

                         -- makeModel feeds (model^.info.maxRows) <$> eFeeds

                   , id <$ eTick
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
            rawSystem (config^.browser) [url]
            return ()

        getItemUrl :: Model -> Maybe String
        getItemUrl (Model _ _ (ItemsView items _)) =
          Just $ T.unpack $ items^.curr.item.itemLink
        getItemUrl _ = Nothing


  listen (value bModel) (\model -> view vty count (model, ""))
  listen eOpenUrl       id

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

  reactimate $ view vty status count <$> eModelBuffer

-}


