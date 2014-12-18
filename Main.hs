{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RecursiveDo, DeriveDataTypeable #-}

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

import Data.Typeable
import Data.Monoid

import Control.Applicative
import Control.Lens hiding (view)
import qualified Data.Text as T
import Control.Exception
import Control.Monad
import Data.Char
import System.Exit
import System.Process

import qualified Graphics.Vty as Vty

import FRP.Sodium
import FRP.Sodium.IO

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
modKey ms = flip Vty.EvKey ms . Vty.KASCII

enter :: Vty.Event
enter = Vty.EvKey Vty.KEnter []

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

  size <- Vty.display_bounds $ Vty.terminal vty

  -- XXX: clean this up...
  mmodel <- join . fmap readMaybe <$> safeReadFile "haarss.save"
  let iniModel = maybe (initialModel config (toInteger $ Vty.region_height size)) id mmodel

  sync $ setupReactive config vty iniModel eEvent

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
  s <- readFile fp `catch` (\(_ :: SomeException) -> return "")
  return $ if null s then Nothing else Just s

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
        io = do
          fs <- downloadFeeds (config^.urls) (sync (pushFeedDownloaded ()))
          return $ map (defaultAnn . convert) fs

  rec
    let eFeed :: Event AnnFeed
        eFeed = executeAsyncIO $ snapshot (\_ model -> io (getFeedUrl model))
                                          (filterE (== UpdateFeed) eCommand)
                                          bModel
          where
          getFeedUrl :: Model -> String
          getFeedUrl m = config^.urls^?! ix (m^.feeds.prev.to length)

          io :: String -> IO AnnFeed
          io url = do
            [feed] <- downloadFeeds [url] (sync (pushFeedDownloaded ()))
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

                   , cmdSem UpdateFeed $ \model ->
                       model & downloading .~ 1

                   , cmdSem UpdateFeeds $ \model ->
                       model & downloading .~ config^.urls.to length

                   , (\feed model ->
                       model & feeds.curr %~ flip Feeds.merge feed) <$> eFeed

                   -- XXX: this loses the current position (maybe not bad?)
                   , (\fs model -> model & feeds .~ makeZip
                         (mergeFeeds (closeZip (model^.feeds)) fs)) <$> eFeeds
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
            _ <- rawSystem (config^.browser) [url]
            return ()

        getItemUrl :: Model -> Maybe String
        getItemUrl (Model _ _ (ItemsView is _) _) =
          Just $ T.unpack $ is^.curr.item.itemLink
        getItemUrl _ = Nothing


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
