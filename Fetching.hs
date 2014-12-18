{-# LANGUAGE ScopedTypeVariables #-}

module Fetching where

import Control.Applicative
import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception
import Control.Lens

import Data.Array.IO
import qualified Data.ByteString.Lazy as BS
import Data.Char (chr)
import Data.Time

import Network.Wreq
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import System.Timeout

import Text.Feed.Types (Feed)
import Text.Feed.Constructor
  (newFeed, FeedKind(AtomKind), withFeedTitle, withFeedHome)
import Text.Feed.Import (parseFeedString)

-- XXX: Debugging
import Constants
import Config

------------------------------------------------------------------------

data Download a
  = Success UTCTime a
  | Failure UTCTime FailureReason

data FailureReason
  = DownloadFailure HttpException
  | ParseFailure String
  | Timeout
  | Unknown SomeException
  deriving Show -- XXX: debugging

download :: forall a. [String] -> IO () ->
            (BS.ByteString -> Either String a) -> IO [Download a]
download urls callback parser = do

  alive  <- newTVarIO $ length urls
  result <- newArray_ (1, length urls) :: IO (IOArray Int (Download a))

  forM_ (zip [1..] urls) $ \(idx, url) -> do
    forkIO (fetch idx url alive result)

  waitForThreads alive
  getElems result
  where

  waitForThreads :: TVar Int -> IO ()
  waitForThreads alive = atomically $ do
    n <- readTVar alive
    check (n == 0)

  fetch :: Int -> String -> TVar Int -> IOArray Int (Download a) -> IO ()
  fetch idx url alive result = do

    time <- getCurrentTime

    let write = writeArray result idx

    let opts = defaults & redirects .~ 3
                        & manager   .~ Left tlsManagerSettings

    do { mr <- timeout (30 * 1000000) $ getWith opts url

       ; case mr of
           Nothing -> write $ Failure time $ Timeout
           Just r -> case r^.responseBody.to parser of
             Left err -> write $ Failure time $ ParseFailure err
             Right x  -> write $ Success time x

       } `catches`
           [ Handler (\(e :: HttpException) ->
               write $ Failure time $ DownloadFailure e)
           , Handler (\(e :: SomeException) ->
               write $ Failure time $ Unknown e)
           ]

    callback
    atomically $ modifyTVar alive pred


-- XXX: Error handling, avoid unpacking bytestring
feedParser :: BS.ByteString -> Either String Feed
feedParser = maybe (Left "failed to parse feed") Right .
           parseFeedString . map (chr . fromEnum) . BS.unpack

downloadFeeds :: [String] -> IO () -> IO [Feed]
downloadFeeds urls callback =
  map downloadToFeed . zip urls <$> download urls callback feedParser
  where
  -- XXX: Should be returning an annotated feed here?
  downloadToFeed :: (String, Download Feed) -> Feed
  downloadToFeed (_,   Success _ f) = f
  downloadToFeed (url, Failure _ _) =
    withFeedTitle ("! " ++ url) $
    withFeedHome url $
    newFeed AtomKind

------------------------------------------------------------------------

downloadDebug :: [String] -> IO ()
downloadDebug urls = do
  dls <- download urls (return ()) feedParser
  forM_ (zip urls dls) $ \(url, dl) -> case dl of
    Success _ _ -> putStrLn url
    Failure _ e -> putStrLn $ show e ++ " " ++ url

runTest :: IO ()
runTest = do
  cfgPath <- getConfigPath
  cfg <- read <$> readFile cfgPath
  downloadDebug (cfg^.urls)
