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

import Fetching.History
import Feeds (AnnFeed(..), defaultAnn, history, convert)

------------------------------------------------------------------------

download :: forall a. [String] -> IO () ->
            (BS.ByteString -> Either String a) -> IO [(Maybe a, History)]
download urls callback parser = do

  alive  <- newTVarIO $ length urls
  result <- newArray_ (1, length urls) :: IO (IOArray Int (Maybe a, History))

  forM_ (zip [1..] urls) $ \(idx, url) -> do
    forkIO (fetch idx url alive result)

  waitForThreads alive
  getElems result
  where

  waitForThreads :: TVar Int -> IO ()
  waitForThreads alive = atomically $ do
    n <- readTVar alive
    check (n == 0)

  fetch :: Int -> String -> TVar Int -> IOArray Int (Maybe a, History) -> IO ()
  fetch idx url alive result = do

    time <- getCurrentTime

    let write = writeArray result idx

    let opts = defaults & redirects .~ 3
                        & manager   .~ Left tlsManagerSettings

    do { mr <- timeout (30 * 1000000) $ getWith opts url

       ; case mr of
           Nothing -> write (Nothing, Failure time $ TimeoutFailure)
           Just r -> case r^.responseBody.to parser of
             Left err -> write (Nothing, Failure time $ ParseFailure err)
             Right x  -> write (Just x, Success time)

       } `catches`
           [ Handler (\(e :: HttpException) ->
               write (Nothing, Failure time $
                       DownloadFailure (simplifyHttpException e)))
           , Handler (\(_ :: SomeException) ->
               write (Nothing, Failure time UnknownFailure))
           ]

    callback
    atomically $ modifyTVar alive pred


-- XXX: Error handling, avoid unpacking bytestring
feedParser :: BS.ByteString -> Either String Feed
feedParser = maybe (Left "failed to parse feed") Right .
           parseFeedString . map (chr . fromEnum) . BS.unpack

downloadFeeds :: [String] -> IO () -> IO [AnnFeed]
downloadFeeds urls callback =
  map downloadToFeed . zip urls <$> download urls callback feedParser
  where
  downloadToFeed :: (String, (Maybe Feed, History)) -> AnnFeed
  downloadToFeed (_,   (Just f,  h)) = defaultAnn (convert f) &
                                         history .~ [h]
  downloadToFeed (url, (Nothing, h)) = defaultAnn (convert f) &
                                         history .~ [h]
    where
    f = withFeedTitle url $
        withFeedHome url $
        newFeed AtomKind

------------------------------------------------------------------------

{-
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
-}
