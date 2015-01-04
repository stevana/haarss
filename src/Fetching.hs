{-# LANGUAGE ScopedTypeVariables #-}

module Fetching where

import Control.Applicative
import Control.Monad
import Control.Concurrent.ParallelIO
import Control.Exception
import Control.Lens

import qualified Data.ByteString.Lazy as BS
import Data.Time
import qualified Data.Text as T

import Network.Wreq
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import System.Timeout

import Fetching.History
import Feed.Feed
import Feed.Annotated
import Feed.Parser

import Constants
import qualified Config

------------------------------------------------------------------------

download1 :: String -> IO () -> (BS.ByteString -> Either String a) ->
             IO (Maybe a, History)
download1 url callback parser = do
  time <- getCurrentTime
  let opts = defaults & redirects .~ 3
                      & manager   .~ Left tlsManagerSettings
  r <- do
    mr <- timeout (30 * 1000000) $ getWith opts url
    case mr of
      Nothing -> return (Nothing, Failure time TimeoutFailure)
      Just r  -> case r^.responseBody.to parser of
        Left err -> return (Nothing, Failure time $ ParseFailure err)
        Right x  -> return (Just x, Success time)
    `catches`
      [ Handler (\(e :: HttpException) ->
          return (Nothing, Failure time $
                           DownloadFailure $ simplifyHttpException e))
      , Handler (\(_ :: SomeException) ->
          return (Nothing, Failure time UnknownFailure))
      ]
  callback
  return r

download :: [String] -> IO () -> (BS.ByteString -> Either String a) ->
            IO [(Maybe a, History)]
download urls  callback parser = parallel $
  flip map urls $ \url -> download1 url callback parser

feedParser :: BS.ByteString -> Either String Feed
feedParser = bimap show id . parseFeed

downloadFeeds :: [String] -> IO () -> IO [AnnFeed]
downloadFeeds urls callback =
  map downloadToFeed . zip urls <$> download urls callback feedParser
  where
  downloadToFeed :: (String, (Maybe Feed, History)) -> AnnFeed
  downloadToFeed (_,   (Just f,  h)) = defAnnFeed f &
                                         history .~ [h]
  downloadToFeed (url, (Nothing, h)) = defAnnFeed f &
                                         history .~ [h]
    where
    f = newEmptyFeed AtomKind
      & feedTitle ?~ T.pack url

------------------------------------------------------------------------

downloadDebug :: [String] -> IO ()
downloadDebug urls = do
  dls <- download urls (return ()) feedParser
  forM_ (zip urls dls) $ \(url, (_ , h)) -> case h of
    Success _   -> putStrLn url
    Failure _ e -> putStrLn $ show e ++ ": " ++ url

runTest :: IO ()
runTest = do
  cfgPath <- getConfigPath
  cfg <- read <$> readFile cfgPath
  downloadDebug (cfg^.Config.urls)
