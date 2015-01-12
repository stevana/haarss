{-# LANGUAGE ScopedTypeVariables #-}

module Haarss.Fetching where

import           Control.Applicative
import           Control.Concurrent.ParallelIO
import           Control.Exception
import           Control.Lens
import           Control.Monad

import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import           Data.Time

import           Network.HTTP.Client           (HttpException)
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
import           Network.Wreq

import           System.Directory
import           System.FilePath
import           System.Timeout

import           Haarss.Config
import           Haarss.Feed.Annotated
import           Haarss.Feed.Feed
import           Haarss.Feed.Parser
import           Haarss.Fetching.History

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
download urls callback parser = parallel $
  flip map urls $ \url -> download1 url callback parser

feedParser :: BS.ByteString -> Either String Feed
feedParser = bimap show id . parseFeed

downloadFeeds :: [String] -> IO () -> IO [AnnFeed]
downloadFeeds urls callback =
  zipWith (curry downloadToFeed) urls <$> download urls callback feedParser
  where
  downloadToFeed :: (String, (Maybe Feed, History)) -> AnnFeed
  downloadToFeed (url, (Just f,  h)) = defAnnFeed f' &
                                         history .~ [h]
    where
    f' = f & feedHome .~ url
  downloadToFeed (url, (Nothing, h)) = defAnnFeed f &
                                         history .~ [h]
    where
    f = newEmptyFeed AtomKind
      & feedTitle ?~ T.pack url
      & feedHome  .~ url

------------------------------------------------------------------------
-- XXX: Debugging

{-
downloadDebug :: [String] -> IO ()
downloadDebug urls = do
  dls <- download urls (return ()) feedParser
  forM_ (zip urls dls) $ \(url, (_ , h)) -> case h of
    Success _   -> putStrLn url
    Failure _ e -> putStrLn $ show e ++ ": " ++ url

runTest :: IO ()
runTest = do
  cfgPath <- getAppUserDataDirectory $ "haarss" </> "config"
  cfg <- read <$> readFile cfgPath
  downloadDebug (cfg^..entries.traverse._2)
-}
