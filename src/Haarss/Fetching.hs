{-# LANGUAGE ScopedTypeVariables #-}

module Haarss.Fetching where

import           Control.Concurrent.ParallelIO
import           Control.Exception
import           Control.Lens
import qualified Data.Text                     as T
import           Data.Time
import           Network.HTTP.Client           (HttpException)
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
import           Network.Wreq
import           System.Timeout

import           Haarss.Feed.Annotated
import           Haarss.Feed.Feed
import           Haarss.Feed.Parser
import           Haarss.Fetching.History

------------------------------------------------------------------------

download1 :: String     -- ^ URL
          -> IO ()      -- ^ Callback
          -> UTCTime    -- ^ Time
          -> Options    -- ^ Client configuration
          -> IO AnnFeed
download1 url callback time opts = do
  callback
  mr <- timeout (3 * 1000000) $ getWith opts url
  case mr of
    Nothing -> return $ failFeed TimeoutFailure
    Just r  -> do
      case r^.responseBody.to parseFeed.to (bimap show id) of
        Left  err -> return $ failFeed $ ParseFailure err
        Right f   -> return $ defAnnFeed (f & feedHome .~ url)
                                 & history .~ [Success time]
  `catches`
    [ Handler (\(e :: HttpException) ->
        return $ failFeed $ DownloadFailure $ simplifyHttpException e)
    , Handler (\(_ :: SomeException) ->
        return $ failFeed UnknownFailure)
    ]
  where
  failFeed e = defAnnFeed (newEmptyFeed AtomKind
                             & feedTitle ?~ T.pack url
                             & feedHome  .~ url)
                 & history .~ [Failure time e]

download :: [String] -> IO () -> IO [AnnFeed]
download urls callback = do
  time <- getCurrentTime
  let opts = defaults & redirects .~ 3
                      & manager   .~ Left tlsManagerSettings

  parallel $ flip map urls $ \url -> download1 url callback time opts

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
