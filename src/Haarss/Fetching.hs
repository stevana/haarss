{-# LANGUAGE ScopedTypeVariables #-}

module Haarss.Fetching where

import           Control.Monad
import           Control.Lens
import           Data.ByteString               (ByteString)
import qualified Data.Text                     as T
import           Data.Time
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
import           Network.Wreq

import           Haarss.Feed.Annotated
import qualified Haarss.Fetching.History       as H

import           Yeast.Feed
import           Yeast.Fetch

------------------------------------------------------------------------

download :: [String] -> IO () -> Maybe (ByteString, Int) -> IO [AnnFeed]
download urls callback mproxy = do
  time <- getCurrentTime
  let opts = defaults & redirects .~ 3
                      & manager   .~ Left tlsManagerSettings
                      & proxy     .~ uncurry httpProxy `fmap` mproxy

  efs <- fetchManyWith opts (const callback) urls

  forM (zip urls efs) $ \(url, ef) -> do
    case ef of
      Left e  -> return $
        defAnnFeed (emptyFeed AtomKind
                      & title    ?~ T.pack url
                      & feedHome ?~ T.pack url)
          & history .~ [H.Failure time $ simplifyError e]

      Right f -> return $
        defAnnFeed (f & feedHome ?~ T.pack url)
          & history .~ [H.Success time]

  where
  simplifyError :: FetchError -> H.FailureReason
  simplifyError TimeoutFailure      = H.TimeoutFailure
  simplifyError (DownloadFailure e) = H.DownloadFailure $
    H.simplifyHttpException e
  simplifyError (ParseFailure _)    = H.ParseFailure ""
  simplifyError (UnknownFailure _)  = H.UnknownFailure
