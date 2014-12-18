{-# LANGUAGE ScopedTypeVariables #-}

module Fetching where

import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception
import Control.Lens

import Data.Char (chr)
import qualified Data.ByteString.Lazy as BS
import Data.Array.IO

import Network.Wreq
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Text.Feed.Types (Feed)
import Text.Feed.Import (parseFeedString)

------------------------------------------------------------------------

download :: [String] -> IO () -> (BS.ByteString -> a) -> IO [a]
download urls callback parser = do

  alive  <- newTVarIO $ length urls
  result <- newArray_ (1, length urls) :: IO (IOArray Int a)

  forM_ (zip [1..] urls) $ \(idx, url) -> do
    forkIO $ fetch idx url parser alive result

  waitForThreads alive
  getElems result
  where
  fetch :: Int -> String -> (BS.ByteString -> a) -> TVar Int ->
           IOArray Int a -> IO ()
  fetch idx url parser alive result = do
    let opts = defaults & redirects .~ 3
                        & manager   .~ Left tlsManagerSettings

    r <- getWith opts url

      -- XXX: there might be other errors?
      -- `catch` \(e :: HttpException) -> throw e

    -- XXX: generalise callback?
    callback

    writeArray result idx $ r^.responseBody.to parser
    atomically $ modifyTVar alive pred

  waitForThreads :: TVar Int -> IO ()
  waitForThreads alive = atomically $ do
    n <- readTVar alive
    check (n == 0)

downloadFeeds :: [String] -> IO () -> IO [Feed]
downloadFeeds urls callback = download urls callback parser
  where
  -- XXX: Error handling, avoid unpacking bytestring
  parser :: BS.ByteString -> Feed
  parser = maybe (error "failed to parse feed") id .
             parseFeedString . map (chr . fromEnum) . BS.unpack
