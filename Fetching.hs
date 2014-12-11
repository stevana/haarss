{-# LANGUAGE ScopedTypeVariables #-}

module Fetching where

import Data.Either
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Array.IO
import Text.Feed.Types (Feed)
import Text.Feed.Import (parseFeedString)

import Control.Exception
import Control.Lens
import Data.ByteString.Lens (unpackedChars)

import Network.Wreq
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.TLS (tlsManagerSettings)

------------------------------------------------------------------------

fetchFeed' :: String -> IO Feed
-- fetchFeed' url = head <$> fetchFeeds' [url]
fetchFeed' url = do
  mfeed <- openAsFeed' url
  case mfeed of
    Left err   -> error err
    Right feed -> return feed


type Result = (String, Either String Feed)

-- XXX: Deal with errors...
fetchFeeds' :: [String] -> TVar Int -> IO [Feed]
fetchFeeds' urls count = fmap (\result -> rights $ map snd result) $ fetchFeeds urls count

fetchFeeds :: [String] -> TVar Int -> IO [Result]
fetchFeeds urls count = do

  -- XXX: create this in main and pass it around so we can fromPoll it
  -- and display progress while updating?
  atomically $ writeTVar count $ length urls
  result <- newArray_ (1, length urls) :: IO (IOArray Int Result)
  forM_ (zip [1..] urls) $ \(idx, url) -> do
    forkIO $ fetch url result idx count

  waitForThreads count
  getElems result
  where
  waitForThreads :: TVar Int -> IO ()
  waitForThreads alive = atomically $ do
    n <- readTVar alive
    check (n == 0)

fetch :: String -> IOArray Int Result -> Int -> TVar Int -> IO ()
fetch url arr idx count = do
  result <- openAsFeed' url
  writeArray arr idx (url, result)
  atomically $ do
    n <- readTVar count
    writeTVar count $ n - 1

------------------------------------------------------------------------

openAsFeed' :: String -> IO (Either String Feed)
openAsFeed' url = do
  eDoc <- downloadURL url
  case eDoc of
    Left err  -> return $ Left $ show err
    Right doc -> case parseFeedString doc of
      Nothing   -> return $ Left "failed to parse feed"
      Just feed -> return $ Right feed

downloadURL :: String -> IO (Either HttpException String)
downloadURL uri = do

  let opts = defaults & redirects .~ 3
                      & manager   .~ Left tlsManagerSettings

  r <- getWith opts uri

    -- XXX: there might be other errors?
    `catch` \(e :: HttpException) -> throw e

  return $ Right $ r ^. responseBody . unpackedChars -- XXX: don't unpack
