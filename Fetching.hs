module Fetching where

import Data.Either
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Array.IO
import Text.Feed.Types (Feed)
import Text.Feed.Import (parseFeedString)

import Network.HTTP
import Network.URI

------------------------------------------------------------------------


fetchFeed :: String -> TVar Int -> IO Result
fetchFeed url count = head <$> fetchFeeds [url] count

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
    count <- readTVar alive
    check (count == 0)

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
    Left err  -> return $ Left err
    Right doc -> case parseFeedString doc of
      Nothing   -> return $ Left "failed to parse feed"
      Just feed -> return $ Right feed

downloadURL :: String -> IO (Either String String)
downloadURL url =
  case parseURI url of
    Nothing  -> return $ Left "Bad url"
    Just uri -> do
      resp <- simpleHTTP $ request uri
      case resp of
        Left x  -> return $ Left ("Error connecting: " ++ show x)
        Right r -> case rspCode r of
          (2,_,_) -> return $ Right (rspBody r)
          -- A HTTP redirect
          (3,_,_) -> case findHeader HdrLocation r of
            Nothing  -> return $ Left (show r)
            Just url -> downloadURL url
          _       -> return $ Left (show r)

  where
  request uri = Request
    { rqURI     = uri
    , rqMethod  = GET
    , rqHeaders = []
    , rqBody    = ""
    }
