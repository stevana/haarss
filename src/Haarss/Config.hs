{-# LANGUAGE TemplateHaskell #-}

module Haarss.Config where

import           Control.Lens
import           Data.Char        (isSpace)
import           Data.ByteString  (ByteString)
import           Data.Text.Lens
import           System.Directory (createDirectoryIfMissing, doesFileExist,
                                   getAppUserDataDirectory)
import           System.FilePath  (takeDirectory, (</>))

import           Haarss.Feed.Annotated
import           Haarss.Feed.Feed

------------------------------------------------------------------------
-- * Types

data Config = Config
  { _browser :: String
  , _proxy   :: Maybe (ByteString, Int)  -- ^ Hostname and port.
  , _entries :: [(Maybe String, String)]
  }
  deriving (Show, Read)

makeLenses ''Config

------------------------------------------------------------------------

defaultConfig :: Config
defaultConfig = Config
  { _browser = "firefox"
  , _proxy   = Nothing
  , _entries = [ (Just "haarss github feed", haarss) ]
  }
  where
  haarss = "https://github.com/stevana/haarss/commits/master.atom"

------------------------------------------------------------------------

loadConfig :: IO Config
loadConfig = do
  configPath <- getAppUserDataDirectory $ "haarss" </> "config"
  exists     <- doesFileExist configPath

  if not exists
    then do
      createDirectoryIfMissing False $ takeDirectory configPath
      writeFile configPath $ show defaultConfig
      return defaultConfig
    else do
      str <- readFile configPath
      case readMaybe str of
        Nothing  -> error "loadConfig: failed to parse config."
        Just cfg -> return cfg

  where
  readMaybe :: Read a => String -> Maybe a
  readMaybe s = case reads s of
                [(x, rest)] | all isSpace rest -> Just x
                _                              -> Nothing

updateConfig :: Config -> [AnnFeed] -> IO ()
updateConfig cfg fs = do
  configPath <- getAppUserDataDirectory $ "haarss" </> "config"
  writeFile configPath $ ppConfig $ cfg & entries .~
    (flip map fs $ \f -> (f^.alias.traverse.unpacked.to Just,
                          f^.feed.feedHome.unpacked))

  where
  -- XXX: Use ansi-wl-pprint package?
  ppConfig :: Config -> String
  ppConfig c = unlines
    [ "Config"
    , "  { _browser = " ++ c^.browser.to show
    , "  , _proxy   = " ++ c^.proxy.to show
    , "  , _entries = "
    , "    " ++ ppList (c^.entries)
    , "  }"
    ]
    where
    ppList :: Show a => [a] -> String
    ppList []       = "[]"
    ppList (x : xs) = "[ " ++ show x ++ "\n" ++ go xs
      where
      go []       = "    ]"
      go (y : ys) = "    , " ++ show y ++ "\n" ++ go ys
