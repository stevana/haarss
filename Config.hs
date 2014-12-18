{-# LANGUAGE TemplateHaskell #-}

module Config (Config, urls, browser, readConfig) where

import Control.Lens
import Data.Char (isSpace)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>))

------------------------------------------------------------------------
-- * Constants

haarssDir :: String
haarssDir = ".haarss"

config :: String
config = "config"

------------------------------------------------------------------------
-- * Types

-- Write config to file on exit? Lens. Parser for config files?
data Config = Config
  { _browser :: String
  , _urls    :: [String]  -- Should probably contain more things, like
                          -- nick name for feed, filter..

  -- , shortcuts :: M.Map Vty.Key Command
  -- , colours   :: M.Map Widget Colour

  -- , language :: [Lang] -- one for each feed to mine the body of the
                          -- text?!
  }
  deriving (Show, Read)

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
  { _browser = "firefox"
  , _urls    = [haarss]
  }
  where
  haarss   = "https://github.com/stevana/haarss/commits/master.atom"

------------------------------------------------------------------------
-- * Helpers

readConfig :: IO Config
readConfig = do

  home               <- getEnv "HOME"
  let configFilePath = home </> haarssDir </> config
  exists             <- doesFileExist configFilePath

  if not exists
    then do
      createDirectoryIfMissing False $ home </> haarssDir
      writeFile configFilePath $ show defaultConfig
      return defaultConfig
    else do
      str <- readFile $ home </> haarssDir </> config
      case readMaybe str of
        Nothing  -> error "readConfig: failed to parse config."
        Just cfg -> return cfg

  where
  -- XXX: Also used by readSavedModel, won't be the case if config has a
  -- proper parser...
  readMaybe :: Read a => String -> Maybe a
  readMaybe s = case reads s of
                [(x, rest)] | all isSpace rest -> Just x
                _                              -> Nothing
