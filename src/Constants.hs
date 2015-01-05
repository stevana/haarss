module Constants (getConfigPath, getModelPath) where

import           System.Environment (getEnv)
import           System.FilePath    ((</>))

------------------------------------------------------------------------
-- * Constants

haarssDir :: FilePath
haarssDir = ".haarss"

configPath :: FilePath
configPath = haarssDir </> "config"

modelPath :: FilePath
modelPath = haarssDir </> "savedModel"

------------------------------------------------------------------------
-- * Helpers

homePrefixed :: FilePath -> IO FilePath
homePrefixed fp = do
  home <- getEnv "HOME"
  return $ home </> fp

getConfigPath :: IO FilePath
getConfigPath = homePrefixed configPath

getModelPath :: IO FilePath
getModelPath = homePrefixed modelPath
