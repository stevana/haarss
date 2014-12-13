{-# LANGUAGE TemplateHaskell #-}

module Config where

import Control.Lens

------------------------------------------------------------------------

-- Write config to file on exit? Lens. Parser for config files?
data Config = Config
  { _browser :: String
  , _urls    :: [String]  -- Should probably contain more things, like
                          -- nick name for feed, filter..

  -- , shortcuts :: M.Map Vty.Key Command
  -- , colours   :: M.Map Widget Colour
  -- , language  :: [Lang] -- one for each feed to mine the body of the text?!
  }
  deriving Read

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
  { _browser = "firefox"
  , _urls    = [slashdot, undeadly]
  }
  where
  undeadly = "http://undeadly.org/cgi?action=rss"
  slashdot = "http://rss.slashdot.org/Slashdot/slashdot"
