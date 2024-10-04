module CmdOptions where

import Options.Applicative

data Adventure = Adventure
  { adventure :: String }

adventureOpts :: Parser Adventure
adventureOpts = Adventure
      <$> strOption
          ( long "adventure"
         <> short 'a'
         <> metavar "ADVENTURE"
         <> help "Name of Adventure to choose" )