module CmdOptions (parse, AdventureOptions(AdventureOptions)) where

import Options.Applicative

data AdventureOptions = AdventureOptions String
  -- { adventure      :: String }

choice :: Parser AdventureOptions
choice = AdventureOptions
      <$> strOption
          ( long "adventure"
          <> short 'a'
          <> metavar "NAME"
          <> help "Name of adventure to load." )


parse :: IO AdventureOptions
parse = execParser opts

opts :: ParserInfo AdventureOptions
opts = info (choice <**> helper)
      ( fullDesc
      <> progDesc "Run the named text adventure."
      <> header "Haskell Adventure - a journey into fun!" )

