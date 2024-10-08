module CmdOptions (parse, AdventureOptions(AdventureOptions)) where

import Options.Applicative

newtype AdventureOptions = AdventureOptions String
  -- { adventure      :: String }

choice :: Parser AdventureOptions
choice = AdventureOptions
      <$> strOption
          ( long "adventure"
          <> short 'a'
          <> metavar "NAME"
          <> help "Name of adventure to load." )

versionOption :: String ->  Parser (a -> a)
versionOption s = infoOption s (long "version" <> help "Show version")

parse :: IO AdventureOptions
parse = execParser opts

opts :: ParserInfo AdventureOptions
opts = info (choice <**> versionOption "1" <**> helper)
      ( fullDesc
      <> progDesc "Run the named text adventure."
      <> header "Haskell Adventure - a journey into fun!" )

