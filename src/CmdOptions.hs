module CmdOptions (parse, greet) where

import Options.Applicative

data AdventureOptions = AdventureOptions String
  -- { hello      :: String
  -- , quiet      :: Bool
  -- , enthusiasm :: Int }

adventure :: Parser AdventureOptions
adventure = AdventureOptions
      <$> strOption
          ( long "adventure"
          <> short 'a'
          <> metavar "NAME"
          <> help "Name of adventure to load." )

greet :: AdventureOptions -> IO ()
greet (AdventureOptions a ) = putStrLn $ "You chose: '" ++ a ++ "'." 

parse :: IO AdventureOptions
parse = execParser opts

opts :: ParserInfo AdventureOptions
opts = info (adventure <**> helper)
      ( fullDesc
      <> progDesc "Run the named text adventure."
      <> header "Haskell Adventure - a journey into fun!" )

