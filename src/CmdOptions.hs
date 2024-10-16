{-# LANGUAGE OverloadedStrings #-}
module CmdOptions (parse, AdventureOptions(AdventureOptions)) where

import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, infoOption,
                                      long, metavar, progDesc, progDescDoc,
                                      short, strOption, (<**>))
import           Prettyprinter       (Pretty (pretty), indent, vsep)

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

parse :: String -> IO AdventureOptions
parse = execParser . opts

opts :: String -> ParserInfo AdventureOptions
opts s = info (choice <**> versionOption s <**> helper)
      ( fullDesc
      <> progDesc "Run the named text adventure."
      <> header "Haskell Adventure - a journey into fun!"
      <> (progDescDoc . Just $ indent 2 $ vsep [pretty s])
      )

