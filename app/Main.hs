--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Use void" #-}


import           CmdOptions           as Cmd (parse)
import           Control.Monad        (forM_)
import           Data.Aeson           (FromJSON, Result (..), ToJSON,
                                       Value (..), decode, eitherDecode,
                                       fromJSON, (.:))
import           Data.Aeson.Types     (Parser, parseMaybe)
import qualified Data.ByteString.Lazy as B
import           Data.Map             (Map)
import           Data.Text            as T (Text, concat, intercalate, unpack)
import qualified DummyAdventure       as Dummy (allPrepositions, allScenes,
                                                allTokens, allVerbs,
                                                defaultScene, gameIntro,
                                                startFlags, startInventory,
                                                startScene)
import           GHC.Generics         (Generic)
import           NarrativeGraph       (Flags, Inventory, Scene, SceneKey,
                                       makeNarrativeGraph)
import           NaturalLanguageLexer (Prepositions, Tokens, Verbs)
import qualified NightmareAdventure   as Nightmare (allPrepositions, allScenes,
                                                    allTokens, allVerbs,
                                                    defaultScene, gameIntro,
                                                    startFlags, startInventory,
                                                    startScene)
import           PrintUtils           (printHelp, printIntro)
import           System.Directory
import           System.Environment   as E (getArgs)
import           System.FilePath      (takeExtension, (</>))
import           System.IO            (hFlush, stdout)
import           TextAdventureCore    (adventure)
import           TextReflow           (reflowPutStr)

data AdventureDetail =
  AdventureDetail { fullName    :: !T.Text
                  , shortName   :: !T.Text
                  , description :: !T.Text
                  } deriving (Show,Generic)

instance FromJSON AdventureDetail
instance ToJSON AdventureDetail

jsonFile :: FilePath
jsonFile = "stories/DummyAdventure.json"

storyDirectory :: FilePath
storyDirectory = "stories"


getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = runMainTest
    -- E.getArgs >>= print >>
    --FilePath -> IO [FilePath] >>= m [String]
    -- listDirectory storyDirectory >>= mapM_ print >>
    -- E.getArgs >>= \case
    --     ["-a", "Dummy Adventure"]       -> runDummy
    --     ["-a", "Dummy"]                 -> runDummy
    --     ["-a", "Nightmare Adventure"]   -> runNightmare
    --     ["-a", "Nightmare"]             -> runNightmare
    --     _                               -> displayHelp

runMainTest :: IO ()
runMainTest = do
    files <- listDirectory storyDirectory
    let jsonFiles = filter (\f -> takeExtension f == ".json") files

    putStrLn "Titles from JSON files:"
    forM_ jsonFiles $ \file -> do
        let fullPath = storyDirectory </> file
        content <- B.readFile fullPath
        case decode content of
            Nothing -> putStrLn $ "Error parsing JSON in file: " ++ file
            Just json -> case extractTitle json of
                Nothing    -> putStrLn $ "No title found in file: " ++ file
                Just title -> putStrLn $ "- " ++ unpack title

extractTitle :: Value -> Maybe Text
extractTitle = parseMaybe parseTitle

parseTitle :: Value -> Parser Text
parseTitle (Object v) = v .: "fullName"
parseTitle _          = fail "Expected an object"


displayHelp :: IO ()
displayHelp =
  (eitherDecode <$> getJSON) >>= \case
    Left err -> putStrLn err
    Right ps ->
      let names = T.unpack $ T.intercalate "\n" (map (\p -> T.concat [fullName p, " (", shortName p, ") -- ", description p]) ps)
      in Cmd.parse names >> return ()

runDummy :: IO ()
runDummy = runGame Dummy.allVerbs
                   Dummy.allPrepositions
                   Dummy.allTokens
                   Dummy.gameIntro
                   Dummy.defaultScene
                   Dummy.startScene
                   Dummy.startInventory
                   Dummy.startFlags
                   Dummy.allScenes

runNightmare :: IO ()
runNightmare = runGame Nightmare.allVerbs
                       Nightmare.allPrepositions
                       Nightmare.allTokens
                       Nightmare.gameIntro
                       Nightmare.defaultScene
                       Nightmare.startScene
                       Nightmare.startInventory
                       Nightmare.startFlags
                       Nightmare.allScenes

runGame :: Verbs -> Prepositions -> Tokens -> String ->
    NarrativeGraph.Scene ->
    String ->
    NarrativeGraph.Inventory ->
    NarrativeGraph.Flags ->
    (Data.Map.Map NarrativeGraph.SceneKey NarrativeGraph.Scene, [NarrativeGraph.SceneKey]) ->
    IO ()
runGame verbs prepositions tokens gameIntro defaultScene startScene startInventory startFlags allScenes =
    printIntro >>
    reflowPutStr gameIntro >>
    putStr "\n" >>
    printHelp >>
    putStr "\n\n\n" >>
    hFlush stdout >>
    adventure verbs prepositions tokens (makeNarrativeGraph adventureScenes endScenes defaultScene) (Just (startScene, startInventory, startFlags)) >>
    return ()
        where (adventureScenes, endScenes) = allScenes

