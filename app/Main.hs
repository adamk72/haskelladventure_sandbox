--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use void" #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import           CmdOptions           as Cmd (parse)
import           Control.Monad        (void)
import           Data.Aeson           (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy as B
import           Data.Map             (Map)
import           Data.Text            as T (Text, concat, pack, unpack)
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

storyDirectory :: FilePath
storyDirectory = "stories"

main :: IO ()
main =
    do
    jsonPaths <- getJsonFilePaths storyDirectory
    results <- processJsonFiles jsonPaths
    E.getArgs >>= \case
        ["-a", "Dummy Adventure"]       -> runDummy
        ["-a", "Dummy"]                 -> runDummy
        ["-a", "Nightmare Adventure"]   -> runNightmare
        ["-a", "Nightmare"]             -> runNightmare
        _                               -> displayHelp  (foldMap concatResults results)
        where
            concatResults (file, Just (_first, _last)) =
                T.concat ["File: ", T.pack file, ", Name: ", _first, " ", _last]
            concatResults (file, Nothing) =
                T.concat ["File: ", T.pack file, ", Failed to parse or extract names"]


displayHelp :: Text -> IO ()
displayHelp t = void $ Cmd.parse $ T.unpack t

getJsonFilePaths :: FilePath -> IO [FilePath]
getJsonFilePaths dir = do
    allFiles <- listDirectory dir
    let jsonFiles = filter (\f -> takeExtension f == ".json") allFiles
    return $ map (dir </>) jsonFiles

readJsonFile :: FilePath -> IO (Maybe AdventureDetail)
readJsonFile filePath = do
    contents <- B.readFile filePath
    return $ decode contents

processJsonFiles :: [FilePath] -> IO [(FilePath, Maybe (T.Text, T.Text))]
processJsonFiles = mapM processFile
  where
    processFile file = do
      mAdventure <- readJsonFile file
      return (file, extractNames mAdventure)

    extractNames (Just adv) = Just (fullName adv, shortName adv)
    extractNames Nothing    = Nothing

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