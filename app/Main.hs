--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import           CmdOptions           as Cmd (parse)
import           Control.Monad        (void)
import           Data.Map             (Map)
import           Data.Text            as T (Text, concat, unpack)
import qualified DummyAdventure       as Dummy (allPrepositions, allScenes,
                                                allTokens, allVerbs,
                                                defaultScene, gameIntro,
                                                startFlags, startInventory,
                                                startScene)
import HelpDetails as Help
    ( storyDirectory, getJsonFilePaths, processJsonFiles )
import           NarrativeGraph       (Flags, Inventory, Scene, SceneKey,
                                       makeNarrativeGraph)
import           NaturalLanguageLexer (Prepositions, Tokens, Verbs)
import qualified NightmareAdventure   as Nightmare (allPrepositions, allScenes,
                                                    allTokens, allVerbs,
                                                    defaultScene, gameIntro,
                                                    startFlags, startInventory,
                                                    startScene)
import           PrintUtils           (printHelp, printIntro)
import           System.Environment   as E (getArgs)
import           System.IO            (hFlush, stdout)
import           TextAdventureCore    (adventure)
import           TextReflow           (reflowPutStr)

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
            concatResults (Just (full, short, desc)) =
                T.concat [full, " (", short, ") -", desc, "\n"]
            concatResults Nothing =
                T.concat ["Failed to parse or extract names"]



displayHelp :: Text -> IO ()
displayHelp t = void $ Cmd.parse $ T.unpack t

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
