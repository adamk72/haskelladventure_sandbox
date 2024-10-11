--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable
{-# LANGUAGE LambdaCase #-}

import           System.Environment as E (getArgs)
import           System.IO          (hFlush, stdout)

import           CmdOptions         (parse)
import           Data.Map           (Map)
import qualified DummyAdventure     as Dummy (allScenes, defaultScene,
                                              gameIntro, startFlags,
                                              startInventory, startScene)
import           NarrativeGraph     (Flags, Inventory, Scene, SceneKey,
                                     makeNarrativeGraph)
import qualified NightmareAdventure as Nightmare (allScenes, defaultScene,
                                                  gameIntro, startFlags,
                                                  startInventory, startScene)
import           PrintUtils         (allColumnWidth, allDelimiters, printHelp,
                                     printIntro)
import           TextAdventureCore  (adventure)
import           TextReflow         (reflowPutStr)

main :: IO ()
main =
    -- E.getArgs >>= print >>
    E.getArgs >>= \case
        ["-a", "Dummy Adventure"]       -> runDummy
        ["-a", "Dummy"]                 -> runDummy
        ["-a", "Nightmare Adventure"]   -> runNightmare
        ["-a", "Nightmare"]             -> runNightmare
        _                               -> displayHelp

displayHelp :: IO ()
displayHelp = readFile "stories.txt" >>= parse >>= mempty

runDummy :: IO ()
runDummy = runGame  Dummy.gameIntro
                    Dummy.defaultScene
                    Dummy.startScene
                    Dummy.startInventory
                    Dummy.startFlags
                    Dummy.allScenes

runNightmare :: IO ()
runNightmare = runGame  Nightmare.gameIntro
                        Nightmare.defaultScene
                        Nightmare.startScene
                        Nightmare.startInventory
                        Nightmare.startFlags
                        Nightmare.allScenes

runGame :: String ->
    NarrativeGraph.Scene ->
    String ->
    NarrativeGraph.Inventory ->
    NarrativeGraph.Flags ->
    (Data.Map.Map NarrativeGraph.SceneKey NarrativeGraph.Scene, [NarrativeGraph.SceneKey]) ->
    IO ()
runGame gameIntro defaultScene startScene startInventory startFlags allScenes =
    printIntro >>
    reflowPutStr allDelimiters allColumnWidth gameIntro >>
    putStr "\n" >>
    printHelp >>
    putStr "\n\n\n" >>
    hFlush stdout >>
    adventure (makeNarrativeGraph adventureScenes endScenes defaultScene) (Just (startScene, startInventory, startFlags)) >>
    return ()
        where (adventureScenes, endScenes) = allScenes

