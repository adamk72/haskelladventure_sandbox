--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable

import System.IO

import NarrativeGraph hiding (sentences, endScenes)
import NaturalLanguageParser
import PrintUtils
import TextReflow

import DummyAdventure

doAdventureLoop :: NarrativeGraph -> SceneKey -> Inventory -> Flags -> Maybe [Sentence] -> IO (Maybe (SceneKey, Inventory, Flags))
doAdventureLoop _ _ _ _ Nothing = return Nothing -- End state of the game
doAdventureLoop narrativeGraph sceneKey inventory flags (Just []) = adventure narrativeGraph (Just (sceneKey, inventory, flags)) --Failed to parse any sentences
doAdventureLoop narrativeGraph sceneKey inventory flags (Just sentences) = performInteraction allDelimiters allColumnWidth narrativeGraph sceneKey inventory flags sentences >>=
    adventure narrativeGraph --Perform the adventure loop

updateAdventure :: NarrativeGraph -> Maybe (SceneKey, Inventory, Flags) -> IO (Maybe (SceneKey, Inventory, Flags))
updateAdventure _ Nothing = return Nothing
updateAdventure narrativeGraph (Just (sceneKey, inventory, flags))
    = putStr "\n> " >>
      hFlush stdout >>
      printInvalidInteractions narrativeGraph sceneKey >>
      getLine >>=
      parseInput inventory flags >>=
      (\state -> putStr "\n" >> hFlush stdout >> return state) >>=
      doAdventureLoop narrativeGraph sceneKey inventory flags

adventure :: NarrativeGraph -> Maybe (SceneKey, Inventory, Flags) -> IO (Maybe (SceneKey, Inventory, Flags))
adventure _ Nothing = reflowPutStr allDelimiters allColumnWidth "Game over. Thanks for playing!" >> hFlush stdout >> return Nothing
adventure narrativeGraph (Just (sceneKey, inventory, flags))
    = printSceneDescription allDelimiters allColumnWidth narrativeGraph (Just (sceneKey, inventory, flags)) >>=
      updateAdventure narrativeGraph

main :: IO ()
main = printIntro >>
       reflowPutStr allDelimiters allColumnWidth gameIntro >>
       putStr "\n" >>
       printHelp >>
       putStr "\n" >>
       putStr "\n" >>
       putStr "\n" >>
       hFlush stdout >>
       adventure (makeNarrativeGraph adventureScenes endScenes defaultScene) (Just (startScene, startInventory, startFlags)) >>
       return ()
           where (adventureScenes, endScenes) = allScenes
