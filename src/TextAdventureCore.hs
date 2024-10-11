module TextAdventureCore (doAdventureLoop, updateAdventure, adventure) where

import           System.IO             (hFlush, stdout)

import           NarrativeGraph        (Flags, Inventory, NarrativeGraph,
                                        SceneKey, performInteraction,
                                        printInvalidInteractions,
                                        printSceneDescription)
import           NaturalLanguageParser (Sentence)
import           PrintUtils            (allColumnWidth, allDelimiters,
                                        parseInput)
import           TextReflow            (reflowPutStr)

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
