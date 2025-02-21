--NarrativeGraph.hs
--Copyright Laurence Emms 2018
--Module for representing a narrative graph in a text adventure
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module NarrativeGraph (SceneKey,
                       Flags(..),
                       Inventory(..),
                       StateChange(..),
                       NarrativeCondition(..),
                       ConditionalDescription(..),
                       ConditionalAction(..),
                       Interaction(..),
                       Scene(..),
                       NarrativeGraph(..),
                       makeNarrativeGraph,
                       evaluateCondition,
                       printConditionalDescription,
                       printSceneDescription,
                       printInvalidInteractions,
                       performInteraction) where

import qualified Control.Monad
import           Data.List             (find)
import qualified Data.Map
import           NaturalLanguageParser
import           System.IO
import           TextReflow

type SceneKey = String
newtype Flags = Flags [String] deriving (Show, Eq)
newtype Inventory = Inventory [String] deriving (Show, Eq)
data StateChange = AddToInventory String |
                   RemoveFromInventory String |
                   SetFlag String |
                   RemoveFlag String |
                   SceneChange SceneKey deriving (Show, Eq)

data NarrativeCondition = InInventory String | --Inventory has an item
                          FlagSet String | --Flag is set
                          SceneIs String | --Current scene is X
                          CTrue | --Always true
                          CFalse | --Always false
                          CNot NarrativeCondition |
                          COr NarrativeCondition NarrativeCondition |
                          CAnd NarrativeCondition NarrativeCondition deriving (Show, Eq)

newtype ConditionalDescription = ConditionalDescription [(
    NarrativeCondition,
    String,
    [StateChange])]
    deriving (Show, Eq)

data ConditionalAction = ConditionalAction {
    condition :: NarrativeCondition, --Condition under which action occurs
    conditionalDescription :: ConditionalDescription, --Description of action
    stateChanges :: [StateChange]}
    deriving (Show, Eq) --State changes to make

data Interaction = Interaction {
    sentences          :: [Sentence],
    conditionalActions :: [ConditionalAction]}
    deriving (Show, Eq)

data Scene = Scene {
    sceneDescription :: ConditionalDescription,
    interactions     :: [Interaction]}
    deriving (Show, Eq)

--By definition the first node in the narrative graph is the starting scene of the game
data NarrativeGraph = NarrativeGraph {
    nodes :: Data.Map.Map SceneKey Scene,
    endScenes :: [SceneKey],
    graphDefaultScene :: Scene}
    deriving (Show, Eq)

--Takes a list of scenes and returns a starting index and a NarrativeGraph
makeNarrativeGraph :: Data.Map.Map SceneKey Scene -> [SceneKey] -> Scene -> NarrativeGraph
makeNarrativeGraph scenes scenesEndScenes defaultScene
    = NarrativeGraph {nodes = scenes,
                      endScenes = scenesEndScenes,
                      graphDefaultScene = defaultScene}

evaluateCondition :: NarrativeCondition -> SceneKey -> Inventory -> Flags -> Bool
evaluateCondition CTrue _ _ _ = True
evaluateCondition CFalse _ _ _ = False
evaluateCondition (FlagSet flag) _ _ (Flags flags) = flag `elem` flags
evaluateCondition (SceneIs scene) currentScene _ _ = scene == currentScene
evaluateCondition (InInventory object) _ (Inventory inventory) _ = object `elem` inventory
evaluateCondition (CNot condition) scene inventory flags = not (evaluateCondition condition scene inventory flags)
evaluateCondition (COr condition0 condition1) scene inventory flags = evaluateCondition condition0 scene inventory flags || evaluateCondition condition1 scene inventory flags
evaluateCondition (CAnd condition0 condition1) scene inventory flags = evaluateCondition condition0 scene inventory flags && evaluateCondition condition1 scene inventory flags

--Print a conditional description by evaluating which conditions are true, concatenating the description, and printing it with reflowPutStrs
printConditionalDescription :: [SceneKey] -> ConditionalDescription -> [String] -> Maybe (SceneKey, Inventory, Flags) -> IO (Maybe (SceneKey, Inventory, Flags))
printConditionalDescription _ (ConditionalDescription []) linesToPrint Nothing
    = reflowPutStrs (reverse linesToPrint) >> putStr "\n" >> return Nothing --Game reached an end state
printConditionalDescription _ (ConditionalDescription []) linesToPrint (Just (sceneKey, inventory, flags))
    = reflowPutStrs (reverse linesToPrint) >> putStr "\n" >> hFlush stdout >> return (Just (sceneKey, inventory, flags)) --No more descriptions to print
printConditionalDescription _ (ConditionalDescription ((_, _, _) : remainingDescriptions)) linesToPrint Nothing
    = reflowPutStrs (reverse linesToPrint) >> putStr "\n" >> return Nothing --Game reached an end state
printConditionalDescription endScenes
                            (ConditionalDescription ((condition, subDescription, stateChanges) : remainingDescriptions)) linesToPrint (Just (sceneKey, inventory, flags))
    | evaluateCondition condition sceneKey inventory flags =
          stateChange (Data.List.find (\case (SceneChange _) -> True
                                             _ -> False) stateChanges)
                       endScenes
                       stateChanges
                       (Just (sceneKey, inventory, flags)) >>= --This conditional description passed all of the preconditions, check whether we need to transition to a new state
          printConditionalDescription endScenes (ConditionalDescription remainingDescriptions) ((subDescription ++ " ") : linesToPrint) --Condition is true, add sub-description to print
    | otherwise
        = printConditionalDescription endScenes (ConditionalDescription remainingDescriptions) linesToPrint (Just (sceneKey, inventory, flags))

printSceneDescription :: NarrativeGraph -> Maybe (SceneKey, Inventory, Flags) -> IO (Maybe (SceneKey, Inventory, Flags))
printSceneDescription (NarrativeGraph {nodes = graphNodes, endScenes = graphEndScenes}) Nothing
    = return Nothing
printSceneDescription (NarrativeGraph {nodes = graphNodes, endScenes = graphEndScenes}) (Just (sceneKey, inventory, flags))
    = let scene = Data.Map.lookup sceneKey graphNodes
      in case scene of
         Nothing -> putStrLn (sceneKey ++ " is not a valid scene") >> return Nothing
         Just (Scene {sceneDescription = thisSceneDescription,
                      interactions = _}) -> printConditionalDescription graphEndScenes thisSceneDescription [] (Just (sceneKey, inventory, flags))

updateFlags :: Flags -> [StateChange] -> Flags
updateFlags (Flags flags) [] = Flags flags
updateFlags (Flags flags) ((RemoveFlag flag) : remainingChanges) = updateFlags (Flags (filter (/= flag) flags)) remainingChanges
updateFlags (Flags flags) ((SetFlag flag) : remainingChanges)
    | flag `elem` flags = updateFlags (Flags flags) remainingChanges
    | otherwise = updateFlags (Flags (flag : flags)) remainingChanges
updateFlags (Flags flags) (_ : remainingChanges) = updateFlags (Flags flags) remainingChanges

updateInventory :: Inventory -> [StateChange] -> Inventory
updateInventory (Inventory inventory) [] = Inventory inventory
updateInventory (Inventory inventory) ((RemoveFromInventory object) : remainingChanges) = updateInventory (Inventory (filter (/= object) inventory)) remainingChanges
updateInventory (Inventory inventory) ((AddToInventory object) : remainingChanges)
    | object `elem` inventory = updateInventory (Inventory inventory) remainingChanges
    | otherwise = updateInventory (Inventory (object : inventory)) remainingChanges
updateInventory (Inventory inventory) (_ : remainingChanges) = updateInventory (Inventory inventory) remainingChanges

--State change takes the next scene index, the end scene index list, the current scene index, the inventory and flags, and a conditional action
--State change evaluates to the next state of the game
stateChange :: Maybe StateChange -> [SceneKey] -> [StateChange] -> Maybe (SceneKey, Inventory, Flags) -> IO (Maybe (SceneKey, Inventory, Flags))
stateChange Nothing _  stateChanges Nothing
    = return Nothing
stateChange _ endScenes stateChanges Nothing
    = return Nothing
stateChange Nothing _  stateChanges (Just (sceneKey, inventory, flags))
    = return (Just (sceneKey,
                    updateInventory inventory stateChanges,
                    updateFlags flags stateChanges)) --If there is no scene transition, return to the current scene with updated inventory and flags
stateChange (Just (SceneChange nextScene)) endScenes stateChanges (Just (sceneKey, inventory, flags))
    = if nextScene `elem` endScenes
      then getChar >> return Nothing --This is an end state for the game
      else return (Just (nextScene,
                         updateInventory inventory stateChanges,
                         updateFlags flags stateChanges)) --Transition to the next scene with updated inventory and flags

--Update game state takes an interaction description, fail string, next scene index, end scene index, current scene index, inventory, and flags
--Update game state Scene transition evaluates to the next state of the game
updateGameState :: [SceneKey] -> SceneKey -> Inventory -> Flags -> ConditionalAction -> IO (Maybe (SceneKey, Inventory, Flags))
updateGameState endScenes currentSceneKey inventory flags conditionalAction@(ConditionalAction {conditionalDescription = thisConditionalDescription, stateChanges = thisStateChanges})
    = printConditionalDescription endScenes thisConditionalDescription [] (Just (currentSceneKey, inventory, flags)) >>=
      stateChange (Data.List.find (\case
                                    (SceneChange _) -> True
                                    _               -> False) thisStateChanges)
                   endScenes
                   thisStateChanges --This conditional action passed all of the preconditions, check whether we need to transition to a new scene

--Perform the interaction and return a tuple of (new scene index, new inventory, new flags)
performConditionalActions :: SceneKey -> [SceneKey] -> Inventory -> Flags -> Maybe Interaction -> Maybe Interaction -> IO (Maybe (SceneKey, Inventory, Flags))
performConditionalActions  currentSceneKey _ inventory flags Nothing Nothing
    = putStr "That does nothing." >>
      hFlush stdout >>
      return (Just (currentSceneKey, inventory, flags)) --If there are no valid interactions actions but the sentence was valid, just return to the current state
performConditionalActions currentSceneKey
                          endScenes
                          inventory
                          flags
                          (Just (Interaction {sentences = _,
                                              conditionalActions = []}))
                          defaultSceneInteractions --There are no remaining conditional actions for the current scene
    = performConditionalActions currentSceneKey endScenes inventory flags Nothing defaultSceneInteractions --All current scene conditional actions were exhausted, try default scene interactions
performConditionalActions currentSceneKey
                          endScenes
                          inventory
                          flags
                          (Just (Interaction {sentences = thisSentences,
                                              conditionalActions = (conditionalAction@(ConditionalAction {condition = thisCondition}) : remainingConditionalActions)}))
                          defaultSceneInteractions -- Ignore default scene interactions if there are still current scene interactions
    | evaluateCondition thisCondition currentSceneKey inventory flags = updateGameState endScenes currentSceneKey inventory flags conditionalAction --The condition for the action passed, update the game state
    | otherwise = performConditionalActions currentSceneKey endScenes inventory flags
                                            (Just (Interaction {sentences = thisSentences,
                                                                conditionalActions = remainingConditionalActions})) defaultSceneInteractions --The condition for the action failed, attempt other actions
performConditionalActions currentSceneKey
                          endScenes
                          inventory
                          flags
                          Nothing --The current scene failed to have any interactions
                          (Just (Interaction {sentences = _,
                                              conditionalActions = []}))
    = performConditionalActions currentSceneKey endScenes inventory flags Nothing Nothing --All possible conditional actions are exhausted
performConditionalActions currentSceneKey
                          endScenes
                          inventory
                          flags
                          Nothing --The current scene failed to have any interactions
                          (Just (Interaction {sentences = thisSentences,
                                              conditionalActions = (conditionalAction@(ConditionalAction {condition = thisCondition}) : remainingConditionalActions)}))
    | evaluateCondition thisCondition currentSceneKey inventory flags = updateGameState endScenes currentSceneKey inventory flags conditionalAction --The condition for the action passed, update the game state
    | otherwise = performConditionalActions currentSceneKey endScenes inventory flags Nothing
                                            (Just (Interaction {sentences = thisSentences,
                                                                conditionalActions = remainingConditionalActions})) --The condition for the action failed, attempt other actions

matchInteraction :: (Interaction, Sentence) -> Bool
matchInteraction (Interaction {sentences = thisSentences}, sentence)
    | sentence `elem` thisSentences = True
    | otherwise = False

findInteraction :: [Interaction] -> [Sentence] -> Maybe Interaction
findInteraction interactions sentences = find matchInteraction ((,) <$> interactions <*> sentences) >>=
                                         (\(x, _) -> Just x)

filterInteraction :: Scene -> Scene -> SceneKey -> [SceneKey] -> Inventory -> Flags -> [Sentence] -> IO (Maybe (SceneKey, Inventory, Flags))
filterInteraction (Scene {sceneDescription = _,
                          interactions = thisSceneInteractions})
                  (Scene {sceneDescription = _,
                          interactions = defaultSceneInteractions})
                  currentSceneKey
                  endScenes
                  inventory
                  flags
                  sentences
    = performConditionalActions currentSceneKey endScenes inventory flags interaction defaultInteraction
        where interaction = findInteraction thisSceneInteractions sentences
              defaultInteraction = findInteraction defaultSceneInteractions sentences

hasInvalidInteractions :: [Interaction] -> Maybe Interaction
hasInvalidInteractions [] = Nothing
hasInvalidInteractions (interaction@(Interaction {sentences = thisSentences}) : remainingInteractions)
    | NullSentence `elem` thisSentences = Just interaction
    | otherwise = hasInvalidInteractions remainingInteractions

printInvalidInteractions :: NarrativeGraph -> SceneKey -> IO ()
printInvalidInteractions narrativeGraph@(NarrativeGraph {nodes = graphNodes}) sceneKey
    = let scene = Data.Map.lookup sceneKey graphNodes
      in case scene of
          Nothing -> Control.Monad.void (putStrLn (sceneKey ++ " is not a valid scene"))
          Just (Scene {sceneDescription = _, interactions = sceneInteractions})
              -> case hasInvalidInteractions sceneInteractions of
                     Nothing -> return ()
                     Just interaction@(Interaction {sentences = thisSentences}) -> putStrLn ("Invalid interaction: " ++  show interaction)

--Perform an interaction with the current scene
--Takes the narrative graph, current scene index, inventory, and sentence as input
--Evaluates to Maybe of the next scene index and inventory state
performInteraction :: NarrativeGraph -> SceneKey -> Inventory -> Flags -> [Sentence] -> IO (Maybe (SceneKey, Inventory, Flags))
performInteraction _ sceneKey inventory flags []
    = putStrLn "Please enter a command." >>
      hFlush stdout >>
      return (Just (sceneKey, inventory, flags)) --If there are no valid sentences, just continue.
performInteraction narrativeGraph@(NarrativeGraph {nodes = graphNodes, endScenes = graphEndScenes, graphDefaultScene = thisDefaultScene}) sceneKey inventory flags sentences
    = let scene = Data.Map.lookup sceneKey graphNodes
      in case scene of
             Nothing -> hFlush stdout >> putStrLn (sceneKey ++ " is not a valid scene") >> return Nothing
             Just currentScene -> hFlush stdout >>
                                  filterInteraction currentScene thisDefaultScene sceneKey graphEndScenes inventory flags sentences
