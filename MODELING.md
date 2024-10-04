This document is to describe how the HaskelAdventure works. The goal is to reverse engineer the logic with the consideration of learning.

# TextAdventure.hs

## Main

Make use of `>>` to call list of IO function.

`reflowPutStr` is used to make sure strings are aligned with screen size.
`adventure`


### Questions

- Why doesn't he put the gameInfo text into its own function like he does with printHelp?

### `adventure`
```haskell
adventure :: NarrativeGraph -> Maybe (SceneKey, Inventory, Flags) -> IO (Maybe (SceneKey, Inventory, Flags))
```
He start in `main` with using `makeNarrativeGraph`:
```haskell
makeNarrativeGraph :: Data.Map.Map SceneKey Scene -> [SceneKey] -> Scene -> NarrativeGraph
```

where he passes in an `allScenes` list which is consists of two lists of tuples, the first for the opening part of the adventure and the last for the end of the adventure. Those are split up and passed on to form the `NarrativeGraph` which is a data type.

`adventure` calls `updateAdventure` which calls `doAdventureLoop` which calls back to `adventure` to keep the game going.


### `parseInput`

Does what it says. This function in interesting because he didn't really componentize it. It takes the input string and pattern matches it against its LC and runs a function directly. I figured some sort of `fmap` would have come into play here.

The `where` clause has a `sentences` function that is mapped against and if turns up empty (`[]`), returns an error message to the user.

