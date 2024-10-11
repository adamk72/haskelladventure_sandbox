--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE LambdaCase #-}

import           System.IO

-- import CmdOptions
import           CmdOptions         (AdventureOptions (AdventureOptions), parse)
import           DummyAdventure     (allScenes, defaultScene, gameIntro,
                                     startFlags, startInventory, startScene)
import           NarrativeGraph     (makeNarrativeGraph)
import           PrintUtils         (allColumnWidth, allDelimiters, printHelp,
                                     printIntro)
import           TextAdventureCore  (adventure)
import           TextReflow         (reflowPutStr)

import           System.Environment as E


main :: IO ()
main =
    -- E.getArgs >>= print >>
    E.getArgs >>= \case
        ["-a", "Dummy Adventure"]       -> runDummy
        ["-a", "Dummy"]                 -> runDummy
        ["-a", "Nightmare Adventure"]   -> runDummy
        ["-a", "Nightmare"]             -> runDummy
        _                               -> displayHelp

displayHelp :: IO ()
displayHelp = readFile "stories.txt" >>= parse >>= mempty

runDummy :: IO ()
runDummy =
    printIntro >>
    reflowPutStr allDelimiters allColumnWidth gameIntro >>
    putStr "\n" >>
    printHelp >>
    putStr "\n\n\n" >>
    hFlush stdout >>
    adventure (makeNarrativeGraph adventureScenes endScenes defaultScene) (Just (startScene, startInventory, startFlags)) >>
    return ()
        where (adventureScenes, endScenes) = allScenes

