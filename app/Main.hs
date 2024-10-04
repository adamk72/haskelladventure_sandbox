--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable

import System.IO

import NarrativeGraph hiding (endScenes)
import PrintUtils
import TextAdventureCore
import TextReflow

import DummyAdventure

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
