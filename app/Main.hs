--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable

-- import System.IO

import CmdOptions
-- import NarrativeGraph hiding (endScenes)
-- import PrintUtils
-- import TextAdventureCore
-- import TextReflow

-- import DummyAdventure
main :: IO ()
-- main =  (greet =<< parse) >> putStrLn "And a test"
main =  readFile "stories.txt" >>= (\a -> parse a) >>= (\(AdventureOptions a) -> putStrLn $ "You chose: '" ++ a ++ "'.") 

-- main = printIntro >>
--        reflowPutStr allDelimiters allColumnWidth gameIntro >>
--        putStr "\n" >>
--        printHelp >>
--        putStr "\n" >>
--        putStr "\n" >>
--        putStr "\n" >>
--        hFlush stdout >>
--        adventure (makeNarrativeGraph adventureScenes endScenes defaultScene) (Just (startScene, startInventory, startFlags)) >>
--        return ()
          --  where (adventureScenes, endScenes) = allScenes
