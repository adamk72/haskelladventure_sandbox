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
-- greet :: AdventureOptions -> IO ()
-- greet (AdventureOptions a ) = putStrLn $ "You chose: '" ++ a ++ "'." 

main :: IO ()
-- main =  (greet =<< parse) >> putStrLn "And a test"
main = (\x -> case x of
                AdventureOptions a -> putStrLn $ "You chose: '" ++ a ++ "'.") =<< parse

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
