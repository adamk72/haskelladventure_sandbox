--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable

import           System.IO

-- import CmdOptions
import           CmdOptions        (AdventureOptions (AdventureOptions), parse)
import           DummyAdventure    (allScenes, defaultScene, gameIntro,
                                    startFlags, startInventory, startScene)
import           NarrativeGraph    (makeNarrativeGraph)
import           PrintUtils        (allColumnWidth, allDelimiters, printHelp,
                                    printIntro)
import           TextAdventureCore (adventure)
import           TextReflow        (reflowPutStr)

main :: IO ()

main =
    readFile "stories.txt" >>= parse >>=
    (\(AdventureOptions a) -> putStrLn $ "You chose: '" ++ a ++ "'.") >>
    printIntro >>
    reflowPutStr allDelimiters allColumnWidth gameIntro >>
    putStr "\n" >>
    printHelp >>
    putStr "\n\n\n" >>
    hFlush stdout >>
    adventure (makeNarrativeGraph adventureScenes endScenes defaultScene) (Just (startScene, startInventory, startFlags)) >>
    return ()
        where (adventureScenes, endScenes) = allScenes
