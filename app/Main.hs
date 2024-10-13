--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Use void" #-}


import           CmdOptions           as Cmd (parse)
import           Control.Monad        (void)
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Map             (Map)
import           Data.Text            as T (Text, concat, unpack, intercalate)
import qualified DummyAdventure       as Dummy (allPrepositions, allScenes,
                                                allTokens, allVerbs,
                                                defaultScene, gameIntro,
                                                startFlags, startInventory,
                                                startScene)
import           GHC.Generics
import           NarrativeGraph       (Flags, Inventory, Scene, SceneKey,
                                       makeNarrativeGraph)
import           NaturalLanguageLexer (Prepositions, Tokens, Verbs)
import qualified NightmareAdventure   as Nightmare (allPrepositions, allScenes,
                                                    allTokens, allVerbs,
                                                    defaultScene, gameIntro,
                                                    startFlags, startInventory,
                                                    startScene)
import           PrintUtils           (printHelp, printIntro)
import           System.Environment   as E (getArgs)
import           System.IO            (hFlush, stdout)
import           TextAdventureCore    (adventure)
import           TextReflow           (reflowPutStr)
import Data.List (intercalate)
import Data.List (intersperse)

data Person =
  Person { firstName  :: !T.Text
         , lastName   :: !T.Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show,Generic)

instance FromJSON Person
instance ToJSON Person

jsonFile :: FilePath
jsonFile = "stories/pizza.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

getStories :: IO String
getStories = readFile "stories.txt"

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
displayHelp =
  (eitherDecode <$> getJSON) >>= \d ->
  case d of
    Left err -> putStrLn err
    Right ps ->
      let names = T.unpack $ T.intercalate "\n" (map (\p -> T.concat [firstName p, " ", lastName p]) ps)
      in Cmd.parse names >> return ()

runDummy :: IO ()
runDummy = runGame Dummy.allVerbs
                   Dummy.allPrepositions
                   Dummy.allTokens
                   Dummy.gameIntro
                   Dummy.defaultScene
                   Dummy.startScene
                   Dummy.startInventory
                   Dummy.startFlags
                   Dummy.allScenes

runNightmare :: IO ()
runNightmare = runGame Nightmare.allVerbs
                       Nightmare.allPrepositions
                       Nightmare.allTokens
                       Nightmare.gameIntro
                       Nightmare.defaultScene
                       Nightmare.startScene
                       Nightmare.startInventory
                       Nightmare.startFlags
                       Nightmare.allScenes

runGame :: Verbs -> Prepositions -> Tokens -> String ->
    NarrativeGraph.Scene ->
    String ->
    NarrativeGraph.Inventory ->
    NarrativeGraph.Flags ->
    (Data.Map.Map NarrativeGraph.SceneKey NarrativeGraph.Scene, [NarrativeGraph.SceneKey]) ->
    IO ()
runGame verbs prepositions tokens gameIntro defaultScene startScene startInventory startFlags allScenes =
    printIntro >>
    reflowPutStr gameIntro >>
    putStr "\n" >>
    printHelp >>
    putStr "\n\n\n" >>
    hFlush stdout >>
    adventure verbs prepositions tokens (makeNarrativeGraph adventureScenes endScenes defaultScene) (Just (startScene, startInventory, startFlags)) >>
    return ()
        where (adventureScenes, endScenes) = allScenes

