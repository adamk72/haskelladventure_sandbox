{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module HelpDetails (storyDirectory, getJsonFilePaths, processJsonFiles) where


import           Data.Aeson           (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy as B
import           Data.Text            as T (Text)
import           GHC.Generics         (Generic)
import           System.Directory
import           System.FilePath      (takeExtension, (</>))

data AdventureDetail =
  AdventureDetail { fullName    :: !T.Text
                  , shortName   :: !T.Text
                  , description :: !T.Text
                  } deriving (Show,Generic)

instance FromJSON AdventureDetail
instance ToJSON AdventureDetail

storyDirectory :: FilePath
storyDirectory = "stories"

getJsonFilePaths :: FilePath -> IO [FilePath]
getJsonFilePaths dir = do
    allFiles <- listDirectory dir
    let jsonFiles = filter (\f -> takeExtension f == ".json") allFiles
    return $ map (dir </>) jsonFiles

readJsonFile :: FilePath -> IO (Maybe AdventureDetail)
readJsonFile filePath = do
    contents <- B.readFile filePath
    return $ decode contents

processJsonFiles :: [FilePath] -> IO [Maybe (T.Text, T.Text, T.Text)]
processJsonFiles = mapM processFile
  where
    processFile file = do
      mAdventure <- readJsonFile file
      return (extractNames mAdventure)

    extractNames (Just adv) = Just (fullName adv, shortName adv, description adv)
    extractNames Nothing    = Nothing