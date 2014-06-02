-- Aptly

module Aptly (
    Section(..),
    Action(..),
    Result,
    aptly,
    aptlyList
    ) where

import Prelude
import Package
import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.Char (toLower)
import Data.Text (pack, unpack)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

--

data Section = Testing | Stable
    deriving (Show, Enum)

data Action =
    List Section
    | Copy Section Section Package
    | Remove Section Package
        deriving (Show)

data Result =
    Success [String]
    | Failure String
        deriving (Show)

--

aptly :: Action -> IO Result
aptly action = do
    let args = constructArgs action
    result <- readProcessWithExitCode aptlyExecutable (repoCommand : args) mempty
    return $ fromResult result where
        aptlyExecutable = "echo"
        repoCommand = "repo"
        fromResult (ExitFailure code, _, errorString) = fromError code errorString
        fromResult (ExitSuccess, stdout, _) = parseResult stdout

aptlyList :: Section -> IO (Maybe PackageList)
aptlyList section = do
    result <- aptly (List section)
    return $ case result of
        Success names -> Just $ mapMaybe (fromFullName . pack) names
        Failure _ -> Nothing

constructArgs :: Action -> [String]
constructArgs = construct' where
    construct' (List section) = ["show", showSection section, "-with-packages"]
    construct' (Copy secFrom secTo p) = ["copy", showSection secFrom, showSection secTo, packageName p]
    construct' (Remove section p) = ["remove", showSection section, packageName p]
    packageName = quote . unpack . fullName
    quote s = concat ["\"", s, "\""]
    showSection = map toLower . show

fromError :: Int -> String -> Result
fromError code string =
    Failure $ "Failed with exit code " ++ show code ++ case lines string of 
        [] -> ""
        [l] -> ": " ++ l
        (l:_) -> ": " ++ l ++ " ..."

parseResult :: String -> Result
parseResult stdout = Success $ lines stdout
