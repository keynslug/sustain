--
-- Aptly

module Aptly (
    Action(..),
    Result(..),
    run,
    list
    ) where

import Prelude
import Package
import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.Char (toLower)
import Data.Text (pack, unpack)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

data Action =
    List Section
    | Copy Section Package
    | Remove Package
    | Sync Section

data Result =
    Success [String]
    | Failure String
        deriving (Show)

run :: Action -> IO Result
run action = do
    result <- readProcessWithExitCode aptlyExecutable (constructArgs action) mempty
    return $ fromResult result where
        aptlyExecutable = "aptly"
        fromResult (ExitFailure code, _, errorString) = fromError code errorString
        fromResult (ExitSuccess, stdout, _) = parseResult stdout

list :: Section -> IO (Maybe PackageList)
list sec = do
    result <- run (List sec)
    return $ case result of
        Success names -> Just $ mapMaybe ((fromSectionFullName sec) . pack) names
        Failure _ -> Nothing

constructArgs :: Action -> [String]
constructArgs = construct where
    construct (List sec) = ["repo", "show", "-with-packages", showSection sec]
    construct (Copy to p) = ["repo", "copy", showSection $ section p, showSection to, packageName p]
    construct (Remove p) = ["repo", "remove", showSection $ section p, packageName p]
    construct (Sync sec) = ["publish", "update", showSection sec]
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
