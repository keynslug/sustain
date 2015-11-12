--
-- Aptly

module Aptly.CLI (
    Action(..),
    Result(..),
    run,
    list,
    listAll
    ) where

import Prelude
import Package
import Data.Monoid
import Data.List (isPrefixOf, dropWhileEnd)
import Data.Maybe (mapMaybe)
import Data.Char (toLower, isSpace)
import Data.Text (pack, unpack)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

data Action =
    List Section
    | Copy Section Package
    | Remove Package
    | Sync Section
    | Cleanup

data Result =
    Success [String]
    | Failure String
        deriving (Show)

run :: Action -> IO Result
run action = do
    let args = constructArgs action
    result <- readProcessWithExitCode aptlyExecutable args mempty
    return $ fromResult result where
        aptlyExecutable = "aptly"
        fromResult (ExitFailure code, stdout, _) = fromError code $ lines stdout
        fromResult (ExitSuccess, stdout, _) = parseResult action $ lines stdout

list :: Section -> IO (Maybe PackageList)
list sec = do
    result <- run (List sec)
    return $ case result of
        Failure _ -> Nothing
        Success ls -> do
            let names = dropWhile (isPrefixOf "Packages") ls
            if null names
                then Nothing
                else Just $ mapMaybe fromLine $ map stripWs $ tail names where
                    fromLine = fromSectionPackageID sec . Filename . pack
                    stripWs = dropWhile isSpace . dropWhileEnd isSpace

listAll :: IO PackageList
listAll = do
    let ds = enumFrom minBound
    fmap (concat . mapMaybe id) $ mapM list ds

constructArgs :: Action -> [String]
constructArgs = construct where
    construct (List sec) = ["repo", "show", "-with-packages", showSection sec]
    construct (Copy to p) = ["repo", "copy", showSection $ section p, showSection to, packageName p]
    construct (Remove p) = ["repo", "remove", showSection $ section p, packageName p]
    construct (Sync sec) = ["publish", "update", showSection sec]
    construct Cleanup = ["db", "cleanup"]
    packageName = unpack . fileName
    showSection = map toLower . show

fromError :: Int -> [String] -> Result
fromError code ls =
    Failure $ "Failed with exit code " ++ show code ++ case ls of
        [] -> ""
        _  -> " and message: " ++ last ls

parseResult :: Action -> [String] -> Result
parseResult (Copy _ _) stdout
    | length stdout > 1 = Success stdout
    | otherwise = Failure "Nothing was copied"
parseResult (Remove _) stdout
    | length stdout > 1 = Success stdout
    | otherwise = Failure "Nothing was removed"
parseResult _ stdout =
    Success stdout
