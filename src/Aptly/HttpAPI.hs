--
-- Aptly

module Aptly.HttpAPI (
    list,
    listAll
    ) where

import Package
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Char (toLower)
import Data.Text (Text)
import Network.Wreq
import Control.Lens ((^.))

data Action =
    List Section
    | Copy Section Package
    | Remove Package
    | Sync Section

list :: Section -> IO (Maybe PackageList)
list sec = do
    response <- get (endpoint $ List sec) >>= asJSON
    let list = response ^. responseBody
    let from = \s -> fromSectionPackageID sec (PackageKey s)
    return $ Just (mapMaybe from list)

listAll :: IO PackageList
listAll = do
    let ds = enumFrom minBound
    fmap (concat . mapMaybe id) $ mapM list ds

endpoint :: Action -> String
endpoint = intercalate "/" . (baseUrl :) . ep where
    ep (List sec) = ["repos", showSection sec, "packages"]
    baseUrl = "http://aptly.platbox.com/api"
    showSection = map toLower . show
