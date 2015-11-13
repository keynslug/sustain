--
-- Aptly

module Aptly.HttpAPI (
    Result(..),
    Prefix,
    list,
    listAll,
    copy,
    delete,
    sync,
    syncWithPrefix
    ) where

import Prelude

import Package
import Data.List (intercalate)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Char (toLower)
import Data.Text (Text, pack)
import Data.ByteString (ByteString)
import Data.Aeson (decodeStrict, toJSON, object, (.=))
import Control.Exception (handle, throw)
import Control.Lens ((^.))
import Network.Wreq (responseBody, statusCode)
import qualified Network.Wreq as HTTP
import Network.HTTP.Client (HttpException(..))

type Prefix = Maybe String

data Resourse = Repo Section | Publish Section Prefix
    deriving (Show, Eq)

data Result = Success | Failure Text
    deriving (Show, Eq)

list :: Section -> IO (Maybe PackageList)
list sec = bracketRequestMaybe $ do
    let ep = endpoint (Repo sec)
    response <- HTTP.get ep >>= HTTP.asJSON
    let pkgs = response ^. responseBody
    return $ mapMaybe (\s -> fromSectionPackageID sec (PackageKey s)) pkgs

copy :: Section -> Package -> IO Result
copy sec p = bracketRequest $ do
    let ep = endpoint (Repo sec)
    let copyWhat = toJSON $ object ["PackageRefs" .= [ref p]]
    HTTP.post ep copyWhat

delete :: Package -> IO Result
delete p = bracketRequest $ do
    let ep = endpoint (Repo $ section p)
    let deleteWhat = toJSON $ object ["PackageRefs" .= [ref p]]
    HTTP.delete ep deleteWhat

sync :: Section -> IO Result
sync = syncWithPrefix Nothing

syncWithPrefix :: Prefix -> Section -> IO Result
syncWithPrefix prefix sec = bracketRequest $ do
    let ep = endpoint (Publish sec prefix)
    let syncOptions = toJSON $ object ["ForceOverwrite" .= True]
    HTTP.put ep syncOptions

listAll :: IO PackageList
listAll = do
    let ds = enumFrom minBound
    fmap (concat . mapMaybe id) $ mapM list ds

bracketRequestMaybe :: IO a -> IO (Maybe a)
bracketRequestMaybe = fmap (either Just (const Nothing)) . bracketRequest'

bracketRequest :: IO a -> IO Result
bracketRequest = fmap (either (const Success) Failure) . bracketRequest'

bracketRequest' :: IO a -> IO (Either a Text)
bracketRequest' = handle hdl . fmap Left where
    hdl :: HttpException -> IO (Either a Text)
    hdl (StatusCodeException status hs _) = do
        let code = status ^. statusCode
        let description = fromMaybe mempty $ lookup "X-Response-Body-Start" hs
                >>= (decodeStrict :: ByteString -> Maybe [[(Text, Text)]])
                >>= lookup "error" . head
                >>= return
        let result = mconcat [pack (show code), " ", description]
        return $ Right result
    hdl e =
        throw e

endpoint :: Resourse -> String
endpoint = intercalate "/" . (baseUrl :) . ep where
    ep (Repo sec) = ["repos", showSection sec, "packages"]
    ep (Publish sec prefix) = ["publish", showPrefix prefix, showSection sec]
    baseUrl = "http://aptly.platbox.com/api"
    showSection = map toLower . show
    showPrefix = fromMaybe ""
