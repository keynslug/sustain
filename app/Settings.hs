--
-- Settings

module Settings where

import Prelude
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Yaml

data Settings = Settings {
    host :: Text,
    port :: Int,
    authUri :: String,
    ldapDomain :: String,
    ldapDomainFrags :: [String],
    bindUser :: String,
    bindPassword :: String,
    privilegedUsers :: [Text]
    } deriving (Show, Read, Eq)

uriFromDomain :: Bool -> String -> String
uriFromDomain sec dom = prefix sec ++ dom ++ "/" where
    prefix False = "ldap://"
    prefix True = "ldaps://"

splitDomain :: String -> [String]
splitDomain dom = splitOn '.' dom where
    splitOn c s = case dropWhile (== c) s of
        "" -> []
        s' -> w : splitOn c rest
            where (w, rest) = break (== c) s'

fromValue :: Value -> Maybe Settings
fromValue = parseMaybe parser where
    parser (Object o) = do
        dom <- o .: "ldapDomain"
        uri <- o .:? "ldapUri" .!= (uriFromDomain False dom)
        Settings
            <$> o .:? "host" .!= ""
            <*> o .: "port"
            <*> return uri
            <*> return dom
            <*> return (splitDomain dom)
            <*> o .: "bindUser"
            <*> o .: "bindPassword"
            <*> o .:? "privileged" .!= []
    parser _ = mzero

readSettings :: FilePath -> IO Settings
readSettings fp = do
    v <- decodeFile fp
    maybe (fail "Wrong settings") return $
        v >>= fromValue
