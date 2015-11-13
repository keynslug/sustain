--
-- Package

module Package (
    Package,
    PackageList,
    Section(..),
    PackageID(..),
    fromSectionPackageID,
    section,
    identifier,
    fileName,
    ref,
    name,
    version,
    platform
) where

import Prelude

import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Text (Text, pack, unpack, split, uncons, intercalate)
import Data.Aeson
import Control.Monad (mzero)
import Version

data Section = Testing | Stable
    deriving (Show, Read, Enum, Bounded, Eq, Ord)

data PackageID = Filename Text | PackageKey Text
    deriving (Show, Read, Eq)

data Package = Package {
    section :: Section,
    identifier :: PackageID,
    platform :: Text,
    name :: Text,
    version :: Text,
    rank :: Version
    } deriving (Eq, Show)

instance Ord Package where
    compare p1 p2 =
        comparing name p1 p2 <>
        comparing section p1 p2 <>
        comparing rank p1 p2 <>
        comparing platform p1 p2

type PackageList = [Package]

instance ToJSON Section where
    toJSON = String . pack . show

instance FromJSON Section where
    parseJSON = withText "Section" $ \t -> return (read $ unpack t)

instance ToJSON PackageID where
    toJSON (Filename fn) = String fn
    toJSON (PackageKey key) = object [ "key" .= key ]

instance FromJSON PackageID where
    parseJSON (Object o) = PackageKey <$> o .: "key"
    parseJSON (String fn) = Filename <$> return fn
    parseJSON _ = mzero

instance ToJSON Package where
    toJSON p = object [ "id" .= identifier p, "section" .= section p ]

instance FromJSON Package where
    parseJSON (Object o) = fromJust <$> (fromSectionPackageID <$> o .: "section" <*> o .: "id")
    parseJSON _ = mzero

fromSectionPackageID :: Section -> PackageID -> Maybe Package
fromSectionPackageID sec ident = from ident $ builder sec ident where
    from (Filename fn) = fromFileName fn
    from (PackageKey key) = fromKey key

fileName :: Package -> Text
fileName p = intercalate "_" $ map ($ p) [name, version, platform]

ref :: Package -> Text
ref = extract . identifier where
    extract (Filename fn) = fn
    extract (PackageKey key) = key

type PackageBuilder = Text -> Text -> Text -> Package

builder :: Section -> PackageID -> PackageBuilder
builder sec ident p n v = Package sec ident p n v (fromText v)

fromFileName :: Text -> PackageBuilder -> Maybe Package
fromFileName fn build =
    case split (== '_') fn of
        [n, v, p] -> Just $ build p n v
        _ -> Nothing

fromKey :: Text -> PackageBuilder -> Maybe Package
fromKey key build = do
    (what, text) <- uncons key
    case (what, split (== ' ') text) of
        ('P', [p, n, v, _]) -> Just $ build p n v
        _ -> Nothing
