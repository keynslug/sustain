--
-- Package

module Package (
    Package,
    PackageList,
    Section(..),
    fromSectionFullName,
    section,
    fullName,
    name,
    version,
    platform
) where

import Prelude

import Data.Ord (comparing)
import Data.Monoid
import Data.Text (Text, split)
import Version

data Section = Testing | Stable
    deriving (Show, Read, Enum, Bounded, Eq, Ord)

data Package = Package {
    section :: Section,
    fullName :: Text,
    name :: Text,
    version :: Text,
    rank :: Version,
    platform :: Text
    } deriving (Eq, Show)

instance Ord Package where
    compare p1 p2 =
        comparing name p1 p2 <>
        comparing section p1 p2 <>
        comparing rank p1 p2 <>
        comparing platform p1 p2

type PackageList = [Package]

fromSectionFullName :: Section -> Text -> Maybe Package
fromSectionFullName sec s =
    case split (== '_') s of
        [n, v, p] -> Just $ Package sec s n v (fromText v) p
        _ -> Nothing
