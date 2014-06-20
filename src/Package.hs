--
-- Package

module Package (
    Package,
    PackageList,
    Section(..),
    Version,
    Platform,
    fromSectionFullName,
    section,
    fullName,
    name,
    version,
    platform
) where

import Prelude
import Data.Function (on)
import Data.Ord (comparing)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text

data Section = Testing | Stable
    deriving (Show, Read, Enum, Bounded, Eq, Ord)

type Version = Text
type Platform = Text

data Package = Package {
    section :: Section,
    fullName :: Text,
    name :: Text,
    version :: Version,
    platform :: Platform
    } deriving (Eq, Show)

instance Ord Package where
    compare p1 p2 =
        comparing name p1 p2 <>
        comparing section p1 p2 <>
        (compareVersion `on` version) p1 p2 <>
        comparing platform p1 p2

type PackageList = [Package]

fromSectionFullName :: Section -> Text -> Maybe Package
fromSectionFullName sec s =
    case Text.split (== '_') s of
        [n, v, p] -> Just $ Package sec s n v p
        _ -> Nothing

compareVersion :: Version -> Version -> Ordering
compareVersion = comparing dissect

dissect :: Version -> ([Text], [Text])
dissect subject = (epoch : parts, leftover) where
    (epoch, rest) = let (e, r) = Text.breakOn ":" subject in
        if Text.null r
            then ("0", subject)
            else (e, safeTail r)
    (parts, leftover) = let (p, r) = Text.breakOnEnd "-" rest in
        if Text.null p
            then (parts' r, [])
            else (parts' $ safeInit p, parts' r)
    parts' s = take maxParts $ Text.split (flip elem $ delimeters) s ++ partsTail
    partsTail = repeat Text.empty
    safeInit = safeOn Text.init
    safeTail = safeOn Text.tail
    delimeters = ".+-:~"
    maxParts = 8

safeOn :: (Text -> Text) -> Text -> Text
safeOn f s
    | Text.null s = s
    | otherwise = f s
