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

import Prelude hiding (null, init, tail, length)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Monoid
import Data.Text (Text, null, empty, init, tail, length, breakOn, breakOnEnd, split)

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
    case split (== '_') s of
        [n, v, p] -> Just $ Package sec s n v p
        _ -> Nothing

compareVersion :: Version -> Version -> Ordering
compareVersion = comparing dissect

newtype VersionHunk = VersionHunk Text
    deriving (Eq)

instance Ord VersionHunk where
    compare (VersionHunk h1) (VersionHunk h2) =
        comparing length h1 h2 <> compare h1 h2

dissect :: Version -> ([VersionHunk], [VersionHunk])
dissect subject = (epoch : hunks, leftover) where
    (epoch, rest) = let (e, r) = breakOn ":" subject in
        if null r
            then (VersionHunk "0" , subject)
            else (VersionHunk e   , safeTail r)
    (hunks, leftover) = let (p, r) = breakOnEnd "-" rest in
        if null p
            then (hunks' r, [])
            else (hunks' $ safeInit p, hunks' r)
    hunks' s = fmap VersionHunk $
        take maxHunks $ split (flip elem $ delimeters) s ++ hunksLeft
    hunksLeft = repeat empty
    safeInit = safeOn init
    safeTail = safeOn tail
    delimeters = ".+-:~"
    maxHunks = 8

safeOn :: (Text -> Text) -> Text -> Text
safeOn f s
    | null s = s
    | otherwise = f s
