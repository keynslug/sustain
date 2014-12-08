--
-- Version

module Version (Version, VersionHunk, fromText) where

import Prelude hiding (null, init, tail, length, dropWhile)

import Data.Char
import Data.Ord (comparing)
import Data.Monoid
import Data.Text (Text, null, empty, unpack, init, tail, breakOn, breakOnEnd, dropWhile, groupBy)

import Control.Monad.State

newtype VersionHunk = VersionHunk Text
    deriving (Eq, Show)

vhunk :: Text -> VersionHunk
vhunk = VersionHunk

unhunk :: VersionHunk -> Text
unhunk (VersionHunk v) = v

instance Ord VersionHunk where
    compare = comparing (
        map (
            map order .
            unpack .
            dropWhile (== '0')) .
        groupBy (\a b -> isDigit a && isDigit b) .
        unhunk) where
            order c
                | isAlphaNum c = ord c
                | c == '~' = -1
                | c == chr 0 = 0
                | otherwise = 256 + ord c

data Version = Version {
    epoch :: VersionHunk,
    primary :: VersionHunk,
    revision :: VersionHunk
    } deriving (Eq, Ord, Show)

fromText :: Text -> Version
fromText t = execState (parseEpoch t >>= parseRevision >>= parsePrimary) $
    Version v0 v0 v0 where v0 = VersionHunk empty

parseEpoch :: Text -> State Version Text
parseEpoch t = do
    v <- get
    let (e, t') = breakOn ":" t
    if null t'
        then return t
        else do
            put $ v { epoch = vhunk e }
            return $ safeOn tail t'

parseRevision :: Text -> State Version Text
parseRevision t = do
    v <- get
    let (t', r) = breakOnEnd "-" t
    if null t'
        then return t
        else do
            put $ v { revision = vhunk r }
            return $ safeOn init t'

parsePrimary :: Text -> State Version Text
parsePrimary t = do
    v <- get
    put $ v { primary = vhunk t }
    return mempty

safeOn :: (Text -> Text) -> Text -> Text
safeOn f s
    | null s = s
    | otherwise = f s
