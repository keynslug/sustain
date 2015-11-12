--
-- Main

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Imports hiding (fileName)
import Routes
import Settings
import Foundation
import Package
import qualified Layout
import Aptly.CLI (Action(..), Result(..))
import Aptly.CLI (run)
import Aptly.HttpAPI (listAll)

import Prelude (Show, show, read)
import Data.String ()
import Data.Aeson (withText, withObject)
import qualified Data.Aeson.Types as Json (Result(..), parse)

import System.Posix.Daemonize (CreateDaemon(..), serviced, simpleDaemon)

mkYesodDispatch "Sustain" resourcesSustain

instance ToJSON Section where
    toJSON = String . pack . show

instance FromJSON Section where
    parseJSON = withText "Section" $ \t -> return (read $ unpack t)

instance ToJSON Package where
    toJSON p = object [ "name" .= fileName p, "section" .= section p ]

instance FromJSON Package where
    parseJSON (Object o) = fmap fromJust $ fromSectionPackageID <$> o .: "section" <*> (Filename <$> o .: "name")
    parseJSON _ = mzero

instance ToJSON Result where
    toJSON (Success _) = object [ "status" .= ("ok" :: Text) ]
    toJSON (Failure r) = object [ "status" .= ("error" :: Text), "reason" .= pack r ]

--
-- Resources

getHomeR :: Handler Html
getHomeR = withHomeLayout $ do
    pkgs <- liftIO listAll
    Layout.withContent $ Layout.packageList pkgs

withPackage :: (Package -> Action) -> Handler Result
withPackage action = do
    pkg <- requireJsonBody :: Handler Package
    liftIO $ run (action pkg)

postStabilizeR :: Handler Value
postStabilizeR = withPackage (Copy Stable) >>= returnJson

postRemoveR :: Handler Value
postRemoveR = withPackage Remove >>= returnJson

postSyncR :: Handler Value
postSyncR = do
    json <- requireJsonBody
    let parser = withObject "Section" (\o -> o .: "section" >>= parseJSON)
    case Json.parse parser json :: Json.Result Section of
        Json.Error _ -> invalidArgs []
        Json.Success sec -> returnJson =<< (liftIO $ run (Sync sec))

postCleanupR :: Handler Value
postCleanupR = do
    _ <- requireJsonBody :: Handler Value
    liftIO $ run Cleanup >>= returnJson

--
-- Main

data Environment = Default
    deriving (Show)

main :: IO ()
--main = serviced $ simpleDaemon { program = const server }
main = server

server :: IO ()
server = do
    s <- readSettings "config.yaml"
    app <- makeFoundation s
    warp (port s) app
