--
-- Main

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Imports hiding (fileName)
import Routes
import Settings
import Foundation
import Package
import qualified Layout
import qualified Aptly.HttpAPI as Aptly

import Prelude (Show)
import Data.String ()
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

import System.Posix.Daemonize (CreateDaemon(..), serviced, simpleDaemon)

mkYesodDispatch "Sustain" resourcesSustain

instance ToJSON Aptly.Result where
    toJSON Aptly.Success = object [ "status" .= ("ok" :: Text) ]
    toJSON (Aptly.Failure r) = object [ "status" .= ("error" :: Text), "reason" .= r ]

--
-- Resources

getHomeR :: Handler Html
getHomeR = withHomeLayout $ do
    pkgs <- liftIO Aptly.listAll
    Layout.withContent $ Layout.packageList pkgs

withPackage :: (Package -> IO Aptly.Result) -> Handler Aptly.Result
withPackage action = do
    pkg <- requireJsonBody :: Handler Package
    liftIO $ action pkg

postStabilizeR :: Handler Value
postStabilizeR = withPackage (Aptly.copy Stable) >>= returnJson

postRemoveR :: Handler Value
postRemoveR = withPackage Aptly.delete >>= returnJson

postSyncR :: Handler Value
postSyncR = do
    json <- requireJsonBody
    let parser = Json.withObject "Section" (\o -> o .: "section" >>= parseJSON)
    case Json.parse parser json :: Json.Result Section of
        Json.Error _ -> invalidArgs []
        Json.Success sec -> returnJson =<< (liftIO $ Aptly.sync sec)

postCleanupR :: Handler Value
postCleanupR = do
    notFound

--
-- Main

data Environment = Default
    deriving (Show)

main :: IO ()
main = serviced $ simpleDaemon { program = const server }

server :: IO ()
server = do
    s <- readSettings "config.yaml"
    app <- makeFoundation s
    warp (port s) app
