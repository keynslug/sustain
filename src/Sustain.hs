--
-- Main

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Imports
import Routes
import Foundation
import Package
import qualified Layout
import Aptly (Action(..), Result(..))
import qualified Aptly

import Prelude (Show, show, read)
import Data.String ()
import Data.Aeson (withText, withObject)
import qualified Data.Aeson.Types as Json (Result(..), parse)

mkYesodDispatch "Sustain" resourcesSustain

instance ToJSON Section where
    toJSON = String . pack . show

instance FromJSON Section where
    parseJSON = withText "Section" $ \t -> return (read $ unpack t)

instance ToJSON Package where
    toJSON p = object [ "name" .= fullName p, "section" .= section p ]

instance FromJSON Package where
    parseJSON (Object o) = fmap fromJust $ fromSectionFullName <$> o .: "section" <*> o .: "name"
    parseJSON _ = mzero

instance ToJSON Result where
    toJSON (Success _) = object [ "status" .= ("ok" :: Text) ]
    toJSON (Failure r) = object [ "status" .= ("error" :: Text), "reason" .= pack r ]

--
-- Resources

getHomeR :: Handler Html
getHomeR = homeLayout $ do
    pkgs <- liftIO Aptly.listAll
    Layout.withContent $ Layout.packageList pkgs

withPackage :: (Package -> Action) -> Handler Result
withPackage action = do
    pkg <- requireJsonBody :: Handler Package
    liftIO $ Aptly.run (action pkg)

postStabilizeR :: Handler Value
postStabilizeR = withPackage (Copy Stable) >>= returnJson

postRemoveR :: Handler Value
postRemoveR = withPackage Remove >>= returnJson

postSyncR :: Handler Value
postSyncR = do
    json <- requireJsonBody
    let parser = withObject "Section" (\o -> o .: "section" >>= parseJSON)
    case Json.parse parser json :: Json.Result Section of
        Json.Error _ ->invalidArgs []
        Json.Success sec -> returnJson =<< (liftIO $ Aptly.run (Sync sec))

--
-- Main

main :: IO ()
main = do
    app <- makeFoundation
    warp 3000 app
