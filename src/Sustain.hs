--
-- Main

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Imports
import Foundation
import Package
import qualified Layout
import Aptly (Action(..), Result(..))
import qualified Aptly

import Prelude (Show, show, read, unlines, putStrLn)
import Data.Maybe
import Data.String ()
import Control.Applicative ((<*>))
import Control.Monad (return, (>>=))

import Data.Aeson (withText, withObject)
import qualified Data.Aeson.Types as Json (Result(..), parse)


--

mkYesodDispatch "Sustain" resourcesSustain

--

pkgs :: PackageList
pkgs = let
    from = mapMaybe (uncurry fromSectionFullName)
    tps = from $ zip (repeat Testing) [
        "cdrdao_1:1.2.3-0.3ubuntu1_amd64",
        "gnupg2_2.0.21-2ubuntu1.1_amd64",
        "libxcb_0.3.9-1_amd64",
        "libcddb2_1.3.2-3fakesync1_amd64",
        "libupnp6_1:1.6.17-1.2_amd64",
        "libdvbpsi7_0.2.2-1_amd64",
        "libcddb2_0.83-4_amd64",
        "libxcb_1.8.1-2ubuntu2.1_amd64",
        "libmatroska5_1.3.0-2_amd64",
        "cdrdao_2.3-3_amd64",
        "libcddb2_4:4.7.0really4.6.0-0ubuntu2_amd64"]
    sps = from $ zip (repeat Stable) [
        "libcddb2_2.1.1-14_amd64",
        "libsdl-image1.2_1.2.12-3~exp1ubuntu2_amd64",
        "libcddb2_1.2.0-2build1_amd64",
        "libssh2_1.2.16-1_amd64",
        "libdvbpsi7_1.2.2-2_amd64",
        "gnupg2_2.0.19-2ubuntu1.1_amd64",
        "libupnp6_0.7.24+dfsg-0.1_amd64",
        "libupnp6_2.0.8-0ubuntu0.13.04.1_amd64",
        "libvlccore5_2.0.8-0ubuntu0.13.04.1_amd64",
        "libxcb_1.8.1-2ubuntu2.1_amd64",
        "libssh2-1_1.4.2-1.1_amd64",
        "libxcb_1.8.1-2ubuntu2.1_amd64"]
    in tps ++ sps

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    Layout.withContent $ Layout.packageList pkgs

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

logResult :: Result -> IO ()
logResult (Success ls) = putStrLn $ unlines $ map (" > " ++) ls
logResult (Failure s) = putStrLn $ " ERROR > " ++ s

withPackage :: (Package -> Action) -> Handler Result
withPackage action = do
    pkg <- requireJsonBody :: Handler Package
    res <- liftIO $ Aptly.run (action pkg)
    liftIO $ logResult res
    return res

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
        Json.Success sec -> do
            res <- liftIO $ Aptly.run (Sync sec)
            liftIO $ logResult res
            returnJson res

--

main :: IO ()
main = do
    app <- makeFoundation
    warp 3000 app
