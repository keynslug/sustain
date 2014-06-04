--
-- Main

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Imports
import Foundation
import qualified Layout
import qualified Package
import qualified Aptly

--

mkYesodDispatch "Sustain" resourcesSustain

--

pkgs :: [Text]
pkgs = [
    "cdrdao_2.3-3_amd64",
    "cdrdao_1:1.2.3-0.3ubuntu1_amd64",
    "gnupg2_2.0.19-2ubuntu1.1_amd64",
    "gnupg2_2.0.21-2ubuntu1.1_amd64",
    "libcddb2_1.3.2-3fakesync1_amd64",
    "libdvbpsi7_0.2.2-1_amd64",
    "libdvbpsi7_1.2.2-2_amd64",
    "libcddb2_0.83-4_amd64",
    "libcddb2_1.2.0-2build1_amd64",
    "libmatroska5_1.3.0-2_amd64",
    "libcddb2_4:4.7.0really4.6.0-0ubuntu2_amd64",
    "libcddb2_2.1.1-14_amd64",
    "libsdl-image1.2_1.2.12-3~exp1ubuntu2_amd64",
    "libssh2-1_1.4.2-1.1_amd64",
    "libssh2_1.2.16-1_amd64",
    "libupnp6_1:1.6.17-1.2_amd64",
    "libupnp6_0.7.24+dfsg-0.1_amd64",
    "libupnp6_2.0.8-0ubuntu0.13.04.1_amd64",
    "libvlccore5_2.0.8-0ubuntu0.13.04.1_amd64",
    "libxcb_1.8.1-2ubuntu2.1_amd64",
    "libxcb_0.3.9-1_amd64",
    "libxcb_1.8.1-2ubuntu2.1_amd64",
    "libxcb_1.8.1-2ubuntu2.1_amd64"
    ]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    let packages = mapMaybe Package.fromFullName pkgs
    Layout.withContent $ Layout.packageList packages

--

main :: IO ()
main = do
    app <- makeFoundation
    warp 3000 app
