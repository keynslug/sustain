-- Routes

module Routes where

import Imports
import Prelude (String)
import Network.HTTP.Conduit (Manager)

data Settings = Settings {
    ldapDomain :: String,
    authHost :: String,
    authPort :: Int,
    bindUser :: String,
    bindPassword :: String
    }

genericSettings :: String -> String -> String -> Settings
genericSettings domain user password =
    Settings domain domain 389 user password

data Sustain = Sustain {
    getStatic :: Static,
    httpManager :: Manager,
    settings :: Settings
    }

mkYesodData "Sustain" [parseRoutes|
    /static          StaticR      Static getStatic
    /auth            AuthR        Auth getAuth
    /                HomeR        GET
    /api/stabilize   StabilizeR   POST
    /api/remove      RemoveR      POST
    /api/sync        SyncR        POST
|]
