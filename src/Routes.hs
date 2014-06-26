-- Routes

module Routes where

import Imports
import Settings
import Network.HTTP.Conduit (Manager)

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
    /api/cleanup     CleanupR     POST
|]
