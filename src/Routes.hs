-- Routes

module Routes where

import Imports
import Network.HTTP.Conduit (Manager)

data Sustain = Sustain { getStatic :: Static, httpManager :: Manager }

mkYesodData "Sustain" [parseRoutes|
    /static          StaticR      Static getStatic
    /auth            AuthR        Auth getAuth
    /                HomeR        GET
    /api/stabilize   StabilizeR   POST
    /api/remove      RemoveR      POST
    /api/sync        SyncR        POST
|]
