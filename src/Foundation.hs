-- Foundation

module Foundation where

import Imports
import Yesod.Static
import Yesod.Default.Util (addStaticContentExternal)
import Text.Jasmine (minifym)

data Sustain = Sustain { getStatic :: Static }

mkYesodData "Sustain" [parseRoutes|
    /static StaticR Static getStatic
    /                HomeR        GET
    /api/stabilize   StabilizeR   POST
    /api/remove      RemoveR      POST
|]

instance Yesod Sustain where

    defaultLayout contents = do
        pc <- widgetToPageContent $ do
            addStylesheet (StaticR $ StaticRoute ["css", "bootstrap.css"] [])
            addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
            addScript (StaticR $ StaticRoute ["js", "bootstrap.js"] [])
            toWidget $(luciusFile "template/basic.lucius")
            toWidget $(juliusFile "template/main.julius")
            contents
        giveUrlRenderer $(hamletFile "template/layout.hamlet")

    addStaticContent =
        addStaticContentExternal (Right . id) genFileName staticDir (StaticR . flip StaticRoute []) where
            genFileName = base64md5

staticDir :: FilePath
staticDir = "static"

makeFoundation :: IO Sustain
makeFoundation = Sustain <$>
    static staticDir
