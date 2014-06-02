-- Layout

module Layout where

import Imports
import Package

--

type Widget site = WidgetT site IO ()
type Widgeter site = Widget site -> Widget site

defaultLayout :: (Yesod site) => Widget site -> HandlerT site IO Html
defaultLayout contents = do
    pc <- widgetToPageContent $ do
        toWidget $(luciusFile "template/normalise.lucius")
        toWidget $(luciusFile "template/basic.lucius")
        contents
    giveUrlRenderer $(hamletFile "template/layout.hamlet")

withContent :: (Yesod site) => Widgeter site
withContent widget = $(whamletFile "template/main.hamlet")

packageList :: (Yesod site) => Text -> PackageList -> Widget site
packageList caption ps = [whamlet|
    <h3>#{caption}
    <ul>
        $forall p <- ps
            <li id=#{fullName p}>
                <span class=name>#{name p}
                <span class=version>#{version p}
                <span class=platform>#{platform p}
|]
