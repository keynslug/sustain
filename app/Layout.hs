-- Layout

module Layout where

import Imports hiding (fileName)
import Routes
import Package

import Prelude (show)
import Data.Char (toLower)
import Data.Aeson (encode)
import Data.Text.Lazy.Encoding (decodeUtf8)

page :: Text -> Widget -> Widget
page message contents = do
    let msg = String message
    setTitle "Aptly Sustain"
    addStylesheet (StaticR $ StaticRoute ["css", "bootstrap.css"] [])
    addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
    addScript (StaticR $ StaticRoute ["js", "bootstrap.js"] [])
    toWidget $(luciusFile "app/template/basic.lucius")
    toWidget $(juliusFile "app/template/main.julius")
    contents

authLayout :: Widget -> Widget
authLayout widget = do
    toWidget $(luciusFile "app/template/auth.lucius")
    withHeader mempty
    widget

homeLayout :: Maybe AuthParams -> Widget -> Widget
homeLayout mauth widget =
    withHeader [whamlet|
        <a ."do sync" data-section="Testing">Sync Testing
        <a ."do sync" data-section="Stable">Sync Stable
        <a ."do cleanup">Cleanup
        $maybe (_, username) <- mauth
            <span ."login">
                <a href="@{AuthR LogoutR}">
                    <span ."glyphicon glyphicon-log-out">
                #{username}
        $nothing
            <a ."login" href="@{AuthR LoginR}">
                <span ."glyphicon glyphicon-log-in">
                Log in
    |] >> widget

withHeader :: Widget -> Widget
withHeader headings =
    [whamlet|
        <header>
            <div .heading>
                <h1>
                    <a href=@{HomeR}>Aptly Sustain
                ^{headings}
            <div #statusbar .ok>
                <span .text>TEST TEXT please ignore
                <a ."close">Dismiss
    |]

withContent :: Widget -> Widget
withContent widget = [whamlet|
    <div #content>
        ^{widget}
        <div .clearfix>
        <footer>
            <p>© 2014 Platbox
|]

packageList :: PackageList -> Widget
packageList [] = do
    [whamlet|
        <div .c0>
            <div .pkglist>
                <h3>Nothing to look at yet, buddy!
    |]
packageList ps = do
    let groupedPs = groupBy ((==) `on` name) $ sort ps
    [whamlet|
        <div .c1>
            <div .navsidebar .affix>
                <div .wrapper>
                    $forall ps <- groupedPs
                        $with c <- captionPackages ps
                            <a href="##{c}">
                                <div data-name="#{c}" .group>
                                    #{c}
                                    <i .number>#{length ps}
                <div .underscan>
                <div .clearfix>
        <div .c2>
            <div .pkglist>
                ^{mapM_ packageSection groupedPs}
    |]

packageSection :: PackageList -> Widget
packageSection ps =
    let
        caption = captionPackages ps
        showSection = map toLower . show
        rps = reverse ps
        ident = decodeUtf8 . encode . identifier
    in [whamlet|
        <h3 ##{caption}>#{caption}
        <ul>
            $forall p <- rps
                $with s <- section p
                    <li data-pkgid="#{ident p}" data-ref="#{ref p}" data-name="#{name p}" data-section="#{show s}" .#{showSection s}>
                        $if s == Testing
                            <div ."btn btn-tooltip btn-primary" data-toggle="tooltip" data-placement="bottom" data-do="stabilize" title="Stabilize">
                                <span ."glyphicon glyphicon-chevron-up">
                        $else
                            <div ."btn btn-tooltip btn-success" data-toggle="tooltip" data-placement="bottom" title="Stable">
                                <span ."glyphicon glyphicon-ok">
                        <div ."btn btn-tooltip btn-danger" data-toggle="tooltip" data-placement="bottom" data-do="remove" title="Remove">
                            <span ."glyphicon glyphicon-remove">
                        <span .name>#{name p}
                        <span .version>#{version p}
                        <span .platform>#{platform p}
    |]

captionPackages :: PackageList -> Text
captionPackages = name . head
