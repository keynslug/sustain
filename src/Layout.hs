-- Layout

module Layout where

import Imports
import Foundation
import Package
import Aptly

import Prelude (show)
import Data.Char (toLower)

withContent :: Widget -> Widget
withContent widget = [whamlet|
    <div #header>
        <h1>
            <a href=@{HomeR}>Aptly Sustain
    <div #statusbar .affix-top .ok>
        <span .text>TEST TEXT please ignore
        <a ."close">Dismiss
    <div #content>
        ^{widget}
        <div .clearfix>
        <div #footer>
            <p>© 2014 Platbox
|]

packageList :: PackageList -> Widget
packageList ps = do
    let groupedPs = groupBy ((==) `on` name) $ sort ps
    [whamlet|
        <div .c1>
            <div .navsidebar .affix-top>
                <div .wrapper>
                    $forall ps <- groupedPs
                        $with c <- captionPackages ps
                            <a href="##{c}">
                                <div data-name="#{c}" .group>
                                    #{c}
                                    <i .number>#{length ps}
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
    in [whamlet|
        <h3 ##{caption}>#{caption}
        <ul>
            $forall p <- rps
                $with s <- section p
                    <li data-pkgname="#{fullName p}" data-name="#{name p}" data-section="#{show s}" .#{showSection s}>
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
