-- Layout

module Layout where

import Imports
import Foundation
import Package

withContent :: Widget -> Widget
withContent widget = [whamlet|
    <div #header>
        <h1>Aptly Sustain
        <div #statusbar .affix-top .ok>
            <p>TEST TEXT please ignore
    <div #content>
        ^{widget}
        <div .clearfix>
        <div #footer>
            <p>© 2014 Platbox
|]

packageList :: PackageList -> Widget
packageList ps = do
    let packages = groupBy ((==) `on` name) $ sort ps
    let grouped = zip (map (name . head) packages) packages
    [whamlet|
        <div .c1>
            <div .navsidebar .affix-top>
                <div .wrapper>
                    $forall (caption, ps) <- grouped
                        <a href="##{caption}">
                            <div .group>
                                #{caption}
                                <i .number>#{length ps}
                <div .clearfix>
        <div .c2>
            <div .pkglist>
                ^{mapM_ (uncurry packageSection) grouped}
    |]

packageSection :: Text -> PackageList -> Widget
packageSection caption ps =
    [whamlet|
        <h3 id="#{caption}">#{caption}
        <ul>
            $forall p <- ps
                <li id=#{fullName p}>
                    <div ."btn btn-tooltip btn-primary" data-toggle="tooltip" data-placement="bottom" data-do="stabilize" title="Stabilize">
                        <span ."glyphicon glyphicon-chevron-up">
                    <div ."btn btn-tooltip btn-danger" data-toggle="tooltip" data-placement="bottom" data-do="remove" title="Remove">
                        <span ."glyphicon glyphicon-remove">
                    <span .name>#{name p}
                    <span .version>#{version p}
                    <span .platform>#{platform p}
    |]
