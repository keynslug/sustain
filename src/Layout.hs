-- Layout

module Layout where

import Imports
import Foundation
import Package

withContent :: Widget -> Widget
withContent widget = $(whamletFile "template/main.hamlet")

packageList :: Text -> PackageList -> Widget
packageList caption ps = [whamlet|
    <h3>#{caption}
    <ul>
        $forall p <- ps
            <li id=#{fullName p}>
                <div ."btn btn-tooltip btn-primary" data-toggle="tooltip" data-placement="bottom" title="Stabilize">
                    <span ."glyphicon glyphicon-upload">
                <div ."btn btn-tooltip btn-danger" data-toggle="tooltip" data-placement="bottom" title="Remove">
                    <span ."glyphicon glyphicon-remove-circle">
                <span .name>#{name p}
                <span .version>#{version p}
                <span .platform>#{platform p}
|]
