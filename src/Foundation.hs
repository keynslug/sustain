-- Foundation

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foundation where

import Imports
import Routes
import Layout as Layout

import Prelude (Char, String, fromIntegral)
import Data.Text.Lazy (toStrict)
import Text.Jasmine (minifym)
import Text.Blaze.Renderer.Text (renderMarkup)
import Yesod.Auth.LDAP
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (conduitManagerSettings, newManager)

--
-- Core type

instance Yesod Sustain where

    defaultLayout contents = do
        mmsg <- getMessage
        let msg = maybe mempty (toStrict . renderMarkup) mmsg
        pc <- widgetToPageContent $ Layout.page msg contents
        giveUrlRenderer $(hamletFile "template/layout.hamlet")

    addStaticContent =
        addStaticContentExternal minifym genFileName staticDir (StaticR . flip StaticRoute []) where
            genFileName = base64md5

    authRoute _ = Just $ AuthR LoginR

    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized _ False     = return Authorized
    isAuthorized _ True      = do
        mu <- maybeAuthId
        return $ case mu of
            Nothing -> AuthenticationRequired
            Just _ -> Authorized

staticDir :: FilePath
staticDir = "static"

instance RenderMessage Sustain FormMessage where
    renderMessage _ _ = defaultFormMessage

--
-- Auth

instance YesodAuth Sustain where

    type AuthId Sustain = Text
    getAuthId = return . Just . credsIdent

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authLayout = defaultLayout . Layout.authLayout
    authHttpManager = httpManager

    maybeAuthId = lookupSession "_ID"

    authPlugins master = [ genericAuthLDAP LDAPConfig {
        usernameModifier = id,
        nameToDN = qualify . unpack,
        identifierModifier = getIndentifier,
        ldapHost = authHost s,
        ldapPort' = fromIntegral $ authPort s,
        initDN = qualify $ bindUser s,
        initPass = bindPassword s,
        baseDN = Just $ fromFragments searchDomain,
        ldapScope = LdapScopeSubtree
        } ] where
            qualify n = mappend n ('@':domain)
            domain = ldapDomain s
            domainFragments = splitOn '.' domain
            baseDomain = zip (repeat "dc") domainFragments
            searchDomain = ("cn", "users") : baseDomain
            s = settings master

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : splitOn c rest
        where (w, rest) = break (== c) s'

fromFragments :: [(String, String)] -> String
fromFragments [] = ""
fromFragments fs = tail $ concatMap ((',' :) . join) fs where
    join (n, v) = n ++ "=" ++ v

getIndentifier :: Text -> [LDAPEntry] -> Text
getIndentifier n (e:_) = maybe n id $ getCommonName e
getIndentifier n _     = n

getCommonName :: LDAPEntry -> Maybe Text
getCommonName (LDAPEntry _ attrs) =
    case map snd $ dropWhile ((/= "cn") . fst) attrs of
        (ns:_) -> Just $ pack $ head ns
        _      -> Nothing

--
-- Layouts

homeLayout :: Widget -> Handler Html
homeLayout contents = do
    mauth <- maybeAuthId
    defaultLayout $ Layout.homeLayout mauth contents

--
-- Foundation

makeFoundation :: IO Sustain
makeFoundation =
    Sustain <$>
        static staticDir <*>
        newManager conduitManagerSettings <*>
        return (Settings
            "int.platbox.com"
            "localhost" 10389
            "test111" "Qweasd123"
            )
