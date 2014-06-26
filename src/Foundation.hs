-- Foundation

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foundation where

import Imports
import Settings
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
    logoutDest _ = AuthR LoginR

    authLayout = withAuthLayout
    authHttpManager = httpManager

    maybeAuthId = lookupSession "_ID"

    authPlugins master = [ genericAuthLDAP LDAPConfig {
        usernameModifier = id,
        nameToDN = qualify . unpack,
        identifierModifier = getIndentifier,
        ldapUri = authUri s,
        initDN = qualify $ bindUser s,
        initPass = bindPassword s,
        baseDN = Just $ fromFragments searchDomain,
        ldapScope = LdapScopeSubtree
        } ] where
            qualify n = mappend n ('@' : ldapDomain s)
            domainFragments = ldapDomainFrags s
            baseDomain = zip (repeat "dc") domainFragments
            searchDomain = ("cn", "users") : baseDomain
            s = settings master

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

withHomeLayout :: Widget -> Handler Html
withHomeLayout contents = do
    mauth <- maybeAuthId
    defaultLayout $ Layout.homeLayout mauth contents

withAuthLayout :: Widget -> Handler Html
withAuthLayout contents = do
    mauth <- maybeAuthId
    maybe
        (defaultLayout $ Layout.authLayout contents)
        (const $ redirect HomeR)
        mauth

--
-- Foundation

makeFoundation :: Settings -> IO Sustain
makeFoundation s = Sustain
    <$> static staticDir
    <*> newManager conduitManagerSettings
    <*> return s
