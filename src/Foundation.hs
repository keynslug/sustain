-- Foundation

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foundation where

import Imports
import Settings
import Routes
import Layout as Layout

import Prelude (Char, String, fromIntegral)
import Data.Text (breakOn, stripPrefix)
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
    isAuthorized _ True = do
        s <- settings <$> getYesod
        mu <- maybeAuthId
        return $ case mu of
            Nothing -> AuthenticationRequired
            Just (uid, _) ->
                if elem uid $ privilegedUsers s
                    then Authorized
                    else AuthenticationRequired

staticDir :: FilePath
staticDir = "static"

instance RenderMessage Sustain FormMessage where
    renderMessage _ _ = defaultFormMessage

--
-- Auth

instance PathPiece AuthParams where
    toPathPiece (uid, uname) = mconcat [uid, "::", uname]
    fromPathPiece s =
        let (uid, uname) = breakOn "::" s in
            (,) uid <$> stripPrefix "::" uname

instance YesodAuth Sustain where

    type AuthId Sustain = AuthParams
    getAuthId cs = return $ let uid = credsIdent cs in Just (uid, getUserName uid cs)

    loginDest _ = HomeR
    logoutDest _ = AuthR LoginR

    authLayout = withAuthLayout
    authHttpManager = httpManager

    maybeAuthId = do
        auth <- lookupSession "_ID"
        return $ maybe Nothing fromPathPiece auth

    authPlugins master = [ genericAuthLDAP LDAPConfig {
        usernameModifier = id,
        nameToDN = qualify . unpack,
        identifierModifier = const,
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

getUserName :: (Yesod m) => Text -> Creds m -> Text
getUserName uid e = maybe uid id $ getCommonName $ credsExtra e where
    getCommonName [] = Nothing
    getCommonName ((n, v) : attrs)
        | n == "cn" = Just v
        | otherwise = getCommonName attrs

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
