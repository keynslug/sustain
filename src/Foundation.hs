-- Foundation

module Foundation where

import Imports
import Yesod.Static
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Auth.LDAP
import LDAP (LDAPScope(..), LDAPEntry(..))
import Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import Data.Text.Lazy (toStrict)
import Text.Jasmine (minifym)
import Text.Blaze.Renderer.Text (renderMarkup)

data Sustain = Sustain { getStatic :: Static, httpManager :: Manager }

mkYesodData "Sustain" [parseRoutes|
    /static          StaticR      Static getStatic
    /auth            AuthR        Auth getAuth
    /                HomeR        GET
    /api/stabilize   StabilizeR   POST
    /api/remove      RemoveR      POST
    /api/sync        SyncR        POST
|]

--
-- Core type

instance Yesod Sustain where

    defaultLayout contents = do
        mmsg <- getMessage
        let msg = String $ maybe mempty (toStrict . renderMarkup) mmsg
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

    authPlugins _ = [ genericAuthLDAP LDAPConfig {
        usernameModifier = id,
        nameToDN = \n -> "uid=" ++ unpack n ++ "," ++ baseDomain,
        identifierModifier = \n (e:_) -> maybe n id $ getCommonName e,
        ldapHost = "keynfawkes.org",
        ldapPort' = 389,
        initDN = "cn=admin,dc=keynfawkes,dc=org",
        initPass = "lowman",
        baseDN = Just baseDomain,
        ldapScope = LdapScopeSubtree
        } ] where
            baseDomain = "ou=people,dc=keynfawkes,dc=org"

    authHttpManager = httpManager

    maybeAuthId = lookupSession "_ID"

getCommonName :: LDAPEntry -> Maybe Text
getCommonName (LDAPEntry _ attrs) =
    case map snd $ dropWhile ((/= "cn") . fst) attrs of
        (ns:_) -> Just $ pack $ head ns
        _      -> Nothing

--
-- Foundation

makeFoundation :: IO Sustain
makeFoundation = 
    Sustain <$>
        static staticDir <*>
        newManager conduitManagerSettings
