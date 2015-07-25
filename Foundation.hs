module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool, toSqlKey, fromSqlKey)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Yesod.Auth.Dummy


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        60    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        (title', parents) <- breadcrumbs

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
            addScript $ StaticR bootstrap_js
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized

    isAuthorized route isWrite = do
        mauth <- maybeAuth
        runDB $ mauth `isAuthorizedTo` permissionRequiredFor route isWrite

    -- 承認リスト画面は、自分の承認リストのみ参照できる
    -- isAuthorized (ApproverR uid) _ = do
    --     mauth <- maybeAuth
    --     case mauth of
    --         Nothing -> return AuthenticationRequired
    --         Just (Entity approverId _) ->
    --             return $ if uid == approverId then Authorized else Unauthorized ("このページは表示できません" :: Text)
    -- -- 休暇申請一覧は、管理者のみ参照できる
    -- isAuthorized RequestsR _ = do
    --     mauth <- maybeAuth
    --     case mauth of
    --         Nothing -> return AuthenticationRequired
    --         Just (Entity uid u)
    --             | isAdmin u -> return Authorized
    --             | otherwise -> return $ Unauthorized ("このページは表示できません" :: Text)
    -- -- 一般ユーザは、自分の休暇申請のみ参照できる
    -- isAuthorized (RequestR rid) _ = do
    --     mauth <- maybeAuth
    --     case mauth of
    --         Nothing -> return AuthenticationRequired
    --         Just (Entity uid u) -> do
    --             r <- runDB $ get404 rid
    --             case isAdmin u of
    --                 True -> return Authorized
    --                 _    -> if holidayRequestUser r == uid
    --                             then return Authorized
    --                             else return $ Unauthorized ("このページは表示できません" :: Text)
    --
    -- Write メソッドは、基本的に認証が必要
    isAuthorized _ True = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just (Entity uid _) -> return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- =====================
-- | 認可制御を抽象化して、単体テストしやすいようにする
data Permission =
      ViewApprover UserId
    | ViewRequests
    | ViewRequest HolidayRequestId
    | ViewUsers

permissionRequiredFor :: Route App -> Bool -> [Permission]
permissionRequiredFor (ApproverR uid) _ = [ViewApprover uid]
permissionRequiredFor RequestsR _ = [ViewRequests]
permissionRequiredFor (RequestR rid) _ = [ViewRequest rid]

permissionRequiredFor (AuthR _) _ = []
permissionRequiredFor FaviconR _ = []
permissionRequiredFor RobotsR _ = []
permissionRequiredFor RootR _ = []
permissionRequiredFor (CancelR _ _) _ = []
permissionRequiredFor UsersR _ = [ViewUsers]
permissionRequiredFor (ApproveR _ _) _ = []
permissionRequiredFor (User _) _ = []



hasPermissionTo :: Entity User -> Permission -> YesodDB App AuthResult
u `hasPermissionTo` (ViewApprover uid) =
    if entityKey u == uid
        then return Authorized
        else  return $ Unauthorized ("このページは表示できません" :: Text)
u `hasPermissionTo` ViewRequests
    | isAdmin (entityVal u) = return Authorized
    | otherwise = return $ Unauthorized ("このページは表示できません" :: Text)

u `hasPermissionTo` (ViewRequest rid) =
    if isAdmin (entityVal u)
        then return Authorized
        else do
            r <- get404 rid
            if entityKey u == holidayRequestUser r
                then return Authorized
                else return $ Unauthorized ("このページは表示できません" :: Text)
u `hasPermissionTo` ViewUsers
    | isAdmin (entityVal u) = return Authorized
    | otherwise = return $ Unauthorized ("このページは表示できません" :: Text)

isAuthorizedTo :: Maybe (Entity User) -> [Permission] -> YesodDB App AuthResult
_           `isAuthorizedTo` []     = return Authorized
Nothing     `isAuthorizedTo` (_:_)  = return AuthenticationRequired
Just u      `isAuthorizedTo` (p:ps) = do
    r <- u `hasPermissionTo` p
    case r of
        Authorized -> Just u `isAuthorizedTo` ps
        _ -> return r -- unauthorized



isAdmin :: User -> Bool
isAdmin = userAdmin

userIdFromToken :: Handler (Maybe (Key User))
userIdFromToken = do
    mtoken <- lookupGetParam "token"
    case mtoken of
        Nothing -> return Nothing
        _ -> do
            mu <- runDB $ selectFirst [UserPassword ==.  mtoken] []
            case mu of
                Just (Entity uid _) -> return $ Just uid
                _ -> return Nothing

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    maybeAuthId = do
        muid <- userIdFromToken
        case muid of
            Nothing -> defaultMaybeAuthId
            _       -> return muid

    getAuthId creds = do
        Entity uid _ <- runDB $ getBy404 $ UniqueUser $ credsIdent creds
        return $ Just uid
        -- ユーザ登録していなければ、新たにユーザを作成する
        -- case x of
        --     Just (Entity uid _) -> return $ Just uid
        --     Nothing -> Just <$> insert User
        --         { userIdent = credsIdent creds
        --         , userPassword = Nothing
        --         , userFullName = Nothing
        --         , userNicname = Nothing
        --         , userApprover = Nothing
        --         , userAdmin = False
        --         }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins m = authPluginBackDoor m [authBrowserId def]

    authHttpManager = getHttpManager

authPluginBackDoor :: App -> [AuthPlugin App] -> [AuthPlugin App]
authPluginBackDoor app =
    if appAllowDummyAuth (appSettings app) then (authDummy : ) else id


instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

instance YesodBreadcrumbs App where
    breadcrumb RootR = return ("トップページ", Nothing)
    breadcrumb UsersR = return ("ユーザ一覧", Nothing)
    breadcrumb (UserR uid) = do
        u <- runDB $ get404 uid
        if isAdmin u
            then return (userIdent u, Just RootR)
            else return (userIdent u, Just UsersR)
    breadcrumb RequestsR = return ("休暇申請リクエスト一覧", Just RootR)
    breadcrumb (RequestR hid) =
        return ("休暇申請番号" ++ (pack. show) (fromSqlKey hid), Just RootR)
    breadcrumb AuthR{} = return ("", Nothing)
    breadcrumb (ApproverR uid) = return ("承認対象一覧", Just RootR)

    breadcrumb (ApproveR uid hid) = return ("", Nothing)
    breadcrumb (RejectR uid hid) = return ("", Nothing)
