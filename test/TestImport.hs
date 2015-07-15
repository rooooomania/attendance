module TestImport
    ( module TestImport
    , module X
    ) where

import           Application                    (makeFoundation)
import           ClassyPrelude                  as X
import           Database.Persist               as X hiding (get)
import           Database.Persist.Sql           (SqlBackend, SqlPersistM,
                                                 connEscapeName, rawExecute,
                                                 rawSql, runSqlPersistMPool,
                                                 unSingle)
import           Foundation                     as X
import           Model                          as X
import           Test.Hspec                     as X hiding (expectationFailure,
                                                      shouldBe, shouldContain,
                                                      shouldMatchList,
                                                      shouldReturn,
                                                      shouldSatisfy)
import           Text.Shakespeare.Text          (st)
import           Yesod.Default.Config2          (ignoreEnv, loadAppSettings)
import           Yesod.Test                     as X

-- Wiping the database
import           Control.Monad.Logger           (runLoggingT)
import           Database.Persist.Sqlite        (createSqlPool, sqlDatabase,
                                                 wrapConnection)
import qualified Database.Sqlite                as Sqlite
import           Settings                       (appDatabaseConf, appRoot)
import           Test.DocTest                   as X
import           Test.Hspec.Expectations.Lifted as X
import           Test.Hspec.QuickCheck          as X
import           Yesod.Core                     (messageLoggerSource)
import           Yesod.Persist.Core             as X (get404, getBy404)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

withApp :: SpecWith App -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    return foundation

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    -- In order to wipe the database, we need to temporarily disable foreign key checks.
    -- Unfortunately, disabling FK checks in a transaction is a noop in SQLite.
    -- Normal Persistent functions will wrap your SQL in a transaction,
    -- so we create a raw SQLite connection to disable foreign keys.
    -- Foreign key checks are per-connection, so this won't effect queries outside this function.

    -- Aside: SQLite by default *does not enable foreign key checks*
    -- (disabling foreign keys is only necessary for those who specifically enable them).
    let settings = appSettings app
    sqliteConn <- rawConnection (sqlDatabase $ appDatabaseConf settings)
    disableForeignKeys sqliteConn

    let logFunc = messageLoggerSource app (appLogger app)
    pool <- runLoggingT (createSqlPool (wrapConnection sqliteConn) 1) logFunc

    flip runSqlPersistMPool pool $ do
        tables <- getTables
        sqlBackend <- ask
        let queries = map (\t -> "DELETE FROM " ++ (connEscapeName sqlBackend $ DBName t)) tables
        forM_ queries (\q -> rawExecute q [])

rawConnection :: Text -> IO Sqlite.Connection
rawConnection t = Sqlite.open t

disableForeignKeys :: Sqlite.Connection -> IO ()
disableForeignKeys conn = Sqlite.prepare conn "PRAGMA foreign_keys = OFF;" >>= void . Sqlite.step

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    return (fmap unSingle tables)

authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    root <- appRoot . appSettings <$> getTestYesod

    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl $ root ++ "/auth/page/dummy"
