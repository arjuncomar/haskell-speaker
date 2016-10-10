{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Speaker.Config
    ( Config(..)
    , withConfig
    , connPool
    , connType
    , runMigrationIO
    ) where
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Exception (throwIO)
import Control.Applicative
import Control.Lens
import Data.Ini
import Data.String
import Database.Persist 
import Database.Persist.Sql
import Data.Pool
import qualified Database.Persist.Postgresql as P
import qualified Database.Persist.Sqlite as S
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as H

data Config = Config {
    _connType :: String
  , _connPool :: ConnectionPool
}
makeLenses ''Config

-- looks in the same directory as the executable for a config file
-- named speaker.conf in ini format.
withConfig :: (MonadBaseControl IO m, MonadIO m) => (Config -> m a) -> m a
withConfig act = runNoLoggingT $ do
  conf <- liftIO $ readIniFile "./speaker.conf"
  (connStr, connType, connCount) <- case conf of
    Left _ -> return (defConnStr, defConnType, read defConnCount)
    Right iniConf -> do 
      let connStr = fromConf defConnStr "db.conn.string" iniConf
      let connType = fromConf defConnType "db.type" iniConf
      let connCount = read $ fromConf defConnCount "db.pool.size" iniConf
      return (connStr, connType, connCount)

  case connType of
    "postgresql" -> P.withPostgresqlPool (fromString connStr) connCount $ runWithPool act connType
    "sqlite" -> S.withSqlitePool (fromString connStr) connCount $ runWithPool act connType
    _ -> liftIO . throwIO $ userError "Invalid connection type." {- There's no way to gracefully fail here.
                                                                   We can't open the db connection. -}
    where defConnStr = ":memory:"
          defConnType = "sqlite"
          defConnCount = "1"
          fromConf def key iniConf = either (const def) Text.unpack $ lookupValue "Speaker" key iniConf
          runWithPool act t pool = lift . act $ Config t pool

runMigrationConfig :: MonadIO m => Config -> Migration -> ReaderT SqlBackend m ()
runMigrationConfig c = case c ^. connType of
  "postgresql" -> P.runMigration
  "sqlite" -> S.runMigration
  -- The following case should be impossible
  _ -> const . liftIO . throwIO $ userError "Invalid connection type, could not migrate the db."

runMigrationIO :: Config -> Migration -> IO ()
runMigrationIO c m = withResource (c ^. connPool) $ runReaderT (runMigrationConfig c m)
