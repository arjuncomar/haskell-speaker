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
    , ConnType(..)
    , _Postgresql
    , _Sqlite
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

data ConnType = Postgresql | Sqlite deriving (Show, Eq)
makePrisms ''ConnType

data Config = Config {
    _connType :: ConnType,
    _connPool :: ConnectionPool
}
makeLenses ''Config

-- looks in the same directory as the executable for a config file
-- named speaker.conf in ini format.
withConfig :: (MonadBaseControl IO m, MonadIO m) => (Config -> m a) -> m a
withConfig act = runNoLoggingT $ do
  conf <- liftIO $ readIniFile "./speaker.conf"
  let (connStr, connType, connCount) = either defaultSettings id $ do
                                          iniConf <- conf
                                          connStr   <- extractConfValue "db.conn.string" iniConf
                                          connType  <- extractConfValue "db.type"        iniConf
                                          connCount <- extractConfValue "db.pool.size"   iniConf
                                          return (connStr, connType, read connCount) 

  case connType of
    "postgresql" -> P.withPostgresqlPool (fromString connStr) connCount $ runWithPool act Postgresql
    "sqlite" -> S.withSqlitePool (fromString connStr) connCount $ runWithPool act Sqlite
    _ -> liftIO . throwIO $ userError "Invalid connection type." {- There's no way to gracefully fail here.
                                                                   We can't open the db connection. -}
    where 
      runWithPool act t pool = lift . act $ Config t pool
      defaultSettings = const (":memory:", "sqlite", 1)
      extractConfValue key iniConf = Text.unpack <$> lookupValue "Speaker" key iniConf

runMigrationIO :: (MonadIO m, MonadBaseControl IO m) => Config -> Migration -> m ()
runMigrationIO c m = withResource (c^.connPool) $ runReaderT (migrateFunc c m)
  where
    migrateFunc c = case c^.connType of
      Postgresql -> P.runMigration
      Sqlite -> S.runMigration
