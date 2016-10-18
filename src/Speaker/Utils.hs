{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Speaker.Utils
    ( toByteString
    , runDB
    , Speaker
    , runSpeaker
    , speakerSqlSettings
    , eKey, eVal
    , throwError'
    ) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Control.Monad.Reader.Class as RC
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Database.Persist.Sqlite
import Speaker.Config
import Control.Lens
import Servant

eKey :: Lens' (Entity r) (Key r)
eKey = lens entityKey $ \e k -> e { entityKey = k }

eVal :: Lens' (Entity r) r
eVal = lens entityVal $ \e v -> e { entityVal = v }

speakerSqlSettings :: MkPersistSettings
speakerSqlSettings = sqlSettings { mpsGenerateLenses = True }

toByteString :: Show s => s -> L.ByteString
toByteString = B.toLazyByteString . B.stringUtf8 . show

type Speaker = ReaderT Config Handler

runSpeaker :: Config -> Speaker :~> Handler
runSpeaker c = Nat $ flip runReaderT c

runDB :: (MonadBaseControl IO m, MonadReader Config m, MonadIO m) => SqlPersistT m b -> m b
runDB query = do
  pool <- RC.asks $ view connPool
  runSqlPool query pool

throwError' :: ServantErr -> Speaker a
throwError' = throwError
