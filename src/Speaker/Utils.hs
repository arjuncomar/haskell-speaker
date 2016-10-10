{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
module Speaker.Utils
    ( toByteString
    , runDB
    , Speaker
    , runSpeaker
    ) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Control.Monad.Reader.Class as RC
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Sqlite
import Speaker.Config
import Control.Lens
import Servant

toByteString :: Show s => s -> L.ByteString
toByteString = B.toLazyByteString . B.stringUtf8 . show

type Speaker = ReaderT Config Handler

runSpeaker' :: Config -> (forall a. Speaker a -> Handler a)
runSpeaker' c s = runReaderT s c

runSpeaker :: Config -> Speaker :~> Handler
runSpeaker c = Nat $ runSpeaker' c

runDB :: (MonadBaseControl IO m, MonadReader Config m, MonadIO m) => SqlPersistT m b -> m b
runDB query = do
  pool <- RC.asks $ view connPool
  runSqlPool query pool
