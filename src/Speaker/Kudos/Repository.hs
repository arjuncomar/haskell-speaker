{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
module Speaker.Kudos.Repository
    ( getKudosDB
    , getKudoDB
    , testKudos
    ) where

import Speaker.Kudos.Category
import Speaker.Kudos.Model
import Speaker.Config
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Logger

testKudos :: [Kudos]
testKudos = [ mkKudos "Hey!" 1 2 Teamwork
            , mkKudos "Hello, hello!" 2 1 Teamwork
            ]

getKudosDB :: (MonadReader Config m, MonadIO m) => SqlPersistT m [Kudos]
getKudosDB = do
    entities <- selectList [] []
    return [kudo | (Entity _ kudo) <- entities]

getKudoDB :: (MonadReader Config m, MonadIO m) => Int -> SqlPersistT m (Maybe Kudos)
getKudoDB = get . toSqlKey . fromIntegral
