{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
module Speaker.User.Repository
    ( getUsersDB
    , getUserDB
    , testUsers
    ) where

import Speaker.User.Model
import Speaker.Config
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Data.Foldable


testUsers :: [User]
testUsers = [ mkUser "Isaac" "Newton"
            , mkUser "Albert" "Einstein"
            ]
getUsersDB :: (MonadReader Config m, MonadIO m) => SqlPersistT m [User]
getUsersDB = do
    entities <- selectList [] []
    return [user | (Entity _ user) <- entities]

getUserDB :: (MonadReader Config m, MonadIO m) => Int -> SqlPersistT m (Maybe User)
getUserDB = get . toSqlKey . fromIntegral
