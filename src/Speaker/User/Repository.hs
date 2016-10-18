{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
module Speaker.User.Repository
    ( getUsersDB
    , getUserById
    , getUserByEmail
    , testUsers
    ) where

import Speaker.Utils
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Control.Lens
import Crypto.PasswordStore
import System.IO.Unsafe(unsafePerformIO)


testUsers :: [User]
testUsers = [ mkUser "Isaac" "Newton" "isaac.newton@example.com" $ unsafeMakePassword "123456"
            , mkUser "Albert" "Einstein" "albert.einstein@example.com" $ unsafeMakePassword "123456"
            ]
getUsersDB :: (MonadReader Config m, MonadIO m) => SqlPersistT m [User]
getUsersDB = do
    entities <- selectList [] []
    return [user | (Entity _ user) <- entities]

getUserById :: (MonadReader Config m, MonadIO m) => Int -> SqlPersistT m (Maybe User)
getUserById = get . toSqlKey . fromIntegral

getUserByEmail :: (MonadReader Config m, MonadIO m) => T.Text -> SqlPersistT m (Maybe User)
getUserByEmail email = do 
  muser <- getBy $ UniqueEmail email
  return $ muser^?_Just.eVal

unsafeMakePassword :: B.ByteString -> T.Text
unsafeMakePassword = T.decodeUtf8 . unsafePerformIO . flip makePassword 17
