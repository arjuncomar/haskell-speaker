{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Speaker.User.Controller
    ( UsersAPI
    , usersApi
    ) where

import Speaker.User.Model
import Speaker.User.Repository
import Speaker.Utils
import Speaker.Config
import Servant
import Control.Lens
import Data.Monoid
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Function


{- 
 - User API: GET /v/users        -- list all users
 -           GET /v/users/userId -- get the user with uid userId
 -}
type UsersAPI = "v" :> "users" :> (GetAllUsers :<|> GetUser)
type GetAllUsers = Get '[JSON] [User]
type GetUser = Capture "userId" Int :> Get '[JSON] User

usersApi :: Config -> Server UsersAPI
usersApi c = enter (runSpeaker c) $ getAllUsers :<|> getUser

getAllUsers :: Speaker [User]
getAllUsers = runDB getUsersDB

getUser :: Int -> Speaker User
getUser uid = do
  muser <- runDB $ getUserDB uid
  case muser of
    Just user -> return user
    Nothing -> throwError err404 { 
      errBody = "No user found with userid = " <> toByteString uid 
    }
