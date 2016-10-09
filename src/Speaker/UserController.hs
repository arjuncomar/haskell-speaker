{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Speaker.UserController
    ( UsersAPI
    , usersApi
    ) where

import Speaker.User
import Speaker.UserRepository
import Speaker.Utils
import Servant
import Control.Lens
import Data.Monoid


type UsersAPI = GetAllUsers :<|> GetUser
type GetAllUsers = "users" :> Get '[JSON] [User]
type GetUser = "users" :> Capture "userId" Int :> Get '[JSON] User

usersApi :: Server UsersAPI
usersApi = getAllUsers :<|> getUser

getAllUsers :: Server GetAllUsers
getAllUsers = return users

getUser :: Server GetUser
getUser uid = case muser of
                Just user -> return user
                Nothing -> throwError err404 { 
                  errBody = "No user found with userid = " <> toByteString uid 
                }
  where muser = users ^? folded.filtered ((== uid) . view userId)
