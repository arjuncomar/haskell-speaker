{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Lens
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid

data User = User
  { _userId        :: Int
  , _userFirstName :: String
  , _userLastName  :: String
  } deriving (Eq, Show)
makeLenses ''User
$(deriveJSON defaultOptions ''User)

type API = UsersAPI
type UsersAPI = GetAllUsers :<|> GetUser
type GetAllUsers = "users" :> Get '[JSON] [User]
type GetUser = "users" :> Capture "userId" Int :> Get '[JSON] User


server :: Server API
server = usersApi

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
  where muser = users ^? folded.filtered ((== uid). view userId)


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

toByteString :: Show s => s -> L.ByteString
toByteString = B.toLazyByteString . B.stringUtf8 . show

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

