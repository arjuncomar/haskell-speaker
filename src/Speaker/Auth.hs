{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
module Speaker.Auth
    ( LoginAPI
    , UserLogin(..)
    , login
    ) where

import Speaker.Config
import Speaker.Utils
import Speaker.User.Repository
import Speaker.User.Model
import Servant
import Servant.Server
import Servant.Auth.Server
import qualified Data.Text as T
import GHC.Generics(Generic)
import Control.Lens
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.TH
import Crypto.PasswordStore
import Data.Text.Strict.Lens

data UserLogin = UserLogin {
    username :: T.Text
  , password :: T.Text
} deriving (Show, Eq, Read, Generic)
$(deriveJSON defaultOptions ''UserLogin)

type LoginAPI = "login" :> ReqBody '[JSON] UserLogin :> PostNoContent '[JSON] NoContent

login :: ServerT LoginAPI Speaker
login (UserLogin username password) = do
  muser <- runDB $ getUserByEmail username
  case muser^?_Just.userPassword.re utf8.filtered (verifyPassword $ utf8 # password) of
    Just _  -> lift $ return NoContent
    Nothing -> throwError err401 {
      errBody = "Invalid username or password."
    }
