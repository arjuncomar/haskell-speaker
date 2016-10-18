{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Speaker.User.Model
    ( User
    , userFirstName
    , userLastName
    , userEmail
    , userPassword
    , mkUser
    , UserId(..)
    , EntityField(..)
    , migrateUser
    , Unique(..)
    ) where

import Speaker.Utils
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Database.Persist
import Database.Persist.TH
import qualified Data.Text as T
import Servant.Auth.Server

share [ mkPersist speakerSqlSettings
      , mkMigrate "migrateUser"] [persistLowerCase|
  User
    firstName T.Text
    lastName  T.Text
    email     T.Text
    password  T.Text
    UniqueEmail email
    deriving Show
|]

$(deriveJSON defaultOptions ''User)
instance ToJWT User
instance FromJWT User

mkUser :: T.Text -> T.Text -> T.Text -> T.Text -> User
mkUser = User
