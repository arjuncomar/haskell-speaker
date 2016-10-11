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
    , mkUser
    , UserId(..)
    , EntityField(..)
    , migrateUser
    ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Database.Persist
import Database.Persist.TH

share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkMigrate "migrateUser"] [persistLowerCase|
  User
    firstName String
    lastName  String
    deriving Show
|]

$(deriveJSON defaultOptions ''User)

mkUser :: String -> String -> User
mkUser = User
