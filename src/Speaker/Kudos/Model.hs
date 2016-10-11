{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Speaker.Kudos.Model
    ( Kudos(..)
    , kudosAuthor
    , kudosContent
    , kudosRecipient
    , mkKudos
    , migrateKudos
    ) where

import Speaker.User.Model
import Speaker.Kudos.Category
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Function

share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkMigrate "migrateKudos"] [persistLowerCase|
  Kudos
    content String
    author UserId
    recipient UserId
    category Category
    deriving Show
|]

$(deriveJSON defaultOptions ''Kudos)

mkKudos :: String -> Int -> Int -> Category -> Kudos
mkKudos content = Kudos content `on` toSqlKey . fromIntegral 
