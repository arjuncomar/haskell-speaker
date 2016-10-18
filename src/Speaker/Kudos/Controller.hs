{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Speaker.Kudos.Controller
    ( KudosAPI
    , kudosApi
    , kudosForbidden
    ) where

import Speaker.Kudos.Model
import Speaker.Kudos.Repository
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
 - Kudos API: GET /v/kudos         -- list all kudos
 -            GET /v/kudos/kudoId  -- get the kudo with kid kudoId
 -}
type KudosAPI = "v" :> "kudos" :> (GetAllKudos :<|> GetKudos)
type GetAllKudos = Get '[JSON] [Kudos]
type GetKudos = Capture "kudosId" Int :> Get '[JSON] Kudos

kudosApi :: ServerT KudosAPI Speaker
kudosApi = getAllKudos :<|> getKudos

kudosForbidden :: ServerT KudosAPI Speaker
kudosForbidden = throwError err401 :<|> const (throwError err401)

getAllKudos :: Speaker [Kudos]
getAllKudos = runDB getKudosDB

getKudos :: Int -> Speaker Kudos
getKudos kid = do
  mkudo <- runDB $ getKudoDB kid
  case mkudo of
    Just kudo -> return kudo
    Nothing -> throwError err404 { 
      errBody = "No user found with userid = " <> toByteString kid 
    }
