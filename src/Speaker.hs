{-# LANGUAGE DataKinds       #-}
module Speaker
    ( startApp
    ) where

import Speaker.User.Controller
import Speaker.User.Model
import Speaker.Config
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Lens
import Data.Monoid
import Database.Persist.Sqlite as S
import Database.Persist.Postgresql as P


type SpeakerAPI = UsersAPI

server :: Config -> Server SpeakerAPI
server = usersApi

startApp :: IO ()
startApp = withConfig $ \c -> do
  runMigrationIO c migrateUser
  run 8080 (app c)

app :: Config -> Application
app = serve api . server

api :: Proxy SpeakerAPI
api = Proxy

