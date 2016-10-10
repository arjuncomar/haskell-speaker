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
import Control.Applicative
import Data.Traversable

type SpeakerAPI = UsersAPI

migrations :: [Migration]
migrations = [migrateUser]

server :: Config -> Server SpeakerAPI
server = usersApi

startApp :: IO ()
startApp = withConfig $ \c -> do
  sequence_ $ runMigrationIO c <$> migrations
  run 8080 (app c)

app :: Config -> Application
app = serve api . server

api :: Proxy SpeakerAPI
api = Proxy

