{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Speaker
    ( startApp
    ) where

import Speaker.Config
import Speaker.Utils
import Speaker.Kudos.Controller
import Speaker.Kudos.Model
import Speaker.Kudos.Repository
import Speaker.User.Controller
import Speaker.User.Model
import Speaker.User.Repository
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Lens
import Data.Monoid
import Database.Persist.Sqlite as S
import Database.Persist.Postgresql as P
import Control.Applicative
import Data.Traversable
import Control.Monad.Trans.Reader(runReaderT)

type SpeakerAPI = "speaker" :> (UsersAPI :<|> KudosAPI)

migrations :: [Migration]
migrations = [migrateUser, migrateKudos]

server :: ServerT SpeakerAPI Speaker
server = usersApi :<|> kudosApi

startApp :: IO ()
startApp = withConfig $ \c -> do
  sequence_ $ runMigrationIO c <$> migrations
  flip runReaderT c . runDB $ do mapM_ insert testUsers
                                 mapM_ insert testKudos
  run 8080 (app c)

app :: Config -> Application
app c = serve api $ enter (runSpeaker c) server

api :: Proxy SpeakerAPI
api = Proxy

