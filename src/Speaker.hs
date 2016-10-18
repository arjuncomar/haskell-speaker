{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Speaker
    ( startApp
    ) where

import Speaker.Auth
import Speaker.Config
import Speaker.Utils
import Speaker.Kudos.Controller
import Speaker.Kudos.Model
import Speaker.Kudos.Repository
import Speaker.User.Controller
import Speaker.User.Model
import Speaker.User.Repository
import Servant
import Servant.Auth.Server
import Network.Wai.Handler.Warp
import Database.Persist.Sqlite as S
import Control.Monad.Trans.Reader(runReaderT)
import Crypto.JOSE.JWK

type ProtectedAPI = "speaker" :> (UsersAPI :<|> KudosAPI :<|> LoginAPI)
type SpeakerAPI auth = Auth auth User :> ProtectedAPI

migrations :: [Migration]
migrations = [migrateUser, migrateKudos]

server :: AuthResult User -> ServerT ProtectedAPI Speaker
server (Authenticated _) = usersApi :<|> kudosApi :<|> const (return NoContent)
server _ = usersForbidden :<|> kudosForbidden :<|> login

startApp :: IO ()
startApp = do
  myKey <- generateKey
  withConfig $ \c -> do
    sequence_ $ runMigrationIO c <$> migrations
    flip runReaderT c . runDB $ do  mapM_ insert testUsers
                                    mapM_ insert testKudos
    run 8080 $ app myKey c

app :: JWK -> Config -> Application
app k c = serveWithContext api (appContext k) $ enter (runSpeaker c) server

api :: Proxy (SpeakerAPI '[JWT])
api = Proxy

appContext :: JWK -> Context '[CookieSettings, JWTSettings]
appContext k = defaultCookieSettings :. defaultJWTSettings k :. EmptyContext
