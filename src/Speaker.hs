{-# LANGUAGE DataKinds       #-}
module Speaker
    ( startApp
    ) where

import Speaker.UserController
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Lens
import Data.Monoid


type SpeakerAPI = UsersAPI

server :: Server SpeakerAPI
server = usersApi

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy SpeakerAPI
api = Proxy

