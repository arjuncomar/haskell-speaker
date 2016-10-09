{-# LANGUAGE TemplateHaskell #-}
module Speaker.User
    ( User
    , userId
    , userFirstName
    , userLastName
    , mkUser
    ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH

data User = User
  { _userId        :: Int
  , _userFirstName :: String
  , _userLastName  :: String
  } deriving (Eq, Show)
makeLenses ''User
$(deriveJSON defaultOptions ''User)

mkUser :: Int -> String -> String -> User
mkUser = User
