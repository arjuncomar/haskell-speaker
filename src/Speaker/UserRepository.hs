module Speaker.UserRepository
    ( users
    ) where

import Speaker.User

users :: [User]
users = [ mkUser 1 "Isaac" "Newton"
        , mkUser 2 "Albert" "Einstein"
        ]
