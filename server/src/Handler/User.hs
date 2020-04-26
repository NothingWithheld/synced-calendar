{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.User where 

import Import
import Yesod.Auth.Util.PasswordStore

strength :: Int
strength = 12

salt :: ByteString
salt = "3s18bst5ebf"

postUserR :: Handler Value 
postUserR = do 
    maybeEmail <- lookupPostParam "email"
    maybePassword <- lookupPostParam "password"
    case (maybeEmail, maybePassword) of 
        (Just email, Just password) -> do 
            potentialUsers <- runDB $ selectList [UserEmail ==. email] []
            case potentialUsers of 
                [_] -> invalidArgs ["User is already created with email: " Import.++ email]
                _ -> do 
                    let registered = True
                    let hashedPass = makePasswordSalt (Import.encodeUtf8 password) (makeSalt salt) strength
                    let user' = User email (Import.decodeUtf8 hashedPass) registered
                    insertedUser <- runDB $ insertEntity user' 
                    returnJson insertedUser
        (_, _) -> invalidArgs ["Failed to provide email and/or hashed_pass values"]

getUserR :: Handler Value 
getUserR = do 
    maybeEmail <- lookupPostParam "email"
    maybePassword <- lookupPostParam "password"
    case (maybeEmail, maybePassword) of 
        (Just email, Just password) -> do 
            let hashedPass = makePasswordSalt (Import.encodeUtf8 password) (makeSalt salt) strength
            potentialUsers <- runDB $ selectList [UserEmail ==. email, UserPassword ==. (Import.decodeUtf8 hashedPass)] []
            case potentialUsers of 
                [user] -> returnJson user
                _ -> invalidArgs ["Failed to login user sucessfully"]
        (_, _) -> invalidArgs ["Failed to login user sucessfully"]
        