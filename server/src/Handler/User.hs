{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.User where 

import Import
import Yesod.Auth.Util.PasswordStore

strength :: Int
strength = 12

salt :: ByteString
salt = "3s18bst5ebf"

getUserR :: Handler Value 
getUserR = do 
    maybeEmail <- lookupGetParam "email"
    case maybeEmail of 
        Just email -> do 
            potentialUsers <- runDB $ selectList [UserEmail ==. email] []
            case potentialUsers of 
                [Entity userId _] -> return $ object ["userId" .= userId]
                _ -> invalidArgs ["Failed to find user with email: " Import.++ email]
        _ -> invalidArgs ["Failed to find parameter email"]

getUserLoginR :: Handler Value 
getUserLoginR = do 
    maybeEmail <- lookupGetParam "email"
    maybePassword <- lookupGetParam "password"
    case (maybeEmail, maybePassword) of 
        (Just email, Just password) -> do 
            let hashedPass = makePasswordSalt (Import.encodeUtf8 password) (makeSalt salt) strength
            potentialUsers <- runDB $ selectList [UserEmail ==. email, UserPassword ==. Just (Import.decodeUtf8 hashedPass)] []
            case potentialUsers of 
                [Entity userId _] -> return $ object ["userId" .= userId]
                _ -> invalidArgs ["Failed to login user sucessfully"]
        (_, _) -> invalidArgs ["Failed to login user sucessfully"]
        
postUserLoginR :: Handler Value 
postUserLoginR = do 
    maybeEmail <- lookupPostParam "email"
    maybePassword <- lookupPostParam "password"
    case (maybeEmail, maybePassword) of 
        (Just email, Just password) -> do 
            potentialUsers <- runDB $ selectList [UserEmail ==. email] []
            case potentialUsers of 
                [Entity _ (User _ _ True)] -> invalidArgs ["User is already created with email: " Import.++ email]
                [Entity userId _] -> do 
                    let hashedPass = makePasswordSalt (Import.encodeUtf8 password) (makeSalt salt) strength
                    runDB $ update userId 
                        [
                            UserPassword =. (Just $ Import.decodeUtf8 hashedPass),
                            UserRegistered =. True
                        ]
                    return $ object ["userId" .= userId]
                _ -> do 
                    let registered = True
                    let hashedPass = makePasswordSalt (Import.encodeUtf8 password) (makeSalt salt) strength
                    let user' = User email (Just $ Import.decodeUtf8 hashedPass) registered
                    Entity userId _ <- runDB $ insertEntity user' 
                    return $ object ["userId" .= userId]
        (_, _) -> invalidArgs ["Failed to provide email and/or hashed_pass values"]
        