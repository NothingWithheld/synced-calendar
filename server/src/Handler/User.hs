{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.User where 

import Import

postUserR :: Handler Value 
postUserR = do 
    maybeEmail <- lookupPostParam "email"
    maybeHashedPass <- lookupPostParam "hashed_pass"
    case (maybeEmail, maybeHashedPass) of 
        (Just email, Just hashedPass) -> do 
            potentialUsers <- runDB $ selectList [UserEmail ==. email] []
            case potentialUsers of 
                [_] -> invalidArgs ["User is already created with email: " Import.++ email]
                _ -> do 
                    let registered = True
                    let user' = User email hashedPass registered
                    insertedUser <- runDB $ insertEntity user' 
                    returnJson insertedUser
        (_, _) -> invalidArgs ["Failed to provide email and/or hashed_pass values"]
        