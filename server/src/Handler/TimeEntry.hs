{-# LANGUAGE OverloadedStrings   #-}

module Handler.TimeEntry where 

import Import 

postTimeEntryR :: Handler Value 
postTimeEntryR = do 
    fromTime <- lookupPostParam "from_time"
    case fromTime of
        Nothing -> return $ object ["number" .= (400 :: Int32)]
        Just fromTime -> do 
            toTime <- lookupPostParam "to_time" 
            case toTime of 
                Nothing -> return $ object ["number" .= (400 :: Int32)]
                Just toTime -> do
                    let timeEntry' = TimeEntry fromTime toTime
                    insertedTimeEntry <- runDB $ insertEntity timeEntry'
                    returnJson insertedTimeEntry