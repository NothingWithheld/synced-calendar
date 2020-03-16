{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.TimeEntry where 

import Import 
import Database.Persist.Sql (toSqlKey)
import Data.Time.LocalTime
import Data.Text.Read
import qualified Data.Text as T

newtype Key entity = Key { unKey :: PersistValue }

getFreeTimeEntryR :: Text -> Handler Value 
getFreeTimeEntryR userId = do 
    allTimeEntries <- runDB $ selectList [FreeTimeEntryUserId <-. [userId]] []
    returnJson allTimeEntries

postFreeTimeEntryR :: Text -> Handler Value 
postFreeTimeEntryR userId = do 
    maybeDay <- lookupPostParam "day"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeFromTime = convertTextToTime maybeFromTimeText
    let maybeToTime = convertTextToTime maybeToTimeText
    case (maybeDay, maybeFromTime, maybeToTime) of
        (Just day, Just fromTime, Just toTime) -> do
            let timeEntry' = FreeTimeEntry userId (toLower day) fromTime toTime
            insertedFreeTimeEntry <- runDB $ insertEntity timeEntry'
            returnJson insertedFreeTimeEntry
        (_, _, _) -> invalidArgs ["Failed to parse day and from_time and to_time params"]

deleteFreeTimeEntryR :: Text -> Handler Value 
deleteFreeTimeEntryR entryIdText = do
    -- runDB $ delete (Key entryIdText)
    -- return Null
    let eitherEntryId = decimal entryIdText 
    case eitherEntryId of 
        Right (entryIdInt, "") -> do
            runDB $ deleteWhere [FreeTimeEntryId ==. toSqlKey (fromIntegral (entryIdInt::Integer))]
            return Null
        _ -> badMethod

convertTextToTime :: Maybe Text -> Maybe TimeOfDay 
convertTextToTime Nothing = Nothing
convertTextToTime (Just s) = do 
    let sSplit = T.splitOn ":" s 
    case sSplit of 
        [hourText, minText] -> do 
            let eitherHourInt = decimal hourText
            let eitherMinInt = decimal minText
            case (eitherHourInt, eitherMinInt) of 
                (Right (hourInt, ""), Right (minInt, "")) -> 
                    if hourInt < 24 && hourInt >= 0 && minInt < 60 && minInt >= 0
                        then Just $ TimeOfDay hourInt minInt 0
                        else Nothing
                (_, _) -> Nothing
        _ -> Nothing
