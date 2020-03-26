{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.TimeEntry where 

import Import 
import Database.Persist.Sql (toSqlKey)
import Data.Text.Read
import qualified Database

getFreeTimeEntryR :: Text -> Handler Value 
getFreeTimeEntryR userId = do 
    allTimeEntries <- runDB $ selectList [FreeTimeEntryUserId <-. [userId]] []
    returnJson allTimeEntries

postFreeTimeEntryR :: Text -> Handler Value 
postFreeTimeEntryR userId = do 
    maybeDay <- lookupPostParam "day"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeFromTime = Database.convertTextToTime maybeFromTimeText
    let maybeToTime = Database.convertTextToTime maybeToTimeText
    case (maybeDay, maybeFromTime, maybeToTime) of
        (Just day, Just fromTime, Just toTime) -> do
            let timeEntry' = FreeTimeEntry userId (toLower day) fromTime toTime
            insertedFreeTimeEntry <- runDB $ insertEntity timeEntry'
            returnJson insertedFreeTimeEntry
        (_, _, _) -> invalidArgs ["Failed to parse day, from_time and/or to_time params"]

putFreeTimeEntryR :: Text -> Handler Value 
putFreeTimeEntryR entryIdText = do 
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeFromTime = Database.convertTextToTime maybeFromTimeText
    let maybeToTime = Database.convertTextToTime maybeToTimeText
    let eitherEntryId = decimal entryIdText
    case (maybeFromTime, maybeToTime, eitherEntryId) of
        (Just fromTime, Just toTime, Right (entryIdInt, "")) -> do
            allTimeEntries <- runDB $ selectList [FreeTimeEntryId ==. toSqlKey (fromIntegral (entryIdInt::Integer))] []
            case allTimeEntries of 
                [Entity entryId FreeTimeEntry {..}] -> do 
                    runDB $ update entryId [FreeTimeEntryFromTime =. fromTime, FreeTimeEntryToTime =. toTime]
                    return Null
                _ -> notFound
        (_, _, _) -> invalidArgs ["Failed to parse day, from_time and/or to_time params"]

deleteFreeTimeEntryR :: Text -> Handler Value 
deleteFreeTimeEntryR entryIdText = do
    let eitherEntryId = decimal entryIdText 
    case eitherEntryId of 
        Right (entryIdInt, "") -> do
            runDB $ deleteWhere [FreeTimeEntryId ==. toSqlKey (fromIntegral (entryIdInt::Integer))]
            return Null
        _ -> badMethod

getAvailableTimeEntryR :: Text -> Handler Value 
getAvailableTimeEntryR userId = do 
    maybeEventId <- lookupGetParam "event_id"
    case maybeEventId of 
        (Just eventId) -> do 
            allTimeEntries <- runDB $ selectList [AvailableTimeEntryUserId <-. [userId], AvailableTimeEntryEventId <-. [eventId]] []
            returnJson allTimeEntries
        _ -> invalidArgs ["Failed to parse event_id params"]

postAvailableTimeEntryR :: Text -> Handler Value 
postAvailableTimeEntryR userId = do 
    maybeEventId <- lookupGetParam "event_id"
    maybeDateText <- lookupPostParam "date"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeDate = Database.convertTextToDate maybeDateText
    let maybeFromTime = Database.convertTextToTime maybeFromTimeText
    let maybeToTime = Database.convertTextToTime maybeToTimeText
    case (maybeDate, maybeEventId, maybeFromTime, maybeToTime) of
        (Just date, Just eventId, Just fromTime, Just toTime) -> do
            let timeEntry' = AvailableTimeEntry userId eventId date fromTime toTime
            insertedAvailableTimeEntry <- runDB $ insertEntity timeEntry'
            returnJson insertedAvailableTimeEntry
        (_, _, _, _) -> invalidArgs ["Failed to parse date, from_time and/or to_time params"]

putAvailableTimeEntryR :: Text -> Handler Value 
putAvailableTimeEntryR entryIdText = do 
    maybeDateText <- lookupPostParam "date"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeDate = Database.convertTextToDate maybeDateText
    let maybeFromTime = Database.convertTextToTime maybeFromTimeText
    let maybeToTime = Database.convertTextToTime maybeToTimeText
    let eitherEntryId = decimal entryIdText
    case (maybeDate, maybeFromTime, maybeToTime, eitherEntryId) of
        (Just date, Just fromTime, Just toTime, Right (entryIdInt, "")) -> do
            allTimeEntries <- runDB $ selectList [AvailableTimeEntryId ==. toSqlKey (fromIntegral (entryIdInt::Integer))] []
            case allTimeEntries of 
                [Entity entryId AvailableTimeEntry {..}] -> do 
                    runDB $ update entryId 
                        [
                            AvailableTimeEntryDate =. date,
                            AvailableTimeEntryFromTime =. fromTime, 
                            AvailableTimeEntryToTime =. toTime
                        ]
                    return Null
                _ -> notFound
        (_, _, _, _) -> invalidArgs ["Failed to parse date, from_time and/or to_time params"]

deleteAvailableTimeEntryR :: Text -> Handler Value 
deleteAvailableTimeEntryR entryIdText = do
    let eitherEntryId = decimal entryIdText 
    case eitherEntryId of 
        Right (entryIdInt, "") -> do
            runDB $ deleteWhere [AvailableTimeEntryId ==. toSqlKey (fromIntegral (entryIdInt::Integer))]
            return Null
        _ -> badMethod
