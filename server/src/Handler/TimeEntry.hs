{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.TimeEntry where 

import Import 
import Database.Persist.Sql (toSqlKey)
import Data.Time.Calendar
import Data.Text.Read
import qualified Database

getFreeTimeEntryR :: Text -> Handler Value 
getFreeTimeEntryR userId = do 
    allTimeEntries <- runDB $ selectList [FreeTimeEntryUserId <-. [userId]] []
    maybeTimeZone <- lookupGetParam "timezone"
    case maybeTimeZone of 
        Just timezone -> returnJson $ catMaybes $ 
            Import.map (\x -> Database.convertFreeTimeEntryToLocal x timezone) allTimeEntries
        _ -> invalidArgs ["Failed to parse timezone"]

postFreeTimeEntryR :: Text -> Handler Value 
postFreeTimeEntryR userId = do 
    maybeDay <- lookupPostParam "day"
    maybeTimeZone <- lookupPostParam "timezone"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeUTCFromTime = Database.convertTextToTime maybeFromTimeText maybeTimeZone
    let maybeUTCToTime = Database.convertTextToTime maybeToTimeText maybeTimeZone
    case (maybeDay, maybeUTCFromTime, maybeUTCToTime) of
        (Just day, Just (fromTimeDayOffset, fromTime), Just (_, toTime)) -> do
            -- The database will hold in the day of fromTime if the event is staggered 
            -- between to two days 
            let maybeUtcDay = Database.updateDayString day fromTimeDayOffset
            case maybeUtcDay of 
                Just utcDay -> do
                    let timeEntry' = FreeTimeEntry userId (toLower utcDay) fromTime toTime
                    insertedFreeTimeEntry <- runDB $ insertEntity timeEntry'
                    returnJson insertedFreeTimeEntry
                _ -> invalidArgs ["Failed to parse day, from_time and/or to_time params"]
        (_, _, _) -> invalidArgs ["Failed to parse day, from_time and/or to_time params"]

putFreeTimeEntryR :: Text -> Handler Value 
putFreeTimeEntryR entryIdText = do
    maybeDay <- lookupPostParam "day"
    maybeTimeZone <- lookupPostParam "timezone"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeUTCFromTime = Database.convertTextToTime maybeFromTimeText maybeTimeZone
    let maybeUTCToTime = Database.convertTextToTime maybeToTimeText maybeTimeZone
    let eitherEntryId = decimal entryIdText
    case (maybeDay, maybeUTCFromTime, maybeUTCToTime, eitherEntryId) of
        (Just day, Just (fromTimeDayOffset, fromTime), Just (_, toTime), Right (entryIdInt, "")) -> do
            allTimeEntries <- runDB $ selectList [FreeTimeEntryId ==. toSqlKey (fromIntegral (entryIdInt::Integer))] []
            case allTimeEntries of 
                [Entity entryId FreeTimeEntry {..}] -> do 
                    -- The database will hold in the day of fromTime if the event is staggered 
                    -- between to two days 
                    let maybeUtcDay = Database.updateDayString day fromTimeDayOffset
                    case maybeUtcDay of 
                        Just utcDay -> do
                            runDB $ update entryId 
                                [
                                    FreeTimeEntryDay =. (toLower utcDay),
                                    FreeTimeEntryFromTime =. fromTime, 
                                    FreeTimeEntryToTime =. toTime
                                ]
                            return Null
                        _ -> invalidArgs ["Failed to parse day, from_time and/or to_time params"]
                _ -> notFound
        (_, _, _, _) -> invalidArgs ["Failed to parse day, from_time and/or to_time params"]

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
    maybeTimeZone <- lookupGetParam "timezone"
    case (maybeEventId, maybeTimeZone) of 
        (Just eventId, Just timezone) -> do 
            allTimeEntries <- runDB $ selectList [AvailableTimeEntryUserId <-. [userId], AvailableTimeEntryEventId <-. [eventId]] []
            returnJson $ catMaybes $ Import.map (\x -> Database.convertAvailableTimeEntryToLocal x timezone) allTimeEntries
        (_, _) -> invalidArgs ["Failed to parse event_id params"]

postAvailableTimeEntryR :: Text -> Handler Value 
postAvailableTimeEntryR userId = do 
    maybeEventId <- lookupGetParam "event_id"
    maybeDateText <- lookupPostParam "date"
    maybeTimeZone <- lookupPostParam "timezone"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeDate = Database.convertTextToDate maybeDateText
    let maybeUTCFromTime = Database.convertTextToTime maybeFromTimeText maybeTimeZone
    let maybeUTCToTime = Database.convertTextToTime maybeToTimeText maybeTimeZone
    case (maybeDate, maybeEventId, maybeUTCFromTime, maybeUTCToTime) of
        (Just date, Just eventId, Just (fromTimeDayOffset, fromTime), Just (_, toTime)) -> do
            -- The database will hold in the date of fromTime if the event is staggered 
            -- between to two days
            let utcDate = addDays fromTimeDayOffset date
            let timeEntry' = AvailableTimeEntry userId eventId utcDate fromTime toTime
            insertedAvailableTimeEntry <- runDB $ insertEntity timeEntry'
            returnJson insertedAvailableTimeEntry
        (_, _, _, _) -> invalidArgs ["Failed to parse date, from_time and/or to_time params"]

putAvailableTimeEntryR :: Text -> Handler Value 
putAvailableTimeEntryR entryIdText = do 
    maybeDateText <- lookupPostParam "date"
    maybeTimeZone <- lookupPostParam "timezone"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeDate = Database.convertTextToDate maybeDateText
    let maybeUTCFromTime = Database.convertTextToTime maybeFromTimeText maybeTimeZone
    let maybeUTCToTime = Database.convertTextToTime maybeToTimeText maybeTimeZone
    let eitherEntryId = decimal entryIdText
    case (maybeDate, maybeUTCFromTime, maybeUTCToTime, eitherEntryId) of
        (Just date, Just (fromTimeDayOffset, fromTime), Just (_, toTime), Right (entryIdInt, "")) -> do
            allTimeEntries <- runDB $ selectList [AvailableTimeEntryId ==. toSqlKey (fromIntegral (entryIdInt::Integer))] []
            case allTimeEntries of 
                [Entity entryId AvailableTimeEntry {..}] -> do 
                    -- The database will hold in the date of fromTime if the event is staggered 
                    -- between to two days
                    let utcDate = addDays fromTimeDayOffset date
                    runDB $ update entryId 
                        [
                            AvailableTimeEntryDate =. utcDate,
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
