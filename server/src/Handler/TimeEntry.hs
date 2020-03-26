{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.TimeEntry where 

import Import 
import Database.Persist.Sql (toSqlKey)
import Data.Time.LocalTime
import Data.Dates
import Data.Text.Read
import qualified Data.Text as T

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

putFreeTimeEntryR :: Text -> Handler Value 
putFreeTimeEntryR entryIdText = do 
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeFromTime = convertTextToTime maybeFromTimeText
    let maybeToTime = convertTextToTime maybeToTimeText
    let eitherEntryId = decimal entryIdText
    case (maybeFromTime, maybeToTime, eitherEntryId) of
        (Just fromTime, Just toTime, Right (entryIdInt, "")) -> do
            allTimeEntries <- runDB $ selectList [FreeTimeEntryId ==. toSqlKey (fromIntegral (entryIdInt::Integer))] []
            case allTimeEntries of 
                [Entity entryId FreeTimeEntry {..}] -> do 
                    runDB $ update entryId [FreeTimeEntryFromTime =. fromTime, FreeTimeEntryToTime =. toTime]
                    return Null
                _ -> notFound
        (_, _, _) -> invalidArgs ["Failed to parse day and from_time and to_time params"]

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
    let maybeDate = convertTextToDate maybeDateText
    let maybeFromTime = convertTextToTime maybeFromTimeText
    let maybeToTime = convertTextToTime maybeToTimeText
    case (maybeDate, maybeEventId, maybeFromTime, maybeToTime) of
        (Just date, Just eventId, Just fromTime, Just toTime) -> do
            let timeEntry' = AvailableTimeEntry userId eventId date fromTime toTime
            insertedAvailableTimeEntry <- runDB $ insertEntity timeEntry'
            returnJson insertedAvailableTimeEntry
        (_, _, _, _) -> invalidArgs ["Failed to parse date, from_time, and/or to_time params"]

putAvailableTimeEntryR :: Text -> Handler Value 
putAvailableTimeEntryR entryIdText = do 
    maybeDateText <- lookupPostParam "date"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeDate = convertTextToDate maybeDateText
    let maybeFromTime = convertTextToTime maybeFromTimeText
    let maybeToTime = convertTextToTime maybeToTimeText
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

convertTextToDate :: Maybe Text -> Maybe Day 
convertTextToDate Nothing = Nothing
convertTextToDate (Just s) = do 
    let sSplit = T.splitOn "-" s 
    case sSplit of 
        [monthText, dayText, yearText] -> do 
            let eitherMonthInt = decimal monthText
            let eitherDayInt = decimal dayText
            let eitherYearInt = decimal yearText
            case (eitherMonthInt, eitherDayInt, eitherYearInt) of 
                (Right (monthInt, ""), Right (dayInt, ""), Right (yearInt, "")) -> 
                    if monthInt <= 12 && monthInt > 0 && dayInt <= 31 && dayInt > 0
                        then Just $ dateTimeToDay $ DateTime yearInt monthInt dayInt 0 0 0
                        else Nothing
                (_, _, _) -> Nothing
        _ -> Nothing
