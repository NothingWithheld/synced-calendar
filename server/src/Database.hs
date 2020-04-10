{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database where 

import Import
import Data.List 
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Dates
import Data.Text.Read
import qualified Data.Text as T

convertFreeTimeEntryToLocal :: Entity FreeTimeEntry -> Text -> Maybe (Entity FreeTimeEntry)
convertFreeTimeEntryToLocal (Entity entryId (FreeTimeEntry userId day fromTime toTime)) timezone = do 
    let eitherLocalFromTime = Database.convertUTCToLocal fromTime timezone
    let eitherLocalToTime = Database.convertUTCToLocal toTime timezone
    case (eitherLocalFromTime, eitherLocalToTime) of 
        (Just (fromTimeDayOffset, localFromTime), Just (_, localToTime)) -> do 
            -- The database will hold in the day of fromTime if the event is staggered 
            -- between to two days 
            let maybeLocalDay = Database.updateDayString day fromTimeDayOffset
            case maybeLocalDay of 
                Just localDay -> Just $ Entity entryId (FreeTimeEntry userId localDay localFromTime localToTime)
                _ -> Nothing
        (_, _) -> Nothing

convertAvailableTimeEntryToLocal :: Entity AvailableTimeEntry -> Text -> Maybe (Entity AvailableTimeEntry)
convertAvailableTimeEntryToLocal (Entity entryId (AvailableTimeEntry userId eventId date fromTime toTime)) timezone = do 
    let eitherLocalFromTime = Database.convertUTCToLocal fromTime timezone
    let eitherLocalToTime = Database.convertUTCToLocal toTime timezone
    case (eitherLocalFromTime, eitherLocalToTime) of 
        (Just (fromTimeDayOffset, localFromTime), Just (_, localToTime)) -> do 
            -- The database will hold in the day of fromTime if the event is staggered 
            -- between to two days 
            let localDate = addDays fromTimeDayOffset date
            return $ Entity entryId (AvailableTimeEntry userId eventId localDate localFromTime localToTime)
        (_, _) -> Nothing

-- | Convert text string "HH:SS" to TimeOfDay in UTC time
-- Params: 
-- * timeString: format "HH:SS"
-- * offset: integer in between -12 to 14 to represent offset in hours of localtime from utctime
-- * dayString: "monday"
convertTextToTime :: Maybe Text -> Maybe Text -> Maybe (Integer, TimeOfDay)
convertTextToTime (Just timeString) (Just offset) = do 
    let timeStringSplit = T.splitOn ":" timeString 
    case timeStringSplit of 
        [hourText, minText] -> do 
            let eitherHourInt = decimal hourText
            let eitherMinInt = decimal minText
            case (eitherHourInt, eitherMinInt) of 
                (Right (hourInt, ""), Right (minInt, "")) -> 
                    if hourInt < 24 && hourInt >= 0 && minInt < 60 && minInt >= 0
                        then convertLocalToUTC (TimeOfDay hourInt minInt 0) offset
                        else Nothing
                (_, _) -> Nothing
        _ -> Nothing
convertTextToTime _ _ = Nothing

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

-- | Convert TimeOfDay in Local time to UTC time
-- Params: 
-- * time: type TimeOfDay
-- * offset: integer in between -12 to 14 to represent offset in hours of localtime from utctime
convertLocalToUTC :: TimeOfDay -> Text -> Maybe (Integer, TimeOfDay)
convertLocalToUTC time offsetText = do 
    let eitherOffsetInt = decimal offsetText
    case eitherOffsetInt of 
        Right (offsetInt, "") -> if offsetInt >= -12 && offsetInt <= 14 
            then Just $ utcToLocalTimeOfDay (TimeZone offsetInt False "") time
            else Nothing
        _ -> Nothing

-- | Convert TimeOfDay in UTC time to Local time
-- Params: 
-- * time: type TimeOfDay
-- * offset: integer in between -12 to 14 to represent offset in hours of localtime from utctime
convertUTCToLocal :: TimeOfDay -> Text -> Maybe (Integer, TimeOfDay)
convertUTCToLocal time offsetText = do 
    let eitherOffsetInt = decimal offsetText
    case eitherOffsetInt of 
        Right (offsetInt, "") -> if offsetInt >= -12 && offsetInt <= 14 
            then Just $ localToUTCTimeOfDay (TimeZone offsetInt False "") time
            else Nothing
        _ -> Nothing

updateDayString :: Text -> Integer -> Maybe Text
updateDayString dayString dayOffset = do
    let days = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]
    let eitherIndexOfDay = elemIndex (toLower dayString) days 
    case eitherIndexOfDay of 
        Just indexOfDay -> do 
            let nextIndexOfDay = (indexOfDay + (fromInteger dayOffset)) `mod` (Import.length days)
            return $ days !! nextIndexOfDay
        _ -> Nothing
