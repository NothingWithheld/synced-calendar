{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database where 

import Import 
import Data.Time.LocalTime
import Data.Dates
import Data.Text.Read
import qualified Data.Text as T

-- | Convert text string "HH:SS" to TimeOfDay in UTC time
-- Params: 
-- * timeString: format "HH:SS"
-- * offset: integer in between -12 to 14 to represent offset in hours of localtime from utctime
-- * dayString: "monday"
convertTextToTime :: Maybe Text :: Maybe Text :: Maybe Text -> Maybe TimeOfDay 
convertTextToTime (Just timeString) (Just offset) (Just dayString)  = do 
    let timeStringSplit = T.splitOn ":" timeString 
    case timeStringSplit of 
        [hourText, minText] -> do 
            let eitherHourInt = decimal hourText
            let eitherMinInt = decimal minText
            case (eitherHourInt, eitherMinInt) of 
                (Right (hourInt, ""), Right (minInt, "")) -> 
                    if hourInt < 24 && hourInt >= 0 && minInt < 60 && minInt >= 0
                        then do 
                            let eitherTimeOfDay = convertLocalToUTC $ (TimeOfDay hourInt minInt 0) offset
                            case eitherTimeOfDay of 
                                Just (dayOffset, timeOfDay) -> (updateDayString dayString dayOffset, timeOfDay)
                                _ -> Nothing
                        else Nothing
                (_, _) -> Nothing
        _ -> Nothing
convertTextToTime _ _ _ = Nothing

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

-- | Convert TimeOfDay to UTC time
-- Params: 
-- * time: type TimeOfDay
-- * offset: integer in between -12 to 14 to represent offset in hours of localtime from utctime
convertLocalToUTC :: Maybe TimeOfDay :: Maybe Text -> Maybe (Integer,TimeOfDay)
convertLocalToUTC Nothing _ = Nothing 
convertLocalToUTC (Just time) offsetText = do 
    let eitherOffsetInt = decimal offsetText
    case eitherOffsetInt of 
        Right offsetInt -> if offsetInt >= -12 and offsetInt <= 14 
            then utcToLocalTimeOfDay (TimeZone offsetInt False "") time
            else Nothing
        _ -> Nothing

updateDayString :: Text :: Integer -> Maybe Text
updateDayString dayString dayOffset = do
    let days = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]
    let eitherIndexOfDay = elemIndex dayString days 
    case eitherIndexOfDay of 
        Just indexOfDay -> do 
            let nextIndexOfDay = (indexOfDay + dayOffset) `mod` (length days)
            return $ Just days !! nextIndexOfDay
        _ -> Nothing
