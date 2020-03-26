{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database where 

import Import 
import Data.Time.LocalTime
import Data.Dates
import Data.Text.Read
import qualified Data.Text as T

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