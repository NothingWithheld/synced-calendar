{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database where 

import Import
import Database.Persist.Sql (toSqlKey)
import Data.List 
import Data.Time.LocalTime
import Data.Dates
import Data.Text.Read
import qualified Data.Text as T

showWithZeros :: Int -> String 
showWithZeros n | Prelude.length (show n) == 1 = "0" Prelude.++ (show n)
                | otherwise = show n

-- | Convert text string "HH:SS" to TimeOfDay in UTC time
-- Params: 
-- * timeString: format "HH:SS"
-- * offset: integer in between -12 to 14 to represent offset in hours of localtime from utctime
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

convertTextToBool :: Maybe Text -> Bool 
convertTextToBool Nothing = False
convertTextToBool (Just s) = do 
    let boolString = T.unpack s 
    case toLower boolString of 
        "true" -> True
        _ -> False

-- | Convert TimeOfDay in Local time to UTC time
-- Params: 
-- * time: type TimeOfDay
-- * offset: integer in between -12 to 14 to represent offset in hours of localtime from utctime
convertLocalToUTC :: TimeOfDay -> Text -> Maybe (Integer, TimeOfDay)
convertLocalToUTC time offsetText = do 
    let eitherOffsetInt = signed decimal offsetText
    case eitherOffsetInt of 
        Right (offsetInt, "") -> if offsetInt >= -12 && offsetInt <= 14 
            then let offsetMinInt = offsetInt * 60
                in Just $ localToUTCTimeOfDay (TimeZone offsetMinInt False "") time
            else Nothing
        _ -> Nothing

-- | Convert TimeOfDay in UTC time to Local time
-- Params: 
-- * time: type TimeOfDay
-- * offset: integer in between -12 to 14 to represent offset in hours of localtime from utctime
convertUTCToLocal :: TimeOfDay -> Text -> Maybe (Integer, TimeOfDay)
convertUTCToLocal time offsetText = do 
    let eitherOffsetInt = signed decimal offsetText
    case eitherOffsetInt of 
        Right (offsetInt, "") -> if offsetInt >= -12 && offsetInt <= 14 
            then let offsetMinInt = offsetInt * 60 
                in Just $ utcToLocalTimeOfDay (TimeZone offsetMinInt False "") time
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

fetchUserId :: Maybe Text -> Handler (Maybe UserId)
fetchUserId (Just userIdText) = do 
    let eitherUserId = decimal userIdText
    case eitherUserId of
        Right (userIdInt, "") -> do
            potentialUsers <- runDB $ selectList [UserId ==. toSqlKey (fromIntegral (userIdInt::Integer))] []
            case potentialUsers of
                [Entity userId _] -> return $ Just userId 
                _ -> return Nothing 
        _ -> return Nothing 
fetchUserId Nothing = do return $ Nothing

fetchUserIdByEmail :: Maybe Text -> Handler (Maybe UserId)
fetchUserIdByEmail (Just userEmailText) = do 
    potentialUsers <- runDB $ selectList [UserEmail ==. userEmailText] []
    case potentialUsers of
        [Entity userId _] -> return $ Just userId 
        _ -> return Nothing 
fetchUserIdByEmail Nothing = do return $ Nothing

fetchProposedEventInvitation :: Maybe Text -> Maybe Text -> Handler (Maybe (Entity ProposedEventInvitation))
fetchProposedEventInvitation (Just eventIdText) (Just recipientIdText) = do 
    maybeUserId <- fetchUserId (Just recipientIdText)
    let eitherEventId = decimal eventIdText
    case (eitherEventId, maybeUserId) of 
        (Right (eventIdInt, ""), Just recipientId) -> do
            allProposedEventInvitations <- runDB $ 
                selectList [
                    ProposedEventInvitationRecipientId ==. recipientId,
                    ProposedEventInvitationEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer)),
                    ProposedEventInvitationConfirmed <-. [False]
                ] []
            case allProposedEventInvitations of 
                [x] -> return $ Just x
                _ -> return Nothing 
        (_, _) -> return Nothing 
fetchProposedEventInvitation _ _ = do return $ Nothing

formatDate :: Day -> Text 
formatDate date = let dateSplit = T.splitOn "-" (pack $ show date) 
    in dateSplit!!1 Import.++ "-" Import.++ dateSplit!!2 Import.++ "-" Import.++ dateSplit!!0

splitStringByCommas :: Text -> [Text]
splitStringByCommas "" = []
splitStringByCommas s = T.splitOn "," (T.filter (/=' ') s)
