{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.TimeEntry where 

import Import 
import Database.Persist.Sql (toSqlKey)
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Text.Read
import qualified Database

data FreeTimeEntryData = FreeTimeEntryData FreeTimeEntryId UserId Text TimeOfDay TimeOfDay
data AvailableTimeEntryData = AvailableTimeEntryData AvailableTimeEntryId UserId ProposedEventId Day TimeOfDay TimeOfDay

instance ToJSON FreeTimeEntryData where 
    toJSON (FreeTimeEntryData entryId userId day (TimeOfDay fromHour fromMinutes _) (TimeOfDay toHour toMinutes _)) =
        object [
            "id" .= entryId,
            "userId" .= userId, 
            "day" .= day,
            "fromTime" .= ((Database.showWithZeros fromHour) Import.++ ":" Import.++ (Database.showWithZeros fromMinutes)),
            "toTime" .= ((Database.showWithZeros toHour) Import.++ ":" Import.++ (Database.showWithZeros toMinutes))
        ]

instance ToJSON AvailableTimeEntryData where 
    toJSON (AvailableTimeEntryData entryId eventId userId date (TimeOfDay fromHour fromMinutes _) (TimeOfDay toHour toMinutes _)) =
        object [
            "id" .= entryId,
            "userId" .= userId, 
            "eventId" .= eventId,
            "date" .= Database.formatDate date,
            "fromTime" .= ((Database.showWithZeros fromHour) Import.++ ":" Import.++ (Database.showWithZeros fromMinutes)),
            "toTime" .= ((Database.showWithZeros toHour) Import.++ ":" Import.++ (Database.showWithZeros toMinutes))
        ]

convertFreeTimeEntryToLocal :: Entity FreeTimeEntry -> Text -> Maybe FreeTimeEntryData
convertFreeTimeEntryToLocal (Entity entryId (FreeTimeEntry userId day fromTime toTime)) timezone = do 
    let maybeLocalFromTime = Database.convertUTCToLocal fromTime timezone
    let maybeLocalToTime = Database.convertUTCToLocal toTime timezone
    case (maybeLocalFromTime, maybeLocalToTime) of 
        (Just (fromTimeDayOffset, localFromTime), Just (_, localToTime)) -> do 
            -- The database will hold in the day of fromTime if the event is staggered 
            -- between to two days 
            let maybeLocalDay = Database.updateDayString day fromTimeDayOffset
            case maybeLocalDay of 
                Just localDay -> Just $ FreeTimeEntryData entryId userId localDay localFromTime localToTime
                _ -> Nothing
        (_, _) -> Nothing

convertAvailableTimeEntryToLocal :: Entity AvailableTimeEntry -> Text -> Maybe AvailableTimeEntryData
convertAvailableTimeEntryToLocal (Entity entryId (AvailableTimeEntry userId eventId date fromTime toTime)) timezone = do 
    let maybeLocalFromTime = Database.convertUTCToLocal fromTime timezone
    let maybeLocalToTime = Database.convertUTCToLocal toTime timezone
    case (maybeLocalFromTime, maybeLocalToTime) of 
        (Just (fromTimeDayOffset, localFromTime), Just (_, localToTime)) -> do 
            -- The database will hold in the day of fromTime if the event is staggered 
            -- between to two days 
            let localDate = addDays fromTimeDayOffset date
            return $ AvailableTimeEntryData entryId userId eventId localDate localFromTime localToTime
        (_, _) -> Nothing

getFreeTimeEntryR :: Text -> Handler Value 
getFreeTimeEntryR userIdText = do
    maybeUserId <- Database.fetchUserId (Just userIdText)
    case maybeUserId of 
        Just userId -> do
            allTimeEntries <- runDB $ selectList [FreeTimeEntryUserId <-. [userId]] []
            maybeTimeZone <- lookupGetParam "timezone"
            case maybeTimeZone of 
                Just timezone -> returnJson $ catMaybes $ 
                    Import.map (\x -> convertFreeTimeEntryToLocal x timezone) allTimeEntries
                _ -> invalidArgs ["Failed to parse timezone"]
        _ -> invalidArgs ["Failed to find user with id: " Import.++ userIdText]

postFreeTimeEntryR :: Text -> Handler Value 
postFreeTimeEntryR userIdText = do 
    maybeDay <- lookupPostParam "day"
    maybeTimeZone <- lookupPostParam "timezone"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    maybeUserId <- Database.fetchUserId (Just userIdText)
    let maybeUTCFromTime = Database.convertTextToTime maybeFromTimeText maybeTimeZone
    let maybeUTCToTime = Database.convertTextToTime maybeToTimeText maybeTimeZone
    case (maybeUserId, maybeDay, maybeUTCFromTime, maybeUTCToTime) of
        (Just userId, Just day, Just (fromTimeDayOffset, fromTime), Just (_, toTime)) -> do
            -- The database will hold in the day of fromTime if the event is staggered 
            -- between to two days 
            let maybeUtcDay = Database.updateDayString day fromTimeDayOffset
            case maybeUtcDay of 
                Just utcDay -> do
                    let timeEntry' = FreeTimeEntry userId (toLower utcDay) fromTime toTime
                    (Entity entryId _) <- runDB $ insertEntity timeEntry'
                    returnJson $ FreeTimeEntryData entryId userId (toLower utcDay) fromTime toTime
                _ -> invalidArgs ["Failed to parse day, from_time and/or to_time params"]
        (_, _, _, _) -> invalidArgs ["Failed to parse day, from_time and/or to_time params"]

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
getAvailableTimeEntryR userIdText = do 
    maybeEventId <- lookupGetParam "event_id"
    maybeTimeZone <- lookupGetParam "timezone"
    maybeUserId <- Database.fetchUserId (Just userIdText)
    case (maybeUserId, maybeEventId, maybeTimeZone) of 
        (Just userId, Just eventIdText, Just timezone) -> do 
            let eitherEventId = decimal eventIdText
            case (eitherEventId) of 
                Right (eventIdInt, "") -> do 
                    allTimeEntries <- runDB $ 
                        selectList [
                            AvailableTimeEntryUserId <-. [userId], 
                            AvailableTimeEntryEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer))
                        ] []
                    returnJson $ catMaybes $ Import.map (\x -> convertAvailableTimeEntryToLocal x timezone) allTimeEntries
                _ -> invalidArgs ["Failed to parse event_id params"]
        (_, _, _) -> invalidArgs ["Failed to parse event_id params"]

postAvailableTimeEntryR :: Text -> Handler Value 
postAvailableTimeEntryR userIdText = do 
    maybeEventId <- lookupPostParam "event_id"
    maybeDateText <- lookupPostParam "date"
    maybeTimeZone <- lookupPostParam "timezone"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    maybeUserId <- Database.fetchUserId (Just userIdText)
    let maybeDate = Database.convertTextToDate maybeDateText
    let maybeUTCFromTime = Database.convertTextToTime maybeFromTimeText maybeTimeZone
    let maybeUTCToTime = Database.convertTextToTime maybeToTimeText maybeTimeZone
    case (maybeUserId, maybeDate, maybeEventId, maybeUTCFromTime, maybeUTCToTime) of
        (Just userId, Just date, Just eventIdText, Just (fromTimeDayOffset, fromTime), Just (_, toTime)) -> do
            let eitherEventId = decimal eventIdText
            case eitherEventId of     
                Right (eventIdInt, "") -> do 
                    allProposedEvents <- runDB $ 
                        selectList [
                            ProposedEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer)),
                            ProposedEventConfirmed <-. [False]
                        ] []
                    case allProposedEvents of 
                        [Entity eventId _] -> do
                            -- The database will hold in the date of fromTime if the event is staggered 
                            -- between to two days
                            let utcDate = addDays fromTimeDayOffset date
                            let timeEntry' = AvailableTimeEntry userId eventId utcDate fromTime toTime
                            (Entity entryId _) <- runDB $ insertEntity timeEntry'
                            returnJson $ AvailableTimeEntryData entryId userId eventId utcDate fromTime toTime
                        _ -> invalidArgs ["Failed to find corresponding ProposedEvent with id: " Import.++ eventIdText]
                _ -> invalidArgs ["Please provide a valid integer for event_id"]
        (_, _, _, _, _) -> invalidArgs ["Failed to parse API params"]

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
