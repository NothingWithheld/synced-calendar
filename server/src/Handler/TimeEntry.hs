{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.TimeEntry where 

import Import 
import Database.Persist.Sql (toSqlKey)
import Data.Dates
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Text.Read
import qualified Database
import qualified Handler.Event

data FreeTimeEntryData = FreeTimeEntryData FreeTimeEntryId UserId Text TimeOfDay TimeOfDay Bool
data AvailableTimeEntryData = AvailableTimeEntryData AvailableTimeEntryId UserId ProposedEventId Day TimeOfDay TimeOfDay Bool

instance ToJSON FreeTimeEntryData where 
    toJSON (FreeTimeEntryData entryId userId day (TimeOfDay fromHour fromMinutes _) (TimeOfDay toHour toMinutes _) spanMultiple) =
        object [
            "id" .= entryId,
            "userId" .= userId, 
            "day" .= day,
            "fromTime" .= ((Database.showWithZeros fromHour) Import.++ ":" Import.++ (Database.showWithZeros fromMinutes)),
            "toTime" .= ((Database.showWithZeros toHour) Import.++ ":" Import.++ (Database.showWithZeros toMinutes)),
            "spanMultiple" .= spanMultiple
        ]

instance ToJSON AvailableTimeEntryData where 
    toJSON (AvailableTimeEntryData entryId userId eventId date (TimeOfDay fromHour fromMinutes _) (TimeOfDay toHour toMinutes _) spanMultiple) =
        object [
            "id" .= entryId,
            "userId" .= userId, 
            "eventId" .= eventId,
            "date" .= Database.formatDate date,
            "fromTime" .= ((Database.showWithZeros fromHour) Import.++ ":" Import.++ (Database.showWithZeros fromMinutes)),
            "toTime" .= ((Database.showWithZeros toHour) Import.++ ":" Import.++ (Database.showWithZeros toMinutes)),
            "spanMultiple" .= spanMultiple
        ]

convertFreeTimeEntryToLocal :: Entity FreeTimeEntry -> Text -> Maybe FreeTimeEntryData
convertFreeTimeEntryToLocal (Entity entryId (FreeTimeEntry userId day fromTime toTime spanMultiple)) timezone = do 
    let maybeLocalFromTime = Database.convertUTCToLocal fromTime timezone
    let maybeLocalToTime = Database.convertUTCToLocal toTime timezone
    case (maybeLocalFromTime, maybeLocalToTime) of 
        (Just (fromTimeDayOffset, localFromTime), Just (toTimeDayOffset, localToTime)) -> do 
            -- The database will hold in the day of fromTime if the event is staggered 
            -- between to two days 
            let maybeLocalDay = Database.updateDayString day fromTimeDayOffset
            case maybeLocalDay of 
                Just localDay -> do 
                    let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
                    return $ FreeTimeEntryData entryId userId localDay localFromTime localToTime newSpanMultiple
                _ -> Nothing
        (_, _) -> Nothing

convertAvailableTimeEntryToLocal :: Entity AvailableTimeEntry -> Text -> Maybe AvailableTimeEntryData
convertAvailableTimeEntryToLocal (Entity entryId (AvailableTimeEntry userId eventId date fromTime toTime spanMultiple)) timezone = do 
    let maybeLocalFromTime = Database.convertUTCToLocal fromTime timezone
    let maybeLocalToTime = Database.convertUTCToLocal toTime timezone
    case (maybeLocalFromTime, maybeLocalToTime) of 
        (Just (fromTimeDayOffset, localFromTime), Just (toTimeDayOffset, localToTime)) -> do 
            -- The database will hold in the day of fromTime if the event is staggered 
            -- between to two days 
            let localDate = addDays fromTimeDayOffset date
            let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
            return $ AvailableTimeEntryData entryId userId eventId localDate localFromTime localToTime newSpanMultiple
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
    maybeTimezone <- lookupPostParam "timezone"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    maybeUserId <- Database.fetchUserId (Just userIdText)
    let maybeUTCFromTime = Database.convertTextToTime maybeFromTimeText maybeTimezone
    let maybeUTCToTime = Database.convertTextToTime maybeToTimeText maybeTimezone
    case (maybeUserId, maybeDay, maybeUTCFromTime, maybeUTCToTime, maybeTimezone) of
        (Just userId, Just day, Just (fromTimeDayOffset, fromTime), Just (toTimeDayOffset, toTime), Just timezone) -> do
            -- The database will hold in the day of fromTime if the event is staggered 
            -- between to two days 
            let maybeUtcDay = Database.updateDayString day fromTimeDayOffset
            case maybeUtcDay of 
                Just utcDay -> do
                    let spanMultiple = if fromTimeDayOffset == toTimeDayOffset then False else True
                    let timeEntry' = FreeTimeEntry userId (toLower utcDay) fromTime toTime spanMultiple
                    insertedEntity <- runDB $ insertEntity timeEntry'
                    let freeTimeEntryData = convertFreeTimeEntryToLocal insertedEntity timezone
                    case freeTimeEntryData of
                        Just entity -> returnJson $ entity
                        Nothing -> invalidArgs ["Created in database, failed to convert back to local time"]
                _ -> invalidArgs ["Failed to parse day, from_time and/or to_time params"]
        (_, _, _, _, _) -> invalidArgs ["Failed to parse day, from_time and/or to_time params"]

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
        (Just day, Just (fromTimeDayOffset, fromTime), Just (toTimeDayOffset, toTime), Right (entryIdInt, "")) -> do
            allTimeEntries <- runDB $ selectList [FreeTimeEntryId ==. toSqlKey (fromIntegral (entryIdInt::Integer))] []
            case allTimeEntries of 
                [Entity entryId (FreeTimeEntry _ _ _ _ spanMultiple)] -> do 
                    -- The database will hold in the day of fromTime if the event is staggered 
                    -- between to two days 
                    let maybeUtcDay = Database.updateDayString day fromTimeDayOffset
                    case maybeUtcDay of 
                        Just utcDay -> do
                            let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
                            runDB $ update entryId 
                                [
                                    FreeTimeEntryDay =. (toLower utcDay),
                                    FreeTimeEntryFromTime =. fromTime, 
                                    FreeTimeEntryToTime =. toTime,
                                    FreeTimeEntrySpanMultiple =. newSpanMultiple
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
    maybeTimezone <- lookupPostParam "timezone"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    maybeUserId <- Database.fetchUserId (Just userIdText)
    let maybeDate = Database.convertTextToDate maybeDateText
    let maybeUTCFromTime = Database.convertTextToTime maybeFromTimeText maybeTimezone
    let maybeUTCToTime = Database.convertTextToTime maybeToTimeText maybeTimezone
    case (maybeUserId, maybeDate, maybeEventId, maybeUTCFromTime, maybeUTCToTime, maybeTimezone) of
        (Just userId, Just date, Just eventIdText, Just (fromTimeDayOffset, fromTime), Just (toTimeDayOffset, toTime), Just timezone) -> do
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
                            let spanMultiple = if fromTimeDayOffset == toTimeDayOffset then False else True
                            let timeEntry' = AvailableTimeEntry userId eventId utcDate fromTime toTime spanMultiple
                            insertedEntity <- runDB $ insertEntity timeEntry'
                            let availableTimeEntryData = convertAvailableTimeEntryToLocal insertedEntity timezone
                            case availableTimeEntryData of
                                Just entity -> returnJson $ entity
                                Nothing -> invalidArgs ["Created in database, failed to convert back to local time"]
                        _ -> invalidArgs ["Failed to find corresponding ProposedEvent with id: " Import.++ eventIdText]
                _ -> invalidArgs ["Please provide a valid integer for event_id"]
        (_, _, _, _, _, _) -> invalidArgs ["Failed to parse API params"]

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
        (Just date, Just (fromTimeDayOffset, fromTime), Just (toTimeDayOffset, toTime), Right (entryIdInt, "")) -> do
            allTimeEntries <- runDB $ selectList [AvailableTimeEntryId ==. toSqlKey (fromIntegral (entryIdInt::Integer))] []
            case allTimeEntries of 
                [Entity entryId (AvailableTimeEntry _ _ _ _ _ spanMultiple)] -> do 
                    -- The database will hold in the date of fromTime if the event is staggered 
                    -- between to two days
                    let utcDate = addDays fromTimeDayOffset date
                    let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
                    runDB $ update entryId 
                        [
                            AvailableTimeEntryDate =. utcDate,
                            AvailableTimeEntryFromTime =. fromTime, 
                            AvailableTimeEntryToTime =. toTime,
                            AvailableTimeEntrySpanMultiple =. newSpanMultiple
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

createAvailableFromFree :: [FreeTimeEntry] -> Day -> Day -> Key ProposedEvent -> [AvailableTimeEntry]
createAvailableFromFree entries fromDate toDate eventId
    | fromDate > toDate = []
    | otherwise =  (createAvailableFromFreeHelper entries fromDate eventId) Import.++ 
        (createAvailableFromFree entries (addDays 1 fromDate) toDate eventId)

createAvailableFromFreeHelper :: [FreeTimeEntry] -> Day -> Key ProposedEvent -> [AvailableTimeEntry]
createAvailableFromFreeHelper [] _ _ = []
createAvailableFromFreeHelper ((FreeTimeEntry userId day fromTime toTime spanMultiple):xs) date eventId = 
    case ((toLower $ pack $ show $ dateWeekDay (dayToDateTime date)) == day && spanMultiple == False, 
        (toLower $ pack $ show $ dateWeekDay (dayToDateTime (addDays (-1) date))) == day && spanMultiple == True) of 
        (True, False) -> [AvailableTimeEntry userId eventId date fromTime toTime spanMultiple] Import.++ 
            createAvailableFromFreeHelper xs date eventId
        (False, True) -> [AvailableTimeEntry userId eventId (addDays (-1) date) fromTime toTime spanMultiple] Import.++ 
            createAvailableFromFreeHelper xs date eventId
        (_, _) -> createAvailableFromFreeHelper xs date eventId

iterateConfirmedEvents :: [AvailableTimeEntry] -> [Handler.Event.ConfirmedEventData] -> [AvailableTimeEntry]
iterateConfirmedEvents entries [] = entries 
iterateConfirmedEvents entries (x:xs) = let new_entries = iterateAvailableTimeEntries entries x in iterateConfirmedEvents new_entries xs

iterateAvailableTimeEntries :: [AvailableTimeEntry] -> Handler.Event.ConfirmedEventData -> [AvailableTimeEntry]
iterateAvailableTimeEntries [] _ = []
iterateAvailableTimeEntries (x:xs) confirmed_event = (splitFreeTime x confirmed_event) Import.++ (iterateAvailableTimeEntries xs confirmed_event)

splitFreeTime :: AvailableTimeEntry -> Handler.Event.ConfirmedEventData -> [AvailableTimeEntry]
splitFreeTime entry@(AvailableTimeEntry userId eventId dateA fromTimeA toTimeA spanMultipleA) 
    (Handler.Event.ConfirmedEventData _ _ _ _ _ _ dateC fromTimeC toTimeC spanMultipleC) = do 
    case (spanMultipleA, spanMultipleC) of 
        (False, False) -> case 
            (fromTimeC <= fromTimeA && toTimeC >= toTimeA && dateA == dateC,
            fromTimeC <= fromTimeA && toTimeC < toTimeA && toTimeC > fromTimeA && dateA == dateC,
            fromTimeC > fromTimeA && toTimeC >= toTimeA && fromTimeC < toTimeA && dateA == dateC,
            fromTimeC > fromTimeA && toTimeC < toTimeA && dateA == dateC) of 
                (True, False, False, False) -> [] 
                (False, True, False, False) ->
                    [AvailableTimeEntry userId eventId dateA toTimeC toTimeA spanMultipleA] 
                (False, False, True, False) ->
                    [AvailableTimeEntry userId eventId dateA fromTimeA fromTimeC spanMultipleA] 
                (False, False, False, True) ->
                    [AvailableTimeEntry userId eventId dateA fromTimeA fromTimeC spanMultipleA,
                        AvailableTimeEntry userId eventId dateA toTimeC toTimeA spanMultipleA] 
                _ -> [entry]
        (True, False) -> case 
            (fromTimeC <= fromTimeA && toTimeC > fromTimeA && dateA == dateC,
            toTimeC >= toTimeA && fromTimeC < toTimeA && dateA == (addDays 1 dateC),
            fromTimeC > fromTimeA && dateA == dateC,
            toTimeC < toTimeA && dateA == (addDays 1 dateC)) of
                (True, False, False, False) ->
                    [AvailableTimeEntry userId eventId dateA toTimeC toTimeA spanMultipleA]
                (False, True, False, False) ->
                    [AvailableTimeEntry userId eventId dateA fromTimeA fromTimeC spanMultipleA]
                (False, False, True, False) ->
                    [AvailableTimeEntry userId eventId dateA fromTimeA fromTimeC False,
                        AvailableTimeEntry userId eventId dateA toTimeC toTimeA spanMultipleA]
                (False, False, False, True) ->
                    [AvailableTimeEntry userId eventId dateA fromTimeA fromTimeC spanMultipleA,
                        AvailableTimeEntry userId eventId dateA toTimeC toTimeA False]
                _ -> [entry]
        (False, True) -> case 
            (fromTimeC <= fromTimeA && dateA == dateC,
            fromTimeC > fromTimeA && fromTimeC < toTimeC && dateA == dateC,
            toTimeC >= toTimeA && (addDays 1 dateA) == dateC,
            toTimeC < toTimeA && toTimeC > fromTimeA && (addDays 1 dateA) == dateC) of 
                (True, False, False, False) -> []
                (False, True, False, False) -> 
                    [AvailableTimeEntry userId eventId dateA fromTimeA fromTimeC spanMultipleA]
                (False, False, True, False) -> []
                (False, False, False, True) -> 
                    [AvailableTimeEntry userId eventId dateA toTimeC toTimeA spanMultipleA]
                _ -> [entry]
        (True, True) -> case 
            (fromTimeC <= fromTimeA && toTimeC >= toTimeA && dateA == dateC,
            fromTimeC > fromTimeA && toTimeC >= toTimeA && dateA == dateC,
            fromTimeC <= fromTimeA && toTimeC < toTimeA && dateA == dateC,
            fromTimeC > fromTimeA && toTimeC < toTimeA && dateA == dateC) of 
                (True, False, False, False) -> []
                (False, True, False, False) -> 
                    [AvailableTimeEntry userId eventId dateA fromTimeA fromTimeC False]
                (False, False, True, False) -> 
                    [AvailableTimeEntry userId eventId (addDays 1 dateA) toTimeC toTimeA False]
                (False, False, False, True) -> 
                    [AvailableTimeEntry userId eventId dateA fromTimeA fromTimeC False,
                        AvailableTimeEntry userId eventId (addDays 1 dateA) toTimeC toTimeA False]
                _ -> [entry] -- not possible to reach here

insertEntriesToDatabase :: [AvailableTimeEntry] -> Handler [Entity AvailableTimeEntry]
insertEntriesToDatabase [] = do return []
insertEntriesToDatabase (x:xs) = do 
    insertedEntity <- runDB $ insertEntity x
    recursed_entities <- insertEntriesToDatabase xs 
    return $ [insertedEntity] Import.++ recursed_entities

postFreeToAvailableTimeEntryR :: Text -> Handler Value 
postFreeToAvailableTimeEntryR eventIdText = do 
    fetchProposedEvent <- Database.fetchProposedEvent (Just eventIdText)
    maybeTimeZone <- lookupPostParam "timezone"
    case (fetchProposedEvent, maybeTimeZone) of 
        (Just (Entity eventId (ProposedEvent _ userId _ _ fromDate toDate False)), Just timezone) -> do 
            allEntityFreeTimeEntries <- runDB $ selectList [FreeTimeEntryUserId ==. userId] []
            allProposedEvents <- runDB $ selectList 
                ([ProposedEventCreatorId ==. userId, ProposedEventConfirmed ==. True] 
                ||. [ProposedEventRecipientId ==. userId, ProposedEventConfirmed ==. True]) []
            allConfirmedEvents <- Import.mapM Handler.Event.getConfirmedEventFromProposedEvent allProposedEvents
            let allFreeTimeEntries = Import.map (\(Entity _ a) -> a) allEntityFreeTimeEntries
            let potentialAvailableTimeEntries = createAvailableFromFree allFreeTimeEntries fromDate toDate eventId
            let allAvailableTimes = iterateConfirmedEvents potentialAvailableTimeEntries (catMaybes allConfirmedEvents)
            inserted_entries <- insertEntriesToDatabase allAvailableTimes
            -- returnJson potentialAvailableTimeEntries
            returnJson $ catMaybes $ Import.map (\x -> convertAvailableTimeEntryToLocal x timezone) inserted_entries
        (_, _) -> invalidArgs ["Failed to pass in valid arguments for timezone or route to valid eventId"]
