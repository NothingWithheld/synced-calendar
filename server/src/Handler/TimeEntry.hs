{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.TimeEntry where 

import Import 
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Data.Dates
import Data.HashMap.Strict
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Text.Read
import qualified Database
import qualified Handler.Event

data FreeTimeEntryData = FreeTimeEntryData FreeTimeEntryId UserId Text TimeOfDay TimeOfDay Bool
data AvailableTimeEntryData = AvailableTimeEntryData AvailableTimeEntryId UserId EventId Day TimeOfDay TimeOfDay Bool
data CommonAvailableTimeEntryData = CommonAvailableTimeEntryData Day TimeOfDay TimeOfDay Bool 

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

instance ToJSON CommonAvailableTimeEntryData where 
    toJSON (CommonAvailableTimeEntryData date (TimeOfDay fromHour fromMinutes _) (TimeOfDay toHour toMinutes _) spanMultiple) =
        object [
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

convertAvailableTimeEntryNoEntityLocal :: AvailableTimeEntry -> Text -> Maybe AvailableTimeEntry
convertAvailableTimeEntryNoEntityLocal (AvailableTimeEntry userId eventId date fromTime toTime spanMultiple) timezone = do 
    let maybeLocalFromTime = Database.convertUTCToLocal fromTime timezone
    let maybeLocalToTime = Database.convertUTCToLocal toTime timezone
    case (maybeLocalFromTime, maybeLocalToTime) of 
        (Just (fromTimeDayOffset, localFromTime), Just (toTimeDayOffset, localToTime)) -> do 
            -- The database will hold in the day of fromTime if the event is staggered 
            -- between to two days 
            let localDate = addDays fromTimeDayOffset date
            let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
            return $ AvailableTimeEntry userId eventId localDate localFromTime localToTime newSpanMultiple
        (_, _) -> Nothing

convertCommonAvailableTimeEntryToLocal :: CommonAvailableTimeEntryData -> Text -> Maybe CommonAvailableTimeEntryData
convertCommonAvailableTimeEntryToLocal (CommonAvailableTimeEntryData date fromTime toTime spanMultiple) timezone = do 
    let maybeLocalFromTime = Database.convertUTCToLocal fromTime timezone
    let maybeLocalToTime = Database.convertUTCToLocal toTime timezone
    case (maybeLocalFromTime, maybeLocalToTime) of 
        (Just (fromTimeDayOffset, localFromTime), Just (toTimeDayOffset, localToTime)) -> do 
            -- The database will hold in the day of fromTime if the event is staggered 
            -- between to two days 
            let localDate = addDays fromTimeDayOffset date
            let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
            return $ CommonAvailableTimeEntryData localDate localFromTime localToTime newSpanMultiple
        (_, _) -> Nothing

createAvailableTimeEntry :: (Text, Text, Text, Text, Text, Text) -> Handler (Maybe AvailableTimeEntryData)
createAvailableTimeEntry (userIdText, eventIdText, dateText, fromTimeText, toTimeText, timezone) = do 
    maybeUserId <- Database.fetchUserId (Just userIdText)
    maybeInvite <- Database.fetchProposedEventInvitation (Just eventIdText) (Just userIdText)
    let maybeDate = Database.convertTextToDate (Just dateText)
    let maybeUTCFromTime = Database.convertTextToTime (Just fromTimeText) (Just timezone)
    let maybeUTCToTime = Database.convertTextToTime (Just toTimeText)  (Just timezone)
    case (maybeUserId, maybeInvite, maybeDate, maybeUTCFromTime, maybeUTCToTime) of 
        (Just userId, Just (Entity _ (ProposedEventInvitation eventId _ _ _ _ _ _ _)), Just date, Just (fromTimeDayOffset, fromTime), Just (toTimeDayOffset, toTime)) -> do
            -- The database will hold in the date of fromTime if the event is staggered 
            -- between to two days
            let utcDate = addDays fromTimeDayOffset date
            let spanMultiple = if fromTimeDayOffset == toTimeDayOffset then False else True
            let timeEntry' = AvailableTimeEntry userId eventId utcDate fromTime toTime spanMultiple
            insertedEntity <- runDB $ insertEntity timeEntry'
            return $ convertAvailableTimeEntryToLocal insertedEntity timezone
        _ -> return Nothing

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
                            runDB $ Import.update entryId 
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

findCommonAvailableTimeEntriesMultiple :: [[AvailableTimeEntry]] -> [CommonAvailableTimeEntryData] -> [CommonAvailableTimeEntryData]
findCommonAvailableTimeEntriesMultiple [] commonEntries = commonEntries
findCommonAvailableTimeEntriesMultiple (x:xs) commonEntries = let newCommonEntries = findCommonAvailableTimeEntries2 x commonEntries 
    in findCommonAvailableTimeEntriesMultiple xs newCommonEntries

findCommonAvailableTimeEntries2 :: [AvailableTimeEntry] -> [CommonAvailableTimeEntryData] -> [CommonAvailableTimeEntryData]
findCommonAvailableTimeEntries2 _ [] = []
findCommonAvailableTimeEntries2 entries (x:xs) = (findCommonAvailableTimeEntry entries x) Import.++ (findCommonAvailableTimeEntries2 entries xs)

findCommonAvailableTimeEntry :: [AvailableTimeEntry] -> CommonAvailableTimeEntryData -> [CommonAvailableTimeEntryData]
findCommonAvailableTimeEntry [] _ = [] 
findCommonAvailableTimeEntry (x:xs) timeEntry = (findCommonAvailableTimeEntryHelper x timeEntry) Import.++ findCommonAvailableTimeEntry xs timeEntry

findCommonAvailableTimeEntryHelper :: AvailableTimeEntry -> CommonAvailableTimeEntryData -> [CommonAvailableTimeEntryData]
findCommonAvailableTimeEntryHelper (AvailableTimeEntry _ _ dateX fromTimeX toTimeX spanMultipleX) 
    (CommonAvailableTimeEntryData dateY fromTimeY toTimeY spanMultipleY) = do 
        case (spanMultipleX, spanMultipleY) of 
            (False, False) -> case 
                (toTimeX <= fromTimeY || fromTimeX >= toTimeY || not (dateX == dateY), -- out of bounds
                fromTimeX <= fromTimeY && toTimeX <= toTimeY, 
                fromTimeX <= fromTimeY && toTimeX > toTimeY, 
                fromTimeX > fromTimeY && toTimeX <= toTimeY,
                fromTimeX > fromTimeY && toTimeX > toTimeY) of 
                    (True, False, False, False, False) -> []
                    (False, True, False, False, False) -> 
                        [CommonAvailableTimeEntryData dateY fromTimeY toTimeX spanMultipleX]
                    (False, False, True, False, False) -> 
                        [CommonAvailableTimeEntryData dateY fromTimeY toTimeY spanMultipleX]
                    (False, False, False, True, False) -> 
                        [CommonAvailableTimeEntryData dateY fromTimeX toTimeX spanMultipleX]
                    (False, False, False, False, True) -> 
                        [CommonAvailableTimeEntryData dateY fromTimeX toTimeY spanMultipleX]
                    _ -> [] -- should never get here
            _ -> []


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
                    let availableTimesNoEntities = Import.map (\(Entity _ x) -> x) allTimeEntries
                    allProposedEventInvitations <- runDB $ selectList 
                        ([ProposedEventInvitationRecipientId ==. userId, ProposedEventInvitationConfirmed ==. True]) []
                    allConfirmedEventInvitations <- Import.mapM Handler.Event.getConfirmedEventInvitationFromProposedEventInvitation allProposedEventInvitations
                    let allAvailableTimes = iterateConfirmedEventInvitations availableTimesNoEntities (catMaybes allConfirmedEventInvitations)
                    returnJson $ catMaybes $ Import.map (\x -> convertAvailableTimeEntryNoEntityLocal x timezone) allAvailableTimes
                _ -> invalidArgs ["Failed to parse event_id params"]
        (_, _, _) -> invalidArgs ["Failed to parse event_id and/or timezone params"]

fetchAvailableTimeEntriesWithId :: Text -> Handler [Entity AvailableTimeEntry]
fetchAvailableTimeEntriesWithId eventIdText = do 
    let eitherEventId = decimal eventIdText
    case (eitherEventId) of 
        Right (eventIdInt, "") -> do 
            allTimeEntries <- runDB $ 
                selectList [
                    AvailableTimeEntryEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer))
                ] []
            return allTimeEntries
        _ -> return []

fetchConfirmedProposedEventInvitations :: Maybe (Key User) -> Handler [Entity ProposedEventInvitation]
fetchConfirmedProposedEventInvitations Nothing = do return []
fetchConfirmedProposedEventInvitations (Just userId) = do 
    allProposedEventInvitations <- runDB $ 
        selectList 
            [
                ProposedEventInvitationRecipientId ==. userId, 
                ProposedEventInvitationConfirmed ==. True
            ] []
    return allProposedEventInvitations

fetchConfirmedEventInvitationsData :: [Entity ProposedEventInvitation] -> Handler [Maybe Handler.Event.ConfirmedEventInvitationData]
fetchConfirmedEventInvitationsData proposedEvents = do 
    confirmedEvents <- Import.mapM Handler.Event.getConfirmedEventInvitationFromProposedEventInvitation proposedEvents
    return confirmedEvents

findUserIdFromAvailableTimeEntry :: [Entity AvailableTimeEntry] -> Maybe (Key User) 
findUserIdFromAvailableTimeEntry [] = Nothing
findUserIdFromAvailableTimeEntry ((Entity _ (AvailableTimeEntry userId _ _ _ _ _)):_) = Just userId

groupAvailableTimeEntriesByRecipient :: [Entity AvailableTimeEntry] -> HashMap Int64 [(Entity AvailableTimeEntry)] -> [[Entity AvailableTimeEntry]]
groupAvailableTimeEntriesByRecipient [] inviteMap = Data.HashMap.Strict.elems inviteMap
groupAvailableTimeEntriesByRecipient (invite@(Entity _ (AvailableTimeEntry userId _ _ _ _ _)):xs) inviteMap = do 
    let value = Data.HashMap.Strict.lookup (fromSqlKey userId) inviteMap 
    case value of 
        Just inviteList -> let newMap = Data.HashMap.Strict.insert (fromSqlKey userId) (inviteList Import.++ [invite]) inviteMap in 
            groupAvailableTimeEntriesByRecipient xs newMap 
        Nothing -> let newMap = Data.HashMap.Strict.insert (fromSqlKey userId) [invite] inviteMap in
            groupAvailableTimeEntriesByRecipient xs newMap

getAvailableTimeEntryCountR :: Text -> Handler Value 
getAvailableTimeEntryCountR eventIdText = do 
    let eitherEventId = decimal eventIdText
    case (eitherEventId) of
        Right (eventIdInt, "") -> do
            allProposedEventInvitations <- runDB $
                selectList 
                    [
                        ProposedEventInvitationEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer)), 
                        ProposedEventInvitationConfirmed ==. False
                    ] [] 
            availableTimeEntriesList <- fetchAvailableTimeEntriesWithId eventIdText 
            let groupedAvailableTimeEntries = groupAvailableTimeEntriesByRecipient availableTimeEntriesList Data.HashMap.Strict.empty
            return $ object 
                [
                    "count_submitted" .= (show $ Import.length groupedAvailableTimeEntries),
                    "total_recipients" .= (show $ Import.length allProposedEventInvitations)
                ]
        _ -> invalidArgs ["Failed to find event with id: " Import.++ eventIdText]

getAvailableTimeEntryMultipleR :: Text -> Handler Value 
getAvailableTimeEntryMultipleR eventIdText = do 
    maybeTimeZone <- lookupGetParam "timezone"
    case maybeTimeZone of 
        Just timezone -> do 
            availableTimeEntriesList <- fetchAvailableTimeEntriesWithId eventIdText 
            let groupedAvailableTimeEntries = groupAvailableTimeEntriesByRecipient availableTimeEntriesList Data.HashMap.Strict.empty
            let userIdsList = Import.map findUserIdFromAvailableTimeEntry groupedAvailableTimeEntries
            proposedEventsList <- Import.mapM fetchConfirmedProposedEventInvitations userIdsList
            confirmedEventsList <- Import.mapM fetchConfirmedEventInvitationsData proposedEventsList
            let zippedFcn = (\entries events -> iterateConfirmedEventInvitations entries (catMaybes events))
            let availableTimeEntriesNoEntitiesList = Import.map (\entries -> Import.map (\(Entity _ x) -> x) entries) groupedAvailableTimeEntries
            let availableTimesEntriesListNoConflicts = Import.zipWith zippedFcn availableTimeEntriesNoEntitiesList confirmedEventsList
            let mappedFcn = (\(AvailableTimeEntry _ _ date fromTime toTime spanMultiple) -> CommonAvailableTimeEntryData date fromTime toTime spanMultiple) 
            case Import.length availableTimesEntriesListNoConflicts of 
                0 -> returnJson $ object []
                _ -> do 
                    let commonEntriesFirst = Import.map mappedFcn (availableTimesEntriesListNoConflicts !! 0)
                    let commonEntries = findCommonAvailableTimeEntriesMultiple availableTimesEntriesListNoConflicts commonEntriesFirst
                    let commonEntriesLocal = Import.map (\x -> convertCommonAvailableTimeEntryToLocal x timezone) commonEntries
                    returnJson $ commonEntriesLocal
        _ -> invalidArgs ["Failed to parse event_id params"]

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
                    allProposedEventInvitations <- runDB $ 
                        selectList [
                            ProposedEventInvitationRecipientId ==. userId,
                            ProposedEventInvitationEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer)),
                            ProposedEventInvitationConfirmed <-. [False]
                        ] []
                    case allProposedEventInvitations of 
                        [Entity _ (ProposedEventInvitation eventId _ _ _ _ _ _ _)] -> do
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
                        _ -> invalidArgs ["Failed to find corresponding ProposedEventInvitation with id: " Import.++ eventIdText]
                _ -> invalidArgs ["Please provide a valid integer for event_id"]
        (_, _, _, _, _, _) -> invalidArgs ["Failed to parse API params"]

postAvailableTimeEntryMultipleR :: Text -> Handler Value 
postAvailableTimeEntryMultipleR userIdText = do 
    maybeEventId <- lookupPostParam "event_id"
    maybeDatesText <- lookupPostParam "dates"
    maybeTimezone <- lookupPostParam "timezone"
    maybeFromTimesText <- lookupPostParam "from_times"
    maybeToTimesText <- lookupPostParam "to_times"
    case (maybeDatesText, maybeEventId, maybeFromTimesText, maybeToTimesText, maybeTimezone) of
        (Just datesText, Just eventIdText, Just fromTimesText, Just toTimesText, Just timezone) -> do
            let dates = Database.splitStringByCommas datesText
            let fromTimes = Database.splitStringByCommas fromTimesText 
            let toTimes = Database.splitStringByCommas toTimesText
            case Import.length dates == Import.length fromTimes && Import.length dates == Import.length toTimes of 
                True -> do 
                    let zipLists = Import.zip3 dates fromTimes toTimes
                    let arguments = Import.map (\(date, fromTime, toTime) -> (userIdText, eventIdText, date, fromTime, toTime, timezone)) zipLists
                    unwrapped_events <- Import.mapM createAvailableTimeEntry arguments
                    returnJson $ catMaybes unwrapped_events
                False -> invalidArgs ["Please provide the same number of dates, from_times, and to_time"]
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
        (Just date, Just (fromTimeDayOffset, fromTime), Just (toTimeDayOffset, toTime), Right (entryIdInt, "")) -> do
            allTimeEntries <- runDB $ selectList [AvailableTimeEntryId ==. toSqlKey (fromIntegral (entryIdInt::Integer))] []
            case allTimeEntries of 
                [Entity entryId (AvailableTimeEntry _ _ _ _ _ spanMultiple)] -> do 
                    -- The database will hold in the date of fromTime if the event is staggered 
                    -- between to two days
                    let utcDate = addDays fromTimeDayOffset date
                    let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
                    runDB $ Import.update entryId 
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

createAvailableFromFree :: [FreeTimeEntry] -> Day -> Day -> Key Event -> [AvailableTimeEntry]
createAvailableFromFree entries fromDate toDate eventId
    | fromDate > toDate = []
    | otherwise =  (createAvailableFromFreeHelper entries fromDate eventId) Import.++ 
        (createAvailableFromFree entries (addDays 1 fromDate) toDate eventId)

createAvailableFromFreeHelper :: [FreeTimeEntry] -> Day -> Key Event -> [AvailableTimeEntry]
createAvailableFromFreeHelper [] _ _ = []
createAvailableFromFreeHelper ((FreeTimeEntry userId day fromTime toTime spanMultiple):xs) date eventId = 
    case ((toLower $ pack $ show $ dateWeekDay (dayToDateTime date)) == day && spanMultiple == False, 
        (toLower $ pack $ show $ dateWeekDay (dayToDateTime (addDays (-1) date))) == day && spanMultiple == True) of 
        (True, False) -> [AvailableTimeEntry userId eventId date fromTime toTime spanMultiple] Import.++ 
            createAvailableFromFreeHelper xs date eventId
        (False, True) -> [AvailableTimeEntry userId eventId (addDays (-1) date) fromTime toTime spanMultiple] Import.++ 
            createAvailableFromFreeHelper xs date eventId
        (_, _) -> createAvailableFromFreeHelper xs date eventId

iterateConfirmedEventInvitations :: [AvailableTimeEntry] -> [Handler.Event.ConfirmedEventInvitationData] -> [AvailableTimeEntry]
iterateConfirmedEventInvitations entries [] = entries 
iterateConfirmedEventInvitations entries (x:xs) = let newEntries = iterateAvailableTimeEntries entries x in iterateConfirmedEventInvitations newEntries xs

iterateAvailableTimeEntries :: [AvailableTimeEntry] -> Handler.Event.ConfirmedEventInvitationData -> [AvailableTimeEntry]
iterateAvailableTimeEntries [] _ = []
iterateAvailableTimeEntries (x:xs) confirmedEvent = (splitFreeTime x confirmedEvent) Import.++ (iterateAvailableTimeEntries xs confirmedEvent)

splitFreeTime :: AvailableTimeEntry -> Handler.Event.ConfirmedEventInvitationData -> [AvailableTimeEntry]
splitFreeTime entry@(AvailableTimeEntry userId eventId dateA fromTimeA toTimeA spanMultipleA) 
    (Handler.Event.ConfirmedEventInvitationData _ _ _ _ _ _ dateC fromTimeC toTimeC spanMultipleC) = do 
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

getFreeToAvailableTimeEntryR :: Text -> Handler Value 
getFreeToAvailableTimeEntryR userIdText = do 
    maybeEventId <- lookupGetParam "event_id"
    maybeTimeZone <- lookupGetParam "timezone"
    fetchProposedEventInvitation <- Database.fetchProposedEventInvitation maybeEventId (Just userIdText)
    case (fetchProposedEventInvitation, maybeTimeZone) of 
        (Just (Entity _ (ProposedEventInvitation eventId _ userId _ _ fromDate toDate False)), Just timezone) -> do 
            allEntityFreeTimeEntries <- runDB $ selectList [FreeTimeEntryUserId ==. userId] []
            allProposedEventInvitations <- runDB $ selectList 
                ([ProposedEventInvitationRecipientId ==. userId, ProposedEventInvitationConfirmed ==. True]) []
            allConfirmedEventInvitations <- Import.mapM Handler.Event.getConfirmedEventInvitationFromProposedEventInvitation allProposedEventInvitations
            let allFreeTimeEntries = Import.map (\(Entity _ a) -> a) allEntityFreeTimeEntries
            let potentialAvailableTimeEntries = createAvailableFromFree allFreeTimeEntries fromDate toDate eventId
            let allAvailableTimes = iterateConfirmedEventInvitations potentialAvailableTimeEntries (catMaybes allConfirmedEventInvitations)
            returnJson $ catMaybes $ Import.map (\x -> convertAvailableTimeEntryNoEntityLocal x timezone) allAvailableTimes
        (_, _) -> invalidArgs ["Failed to pass in valid arguments for timezone or event_id"]
