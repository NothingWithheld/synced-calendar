{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.Event where 

import Import
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Data.HashMap.Strict
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Text.Read
import qualified Database

data ConfirmedEventData = ConfirmedEventData EventId UserId [Text] (Maybe Text) (Maybe Text) Day TimeOfDay TimeOfDay Bool

instance ToJSON ConfirmedEventData where 
    toJSON (ConfirmedEventData eventId creatorId recipientEmails name description date (TimeOfDay fromHour fromMinutes _) (TimeOfDay toHour toMinutes _) spanMultiple) = 
        object [
            "eventId" .= eventId,
            "creatorId" .= creatorId,
            "recipientEmails" .= recipientEmails,
            "name" .= name,
            "description" .= description,
            "date" .= Database.formatDate date,
            "fromTime" .= ((Database.showWithZeros fromHour) Import.++ ":" Import.++ (Database.showWithZeros fromMinutes)),
            "toTime" .= ((Database.showWithZeros toHour) Import.++ ":" Import.++ (Database.showWithZeros toMinutes)),
            "spanMultiple" .= spanMultiple
        ]

data ProposedEventData = ProposedEventData EventId UserId [Text] (Maybe Text) (Maybe Text) Day Day

instance ToJSON ProposedEventData where 
    toJSON (ProposedEventData eventId creatorId recipientEmails name description fromDate toDate) = 
        object [
            "eventId" .= eventId,
            "creatorId" .= creatorId,
            "recipientEmails" .= recipientEmails,
            "name" .= name,
            "description" .= description,
            "fromDate" .= Database.formatDate fromDate,
            "toDate" .= Database.formatDate toDate
        ]

data ConfirmedEventInvitationData = ConfirmedEventInvitationData ConfirmedEventInvitationId EventId UserId UserId (Maybe Text) (Maybe Text) Day TimeOfDay TimeOfDay Bool

instance ToJSON ConfirmedEventInvitationData where 
    toJSON (ConfirmedEventInvitationData _ eventId creatorId recipientId name description date (TimeOfDay fromHour fromMinutes _) (TimeOfDay toHour toMinutes _) spanMultiple) =
        object [
            "eventId" .= eventId, 
            "creatorId" .= creatorId,
            "recipientId" .= recipientId,
            "name" .= name,
            "description" .= description,
            "date" .= Database.formatDate date,
            "fromTime" .= ((Database.showWithZeros fromHour) Import.++ ":" Import.++ (Database.showWithZeros fromMinutes)),
            "toTime" .= ((Database.showWithZeros toHour) Import.++ ":" Import.++ (Database.showWithZeros toMinutes)),
            "spanMultiple" .= spanMultiple
        ]

data ProposedEventInvitationData = ProposedEventInvitationData ProposedEventInvitationId EventId UserId UserId (Maybe Text) (Maybe Text) Day Day Bool

instance ToJSON ProposedEventInvitationData where 
    toJSON (ProposedEventInvitationData _ eventId creatorId recipientId name description fromDate toDate confirmed) = 
        object [
            "eventId" .= eventId,
            "creatorId" .= creatorId,
            "recipientId" .= recipientId,
            "name" .= name,
            "description" .= description,
            "fromDate" .= Database.formatDate fromDate,
            "toDate" .= Database.formatDate toDate, 
            "confirmed" .= confirmed 
        ]

convertConfirmedEventInvitationToLocal :: ConfirmedEventInvitationData -> Text -> Maybe ConfirmedEventInvitationData
convertConfirmedEventInvitationToLocal 
    (ConfirmedEventInvitationData confirmedEventId eventId creatorId recipientId name description date fromTime toTime spanMultiple) timezone = do 
        let maybeLocalFromTime = Database.convertUTCToLocal fromTime timezone
        let maybeLocalToTime = Database.convertUTCToLocal toTime timezone
        case (maybeLocalFromTime, maybeLocalToTime) of 
            (Just (fromTimeDayOffset, localFromTime), Just (toTimeDayOffset, localToTime)) -> do 
                -- The database will hold in the day of fromTime if the event is staggered 
                -- between to two days 
                let localDate = addDays fromTimeDayOffset date
                let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
                return $ ConfirmedEventInvitationData confirmedEventId eventId creatorId recipientId name description localDate localFromTime localToTime newSpanMultiple
            (_, _) -> Nothing

getConfirmedEventInvitationFromProposedEventInvitation :: Entity ProposedEventInvitation -> Handler (Maybe ConfirmedEventInvitationData)
getConfirmedEventInvitationFromProposedEventInvitation 
    (Entity proposedEventId (ProposedEventInvitation eventId creatorId recipientId name description _ _ _)) = do 
        allEvents <- runDB $ selectList [ConfirmedEventInvitationProposedEventInvitationId ==. proposedEventId] []
        case allEvents of 
            [Entity confirmedEventId (ConfirmedEventInvitation _ date fromTime toTime spanMultiple)] -> 
                return $ Just $ ConfirmedEventInvitationData confirmedEventId eventId creatorId recipientId name description date fromTime toTime spanMultiple
            _ -> return $ Nothing

createProposedEventInvitation :: (Key Event, Text, Text, Maybe Text, Maybe Text, Day, Day) -> Handler (Maybe ProposedEventInvitationData)
createProposedEventInvitation (eventId, recipientEmail, creatorIdText, maybeName, maybeDescription, fromDate, toDate) = do 
    let confirmed = False
    maybeCreatorId <- Database.fetchUserId (Just creatorIdText)
    maybeRecipientId <- Database.fetchUserIdByEmail (Just recipientEmail)
    case (maybeCreatorId, maybeRecipientId) of 
        (Just creatorId, Just recipientId) -> do 
            let invite' = ProposedEventInvitation eventId creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
            (Entity inviteId _) <- runDB $ insertEntity invite'
            return $ Just $ ProposedEventInvitationData inviteId eventId creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
        (Just creatorId, Nothing) -> do 
            -- create a user for the email, but have registred be False
            let newUser' = User recipientEmail Nothing False
            Entity recipientId _ <- runDB $ insertEntity newUser'
            let invite' = ProposedEventInvitation eventId creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
            (Entity inviteId _) <- runDB $ insertEntity invite'
            return $ Just $ ProposedEventInvitationData inviteId eventId creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
        (_, _) -> return Nothing

fetchProposedRecipientEmail :: Entity ProposedEventInvitation -> Handler (Maybe Text)
fetchProposedRecipientEmail (Entity _ (ProposedEventInvitation _ _ recipientId _ _ _ _ _)) = do 
    emails <- runDB $ 
        selectList [
            UserId ==. recipientId
        ] [] 
    case emails of 
        [(Entity _ (User email _ _))] -> return $ Just email 
        _ -> return $ Nothing

createProposedEventDataFromMap :: [(Int64, [Entity ProposedEventInvitation])] -> Handler [ProposedEventData]
createProposedEventDataFromMap [] = do return []
createProposedEventDataFromMap ((eventId, invites):xs) = do 
    let (Entity _ (ProposedEventInvitation _ creatorId _ name description fromDate toDate _)) = invites !! 0 
    recipientEmails <- Import.mapM fetchProposedRecipientEmail invites 
    xsProposedEventData <- createProposedEventDataFromMap xs
    return $ [(ProposedEventData (toSqlKey eventId) creatorId (catMaybes recipientEmails) name description fromDate toDate)] Import.++ 
        xsProposedEventData

groupProposedInvitationsToEvents :: [Entity ProposedEventInvitation] -> HashMap Int64 [(Entity ProposedEventInvitation)] -> Handler [ProposedEventData] 
groupProposedInvitationsToEvents [] inviteMap = createProposedEventDataFromMap (Data.HashMap.Strict.toList inviteMap)
groupProposedInvitationsToEvents (invite@(Entity _ (ProposedEventInvitation eventId _ _ _ _ _ _ _)):xs) inviteMap = do 
    let value = Data.HashMap.Strict.lookup (fromSqlKey eventId) inviteMap 
    case value of 
        Just inviteList -> let newMap = Data.HashMap.Strict.insert (fromSqlKey eventId) (inviteList Import.++ [invite]) inviteMap in 
            groupProposedInvitationsToEvents xs newMap 
        Nothing -> let newMap = Data.HashMap.Strict.insert (fromSqlKey eventId) [invite] inviteMap in
            groupProposedInvitationsToEvents xs newMap

fetchConfirmedRecipientEmail :: ConfirmedEventInvitationData -> Handler (Maybe Text)
fetchConfirmedRecipientEmail (ConfirmedEventInvitationData _ _ _ recipientId _ _ _ _ _ _) = do 
    emails <- runDB $ 
        selectList [
            UserId ==. recipientId
        ] [] 
    case emails of 
        [(Entity _ (User email _ _))] -> return $ Just email 
        _ -> return $ Nothing

createConfirmedEventDataFromMap :: [(Int64, [ConfirmedEventInvitationData])] -> Handler [ConfirmedEventData]
createConfirmedEventDataFromMap [] = do return []
createConfirmedEventDataFromMap ((_, invites):xs) = do 
    let (ConfirmedEventInvitationData _ eventId creatorId _ name description date fromTime toTime spanMultiple) = invites !! 0 
    recipientEmails <- Import.mapM fetchConfirmedRecipientEmail invites 
    xsConfirmedEventData <- createConfirmedEventDataFromMap xs
    return $ [(ConfirmedEventData eventId creatorId (catMaybes recipientEmails) name description date fromTime toTime spanMultiple)] Import.++ 
        xsConfirmedEventData

groupConfirmedInvitationsToEvents :: [ConfirmedEventInvitationData] -> HashMap Int64 [(ConfirmedEventInvitationData)] -> Handler [ConfirmedEventData] 
groupConfirmedInvitationsToEvents [] inviteMap = createConfirmedEventDataFromMap (Data.HashMap.Strict.toList inviteMap)
groupConfirmedInvitationsToEvents (invite@(ConfirmedEventInvitationData _ eventId _ _ _ _ _ _ _ _):xs) inviteMap = do 
    let value = Data.HashMap.Strict.lookup (fromSqlKey eventId) inviteMap 
    case value of 
        Just inviteList -> let newMap = Data.HashMap.Strict.insert (fromSqlKey eventId) (inviteList Import.++ [invite]) inviteMap in 
            groupConfirmedInvitationsToEvents xs newMap 
        Nothing -> let newMap = Data.HashMap.Strict.insert (fromSqlKey eventId) [invite] inviteMap in
            groupConfirmedInvitationsToEvents xs newMap

getProposedEventCreatorR :: Text -> Handler Value
getProposedEventCreatorR creatorIdText = do 
    maybeCreatorId <- Database.fetchUserId (Just creatorIdText)
    case maybeCreatorId of
        Just creatorId -> do 
            allInvites <- runDB $ 
                selectList [
                    ProposedEventInvitationCreatorId <-. [creatorId],
                    ProposedEventInvitationConfirmed <-. [False]
                ] []
            event <- groupProposedInvitationsToEvents allInvites Data.HashMap.Strict.empty
            returnJson event
        _ -> invalidArgs ["Failed to find user with creatorId: " Import.++ creatorIdText]

postProposedEventCreatorR :: Text -> Handler Value 
postProposedEventCreatorR creatorIdText = do 
    maybeRecipientEmailsText <- lookupPostParam "recipient_emails"
    maybeName <- lookupPostParam "name"
    maybeDescription <- lookupPostParam "description"
    maybeFromDateText <- lookupPostParam "from_date"
    maybeToDateText <- lookupPostParam "to_date"
    let maybeFromDate = Database.convertTextToDate maybeFromDateText
    let maybeToDate = Database.convertTextToDate maybeToDateText
    case (maybeRecipientEmailsText, maybeFromDate, maybeToDate) of
        (Just recipientEmailsText, Just fromDate, Just toDate) -> do
            (Entity eventId Event) <- runDB $ insertEntity Event
            let recipientEmails = Database.splitStringByCommas recipientEmailsText
            let arguments = Import.map (\x -> (eventId, x, creatorIdText, maybeName, maybeDescription, fromDate, toDate)) recipientEmails
            unwrappedInvites <- Import.mapM createProposedEventInvitation arguments
            let unwrappedInvitesNoMaybes = catMaybes unwrappedInvites
            case Import.length unwrappedInvitesNoMaybes of 
                0 -> return $ object $ []
                _ -> do 
                    let (ProposedEventInvitationData _ _ creatorId _ _ _ _ _ _) = unwrappedInvitesNoMaybes !! 0
                    returnJson $ ProposedEventData eventId creatorId recipientEmails maybeName maybeDescription fromDate toDate
        (_, _, _) -> invalidArgs ["Failed to parse arguments. Check API documentation for valid formatting"]

putProposedEventCreatorR :: Text -> Handler Value 
putProposedEventCreatorR eventIdText = do 
    maybeRecipientIdText <- lookupPostParam "recipient_id"
    maybeNameText <- lookupPostParam "name"
    maybeDescriptionText <- lookupPostParam "description"
    maybeFromDateText <- lookupPostParam "from_date"
    maybeToDateText <- lookupPostParam "to_date"
    maybeConfirmedText <- lookupPostParam "confirmed"
    maybeRecipientId <- Database.fetchUserId maybeRecipientIdText
    let maybeFromDate = Database.convertTextToDate maybeFromDateText
    let maybeToDate = Database.convertTextToDate maybeToDateText
    let confirmed = Database.convertTextToBool maybeConfirmedText
    let eitherEventId = decimal eventIdText
    case (maybeRecipientId, maybeFromDate, maybeToDate, eitherEventId) of
        (Just recipientId, Just fromDate, Just toDate, Right (eventIdInt, "")) -> do
            allEvents <- runDB $ selectList [ProposedEventInvitationId ==. toSqlKey (fromIntegral (eventIdInt::Integer))] []
            case allEvents of 
                [Entity eventId ProposedEventInvitation {..}] -> do 
                    runDB $ Import.update eventId 
                        [
                            ProposedEventInvitationRecipientId =. recipientId,
                            ProposedEventInvitationName =. maybeNameText,
                            ProposedEventInvitationDescription =. maybeDescriptionText,
                            ProposedEventInvitationFromDate =. fromDate,
                            ProposedEventInvitationToDate =. toDate,
                            ProposedEventInvitationConfirmed =. confirmed
                        ]
                    return Null
                _ -> invalidArgs ["Failed to find event with id: " Import.++ eventIdText]
        (_, _, _, _) -> invalidArgs ["Failed to parse arguments. Check API documentation for valid formatting"]

deleteProposedEventCreatorR :: Text -> Handler Value 
deleteProposedEventCreatorR eventIdText = do 
    let eitherEventId = decimal eventIdText 
    case eitherEventId of 
        Right (eventIdInt, "") -> do 
            runDB $ deleteWhere [ProposedEventInvitationEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer))]
            return Null
        _ -> badMethod

getProposedEventRecipientR :: Text -> Handler Value 
getProposedEventRecipientR recipientIdText = do
    maybeRecipientId <- Database.fetchUserId (Just recipientIdText)
    case maybeRecipientId of 
        Just recipientId -> do
            allProposedEventInvitations <- runDB $ 
                selectList [
                    ProposedEventInvitationRecipientId <-. [recipientId],
                    ProposedEventInvitationConfirmed <-. [False]
                ] []
            returnJson $ Import.map (\(Entity inviteId (ProposedEventInvitation eventId creatorId _ name description fromDate toDate confirmed)) ->
                ProposedEventInvitationData inviteId eventId creatorId recipientId name description fromDate toDate confirmed) allProposedEventInvitations
        _ -> invalidArgs ["Failed to find user with id: " Import.++ recipientIdText]

getConfirmedEventCreatorR :: Text -> Handler Value
getConfirmedEventCreatorR creatorIdText = do 
    maybeTimeZone <- lookupGetParam "timezone"
    maybeCreatorId <- Database.fetchUserId (Just creatorIdText)
    case (maybeCreatorId, maybeTimeZone) of
        (Just creatorId, Just timezone) -> do 
            allProposedEventInvitationEntries <- runDB $ 
                selectList [
                    ProposedEventInvitationCreatorId <-. [creatorId],
                    ProposedEventInvitationConfirmed <-. [True]
                ] []
            confirmedInvites <- Import.mapM getConfirmedEventInvitationFromProposedEventInvitation allProposedEventInvitationEntries
            let localConfirmedInvites = catMaybes $ Import.map (\x -> convertConfirmedEventInvitationToLocal x timezone) (catMaybes confirmedInvites)
            confirmedEventData <- groupConfirmedInvitationsToEvents localConfirmedInvites Data.HashMap.Strict.empty
            returnJson confirmedEventData
        (_, _) -> invalidArgs ["Failed to parse timezone"]

createConfirmedEvent :: Entity ProposedEventInvitation -> Handler (Maybe ConfirmedEventInvitationData)
createConfirmedEvent (Entity inviteIdP (ProposedEventInvitation eventId creatorId recipientId name description _ _ _)) = do 
    maybeDateText <- lookupPostParam "date"
    maybeTimezone <- lookupPostParam "timezone"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeDate = Database.convertTextToDate maybeDateText
    let maybeUTCFromTime = Database.convertTextToTime maybeFromTimeText maybeTimezone
    let maybeUTCToTime = Database.convertTextToTime maybeToTimeText maybeTimezone
    case (maybeDate, maybeUTCFromTime, maybeUTCToTime, maybeTimezone) of 
        (Just date, Just (fromTimeDayOffset, fromTime), Just (toTimeDayOffset, toTime), Just timezone) -> do 
            -- The database will hold in the date of fromTime if the event is staggered 
            -- between to two days
            let utcDate = addDays fromTimeDayOffset date
            let spanMultiple = if fromTimeDayOffset == toTimeDayOffset then False else True
            let invite' = ConfirmedEventInvitation inviteIdP utcDate fromTime toTime spanMultiple
            (Entity inviteIdC _) <- runDB $ insertEntity invite'
            runDB $ Import.update inviteIdP [ProposedEventInvitationConfirmed =. True]
            let confirmedEventInvitationData = ConfirmedEventInvitationData inviteIdC eventId creatorId recipientId name description date fromTime toTime spanMultiple
            let localConfirmedEventInvitationData = convertConfirmedEventInvitationToLocal confirmedEventInvitationData timezone
            return localConfirmedEventInvitationData
        _ -> return Nothing

postConfirmedEventCreatorR :: Text -> Handler Value 
postConfirmedEventCreatorR eventIdText = do 
    -- Find the corresponding ProposedEventInvitation object
    let eitherEventId = decimal eventIdText
    case eitherEventId of 
        Right (eventIdInt, "") -> do
            allProposedEventInvitations <- runDB $ 
                selectList [
                    ProposedEventInvitationEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer)),
                    ProposedEventInvitationConfirmed <-. [False]
                ] []
            confirmedEventInvites <- Import.mapM createConfirmedEvent allProposedEventInvitations
            let confirmedEventInvitesNoMaybes = catMaybes confirmedEventInvites
            case Import.length confirmedEventInvitesNoMaybes of 
                0 -> invalidArgs ["Failed to find event with id: " Import.++ eventIdText]
                _ -> do 
                    let (ConfirmedEventInvitationData _ eventId creatorId _ name description date fromTime toTime spanMultiple) = confirmedEventInvitesNoMaybes !! 0
                    recipientEmails <- Import.mapM fetchConfirmedRecipientEmail confirmedEventInvitesNoMaybes
                    returnJson $ ConfirmedEventData eventId creatorId (catMaybes recipientEmails) name description date fromTime toTime spanMultiple
        _ -> invalidArgs ["event_id should be an integer"]

putConfirmedEventCreatorR :: Text -> Handler Value 
putConfirmedEventCreatorR eventIdText = do 
    maybeDateText <- lookupPostParam "date"
    maybeTimezone <- lookupPostParam "timezone"
    maybeFromTimeText <- lookupPostParam "from_time"
    maybeToTimeText <- lookupPostParam "to_time"
    let maybeDate = Database.convertTextToDate maybeDateText
    let maybeUTCFromTime = Database.convertTextToTime maybeFromTimeText maybeTimezone
    let maybeUTCToTime = Database.convertTextToTime maybeToTimeText maybeTimezone
    let eitherEventId = decimal eventIdText
    case (maybeDate, maybeUTCFromTime, maybeUTCToTime, eitherEventId) of
        (Just date, Just (fromTimeDayOffset, fromTime), Just (toTimeDayOffset, toTime), Right (eventIdInt, "")) -> do
            allEventEntries <- runDB $ selectList [ConfirmedEventInvitationId ==. toSqlKey (fromIntegral (eventIdInt::Integer))] []
            case allEventEntries of 
                [Entity eventId (ConfirmedEventInvitation _ _ _ _ spanMultiple)] -> do 
                     -- The database will hold in the date of fromTime if the event is staggered 
                    -- between to two days 
                    let utcDate = addDays fromTimeDayOffset date
                    let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
                    runDB $ Import.update eventId 
                        [
                            ConfirmedEventInvitationDate =. utcDate,
                            ConfirmedEventInvitationFromTime =. fromTime, 
                            ConfirmedEventInvitationToTime =. toTime,
                            ConfirmedEventInvitationSpanMultiple =. newSpanMultiple
                        ]
                    return Null
                _ -> notFound
        (_, _, _, _) -> invalidArgs ["Failed to parse date, timezone, from_time and/or to_time params"]

deleteConfirmedEventCreatorR :: Text -> Handler Value 
deleteConfirmedEventCreatorR eventIdText = do 
    let eitherEventId = decimal eventIdText 
    case eitherEventId of 
        Right (eventIdInt, "") -> do 
            proposedEventInvites <- runDB $ 
                selectList [
                    ProposedEventInvitationEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer)),
                    ProposedEventInvitationConfirmed <-. [True]
                ] []
            let inviteIds = Import.map (\(Entity inviteId _) -> inviteId) proposedEventInvites
            runDB $ deleteWhere [ConfirmedEventInvitationProposedEventInvitationId <-. inviteIds]
            return Null
        _ -> badMethod

getConfirmedEventRecipientR :: Text -> Handler Value
getConfirmedEventRecipientR recipientIdText = do 
    maybeTimeZone <- lookupGetParam "timezone"
    maybeRecipientId <- Database.fetchUserId (Just recipientIdText)
    case (maybeRecipientId, maybeTimeZone) of
        (Just recipientId, Just timezone) -> do 
            allProposedEventInvitation <- runDB $ 
                selectList [
                    ProposedEventInvitationRecipientId <-. [recipientId],
                    ProposedEventInvitationConfirmed <-. [True]
                ] []
            confirmedInvites <- Import.mapM getConfirmedEventInvitationFromProposedEventInvitation allProposedEventInvitation
            returnJson $ catMaybes $ Import.map (\x -> convertConfirmedEventInvitationToLocal x timezone) (catMaybes confirmedInvites)
        (_, _) -> invalidArgs ["Failed to parse timezone"]
