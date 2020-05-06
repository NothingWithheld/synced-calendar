{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.Event where 

import Import
import Database.Persist.Sql (toSqlKey)
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Text.Read
import qualified Database

data ConfirmedEventInvitationData = ConfirmedEventInvitationData ConfirmedEventInvitationId ProposedEventInvitationId UserId UserId (Maybe Text) (Maybe Text) Day TimeOfDay TimeOfDay Bool

instance ToJSON ConfirmedEventInvitationData where 
    toJSON (ConfirmedEventInvitationData confirmedEventId eventId creatorId recipientId name description date (TimeOfDay fromHour fromMinutes _) (TimeOfDay toHour toMinutes _) spanMultiple) =
        object [
            "id" .= confirmedEventId,
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

data ProposedEventInvitationData = ProposedEventInvitationData ProposedEventInvitationId UserId UserId (Maybe Text) (Maybe Text) Day Day Bool

instance ToJSON ProposedEventInvitationData where 
    toJSON (ProposedEventInvitationData eventId creatorId recipientId name description fromDate toDate confirmed) = 
        object [
            "id" .= eventId,
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
    (ConfirmedEventInvitationData confirmedEventId proposedEventId creatorId recipientId name description date fromTime toTime spanMultiple) timezone = do 
        let maybeLocalFromTime = Database.convertUTCToLocal fromTime timezone
        let maybeLocalToTime = Database.convertUTCToLocal toTime timezone
        case (maybeLocalFromTime, maybeLocalToTime) of 
            (Just (fromTimeDayOffset, localFromTime), Just (toTimeDayOffset, localToTime)) -> do 
                -- The database will hold in the day of fromTime if the event is staggered 
                -- between to two days 
                let localDate = addDays fromTimeDayOffset date
                let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
                return $ ConfirmedEventInvitationData confirmedEventId proposedEventId creatorId recipientId name description localDate localFromTime localToTime newSpanMultiple
            (_, _) -> Nothing

getConfirmedEventInvitationFromProposedEventInvitation :: Entity ProposedEventInvitation -> Handler (Maybe ConfirmedEventInvitationData)
getConfirmedEventInvitationFromProposedEventInvitation 
    (Entity proposedEventId (ProposedEventInvitation creatorId recipientId name description _ _ _)) = do 
        allEvents <- runDB $ selectList [ConfirmedEventInvitationProposedEventInvitationId ==. proposedEventId] []
        case allEvents of 
            [Entity confirmedEventId (ConfirmedEventInvitation _ date fromTime toTime spanMultiple)] -> 
                return $ Just $ ConfirmedEventInvitationData confirmedEventId proposedEventId creatorId recipientId name description date fromTime toTime spanMultiple
            _ -> return $ Nothing

createProposedEventInvitation :: (Text, Text, Maybe Text, Maybe Text, Day, Day) -> Handler (Maybe ProposedEventInvitationData)
createProposedEventInvitation (recipientEmail, creatorIdText, maybeName, maybeDescription, fromDate, toDate) = do 
    let confirmed = False
    maybeCreatorId <- Database.fetchUserId (Just creatorIdText)
    maybeRecipientId <- Database.fetchUserIdByEmail (Just recipientEmail)
    case (maybeCreatorId, maybeRecipientId) of 
        (Just creatorId, Just recipientId) -> do 
            let event' = ProposedEventInvitation creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
            (Entity eventId _) <- runDB $ insertEntity event'
            return $ Just $ ProposedEventInvitationData eventId creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
        (Just creatorId, Nothing) -> do 
            -- create a user for the email, but have registred be False
            let newUser' = User recipientEmail Nothing False
            Entity recipientId _ <- runDB $ insertEntity newUser'
            let event' = ProposedEventInvitation creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
            (Entity eventId _) <- runDB $ insertEntity event'
            return $ Just $ ProposedEventInvitationData eventId creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
        (_, _) -> return Nothing

getProposedEventCreatorR :: Text -> Handler Value
getProposedEventCreatorR creatorIdText = do 
    maybeCreatorId <- Database.fetchUserId (Just creatorIdText)
    case maybeCreatorId of
        Just creatorId -> do 
            allProposedEventInvitations <- runDB $ 
                selectList [
                    ProposedEventInvitationCreatorId <-. [creatorId],
                    ProposedEventInvitationConfirmed <-. [False]
                ] []
            returnJson $ Import.map (\(Entity eventId (ProposedEventInvitation _ recipientId name description fromDate toDate confirmed)) ->
                ProposedEventInvitationData eventId creatorId recipientId name description fromDate toDate confirmed) allProposedEventInvitations
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
            let recipientEmails = Database.splitStringByCommas recipientEmailsText
            let arguments = Import.map (\x -> (x, creatorIdText, maybeName, maybeDescription, fromDate, toDate)) recipientEmails
            unwrapped_events <- Import.mapM createProposedEventInvitation arguments
            returnJson $ catMaybes unwrapped_events
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
                    runDB $ update eventId 
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
            runDB $ deleteWhere [ProposedEventInvitationId ==. toSqlKey (fromIntegral (eventIdInt::Integer))]
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
            returnJson $ Import.map (\(Entity eventId (ProposedEventInvitation creatorId _ name description fromDate toDate confirmed)) ->
                ProposedEventInvitationData eventId creatorId recipientId name description fromDate toDate confirmed) allProposedEventInvitations
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
            confirmedEvents <- Import.mapM getConfirmedEventInvitationFromProposedEventInvitation allProposedEventInvitationEntries
            returnJson $ catMaybes $ Import.map (\x -> convertConfirmedEventInvitationToLocal x timezone) (catMaybes confirmedEvents)
        (_, _) -> invalidArgs ["Failed to parse timezone"]

postConfirmedEventCreatorR :: Text -> Handler Value 
postConfirmedEventCreatorR eventIdText = do 
    -- Find the corresponding ProposedEventInvitation object
    let eitherEventId = decimal eventIdText
    case eitherEventId of 
        Right (eventIdInt, "") -> do
            allProposedEventInvitations <- runDB $ 
                selectList [
                    ProposedEventInvitationId ==. toSqlKey (fromIntegral (eventIdInt::Integer)),
                    ProposedEventInvitationConfirmed <-. [False]
                ] []
            case allProposedEventInvitations of 
                [Entity eventId (ProposedEventInvitation creatorId recipientId name description _ _ _)] -> do 
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
                            let event' = ConfirmedEventInvitation eventId utcDate fromTime toTime spanMultiple
                            (Entity confirmedEventId _) <- runDB $ insertEntity event'
                            runDB $ update eventId [ProposedEventInvitationConfirmed =. True]
                            let confirmedEventData = ConfirmedEventInvitationData confirmedEventId eventId creatorId recipientId name description date fromTime toTime spanMultiple
                            let localConfirmedEventInvitationData = convertConfirmedEventInvitationToLocal confirmedEventData timezone
                            case localConfirmedEventInvitationData of
                                Just entity -> returnJson $ entity
                                Nothing -> invalidArgs ["Created in database, failed to convert back to local time"]
                        (_, _, _, _) -> invalidArgs ["Failed to parse date, from_time and/or to_time"]
                _ -> invalidArgs ["Failed to find event with id: " Import.++ eventIdText]
        _ -> invalidArgs ["Failed to find event with id: " Import.++ eventIdText]

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
                    runDB $ update eventId 
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
            runDB $ deleteWhere [ConfirmedEventInvitationId ==. toSqlKey (fromIntegral (eventIdInt::Integer))]
            return Null
        _ -> badMethod

getConfirmedEventRecipientR :: Text -> Handler Value
getConfirmedEventRecipientR recipientIdText = do 
    maybeTimeZone <- lookupGetParam "timezone"
    maybeRecipientId <- Database.fetchUserId (Just recipientIdText)
    case (maybeRecipientId, maybeTimeZone) of
        (Just recipientId, Just timezone) -> do 
            allProposedEventInvitationEntries <- runDB $ 
                selectList [
                    ProposedEventInvitationRecipientId <-. [recipientId],
                    ProposedEventInvitationConfirmed <-. [True]
                ] []
            confirmedEvents <- Import.mapM getConfirmedEventInvitationFromProposedEventInvitation allProposedEventInvitationEntries
            returnJson $ catMaybes $ Import.map (\x -> convertConfirmedEventInvitationToLocal x timezone) (catMaybes confirmedEvents)
        (_, _) -> invalidArgs ["Failed to parse timezone"]
