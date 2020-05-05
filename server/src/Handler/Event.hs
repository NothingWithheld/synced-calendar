{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.Event where 

import Import
import Database.Persist.Sql (toSqlKey)
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Text.Read
import qualified Database

data ConfirmedEventData = ConfirmedEventData ConfirmedEventId ProposedEventId UserId UserId (Maybe Text) (Maybe Text) Day TimeOfDay TimeOfDay Bool

instance ToJSON ConfirmedEventData where 
    toJSON (ConfirmedEventData confirmedEventId eventId creatorId recipientId name description date (TimeOfDay fromHour fromMinutes _) (TimeOfDay toHour toMinutes _) spanMultiple) =
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

data ProposedEventData = ProposedEventData ProposedEventId UserId UserId (Maybe Text) (Maybe Text) Day Day Bool

instance ToJSON ProposedEventData where 
    toJSON (ProposedEventData eventId creatorId recipientId name description fromDate toDate confirmed) = 
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

convertConfirmedEventToLocal :: ConfirmedEventData -> Text -> Maybe ConfirmedEventData
convertConfirmedEventToLocal 
    (ConfirmedEventData confirmedEventId proposedEventId creatorId recipientId name description date fromTime toTime spanMultiple) timezone = do 
        let maybeLocalFromTime = Database.convertUTCToLocal fromTime timezone
        let maybeLocalToTime = Database.convertUTCToLocal toTime timezone
        case (maybeLocalFromTime, maybeLocalToTime) of 
            (Just (fromTimeDayOffset, localFromTime), Just (toTimeDayOffset, localToTime)) -> do 
                -- The database will hold in the day of fromTime if the event is staggered 
                -- between to two days 
                let localDate = addDays fromTimeDayOffset date
                let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
                return $ ConfirmedEventData confirmedEventId proposedEventId creatorId recipientId name description localDate localFromTime localToTime newSpanMultiple
            (_, _) -> Nothing

getConfirmedEventFromProposedEvent :: Entity ProposedEvent -> Handler (Maybe ConfirmedEventData)
getConfirmedEventFromProposedEvent 
    (Entity proposedEventId (ProposedEvent creatorId recipientId name description _ _ _)) = do 
        allEvents <- runDB $ selectList [ConfirmedEventProposedEventId ==. proposedEventId] []
        case allEvents of 
            [Entity confirmedEventId (ConfirmedEvent _ date fromTime toTime spanMultiple)] -> 
                return $ Just $ ConfirmedEventData confirmedEventId proposedEventId creatorId recipientId name description date fromTime toTime spanMultiple
            _ -> return $ Nothing

createProposedEvent :: (Text, Text, Maybe Text, Maybe Text, Day, Day) -> Handler (Maybe ProposedEventData)
createProposedEvent (recipientEmail, creatorIdText, maybeName, maybeDescription, fromDate, toDate) = do 
    let confirmed = False
    maybeCreatorId <- Database.fetchUserId (Just creatorIdText)
    maybeRecipientId <- Database.fetchUserIdByEmail (Just recipientEmail)
    case (maybeCreatorId, maybeRecipientId) of 
        (Just creatorId, Just recipientId) -> do 
            let event' = ProposedEvent creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
            (Entity eventId _) <- runDB $ insertEntity event'
            return $ Just $ ProposedEventData eventId creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
        (Just creatorId, Nothing) -> do 
            -- create a user for the email, but have registred be False
            let newUser' = User recipientEmail Nothing False
            Entity recipientId _ <- runDB $ insertEntity newUser'
            let event' = ProposedEvent creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
            (Entity eventId _) <- runDB $ insertEntity event'
            return $ Just $ ProposedEventData eventId creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
        (_, _) -> return Nothing

getProposedEventCreatorR :: Text -> Handler Value
getProposedEventCreatorR creatorIdText = do 
    maybeCreatorId <- Database.fetchUserId (Just creatorIdText)
    case maybeCreatorId of
        Just creatorId -> do 
            allProposedEvents <- runDB $ 
                selectList [
                    ProposedEventCreatorId <-. [creatorId],
                    ProposedEventConfirmed <-. [False]
                ] []
            returnJson $ Import.map (\(Entity eventId (ProposedEvent _ recipientId name description fromDate toDate confirmed)) ->
                ProposedEventData eventId creatorId recipientId name description fromDate toDate confirmed) allProposedEvents
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
            unwrapped_events <- Import.mapM createProposedEvent arguments
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
            allEvents <- runDB $ selectList [ProposedEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer))] []
            case allEvents of 
                [Entity eventId ProposedEvent {..}] -> do 
                    runDB $ update eventId 
                        [
                            ProposedEventRecipientId =. recipientId,
                            ProposedEventName =. maybeNameText,
                            ProposedEventDescription =. maybeDescriptionText,
                            ProposedEventFromDate =. fromDate,
                            ProposedEventToDate =. toDate,
                            ProposedEventConfirmed =. confirmed
                        ]
                    return Null
                _ -> invalidArgs ["Failed to find event with id: " Import.++ eventIdText]
        (_, _, _, _) -> invalidArgs ["Failed to parse arguments. Check API documentation for valid formatting"]

deleteProposedEventCreatorR :: Text -> Handler Value 
deleteProposedEventCreatorR eventIdText = do 
    let eitherEventId = decimal eventIdText 
    case eitherEventId of 
        Right (eventIdInt, "") -> do 
            runDB $ deleteWhere [ProposedEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer))]
            return Null
        _ -> badMethod

getProposedEventRecipientR :: Text -> Handler Value 
getProposedEventRecipientR recipientIdText = do
    maybeRecipientId <- Database.fetchUserId (Just recipientIdText)
    case maybeRecipientId of 
        Just recipientId -> do
            allProposedEvents <- runDB $ 
                selectList [
                    ProposedEventRecipientId <-. [recipientId],
                    ProposedEventConfirmed <-. [False]
                ] []
            returnJson $ Import.map (\(Entity eventId (ProposedEvent creatorId _ name description fromDate toDate confirmed)) ->
                ProposedEventData eventId creatorId recipientId name description fromDate toDate confirmed) allProposedEvents
        _ -> invalidArgs ["Failed to find user with id: " Import.++ recipientIdText]

getConfirmedEventCreatorR :: Text -> Handler Value
getConfirmedEventCreatorR creatorIdText = do 
    maybeTimeZone <- lookupGetParam "timezone"
    maybeCreatorId <- Database.fetchUserId (Just creatorIdText)
    case (maybeCreatorId, maybeTimeZone) of
        (Just creatorId, Just timezone) -> do 
            allProposedEventEntries <- runDB $ 
                selectList [
                    ProposedEventCreatorId <-. [creatorId],
                    ProposedEventConfirmed <-. [True]
                ] []
            confirmedEvents <- Import.mapM getConfirmedEventFromProposedEvent allProposedEventEntries
            returnJson $ catMaybes $ Import.map (\x -> convertConfirmedEventToLocal x timezone) (catMaybes confirmedEvents)
        (_, _) -> invalidArgs ["Failed to parse timezone"]

postConfirmedEventCreatorR :: Text -> Handler Value 
postConfirmedEventCreatorR eventIdText = do 
    -- Find the corresponding ProposedEvent object
    let eitherEventId = decimal eventIdText
    case eitherEventId of 
        Right (eventIdInt, "") -> do
            allProposedEvents <- runDB $ 
                selectList [
                    ProposedEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer)),
                    ProposedEventConfirmed <-. [False]
                ] []
            case allProposedEvents of 
                [Entity eventId (ProposedEvent creatorId recipientId name description _ _ _)] -> do 
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
                            let event' = ConfirmedEvent eventId utcDate fromTime toTime spanMultiple
                            (Entity confirmedEventId _) <- runDB $ insertEntity event'
                            runDB $ update eventId [ProposedEventConfirmed =. True]
                            let confirmedEventData = ConfirmedEventData confirmedEventId eventId creatorId recipientId name description date fromTime toTime spanMultiple
                            let localConfirmedEventData = convertConfirmedEventToLocal confirmedEventData timezone
                            case localConfirmedEventData of
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
            allEventEntries <- runDB $ selectList [ConfirmedEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer))] []
            case allEventEntries of 
                [Entity eventId (ConfirmedEvent _ _ _ _ spanMultiple)] -> do 
                     -- The database will hold in the date of fromTime if the event is staggered 
                    -- between to two days 
                    let utcDate = addDays fromTimeDayOffset date
                    let newSpanMultiple = if fromTimeDayOffset == toTimeDayOffset then spanMultiple else not spanMultiple
                    runDB $ update eventId 
                        [
                            ConfirmedEventDate =. utcDate,
                            ConfirmedEventFromTime =. fromTime, 
                            ConfirmedEventToTime =. toTime,
                            ConfirmedEventSpanMultiple =. newSpanMultiple
                        ]
                    return Null
                _ -> notFound
        (_, _, _, _) -> invalidArgs ["Failed to parse date, timezone, from_time and/or to_time params"]

deleteConfirmedEventCreatorR :: Text -> Handler Value 
deleteConfirmedEventCreatorR eventIdText = do 
    let eitherEventId = decimal eventIdText 
    case eitherEventId of 
        Right (eventIdInt, "") -> do 
            runDB $ deleteWhere [ConfirmedEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer))]
            return Null
        _ -> badMethod

getConfirmedEventRecipientR :: Text -> Handler Value
getConfirmedEventRecipientR recipientIdText = do 
    maybeTimeZone <- lookupGetParam "timezone"
    maybeRecipientId <- Database.fetchUserId (Just recipientIdText)
    case (maybeRecipientId, maybeTimeZone) of
        (Just recipientId, Just timezone) -> do 
            allProposedEventEntries <- runDB $ 
                selectList [
                    ProposedEventRecipientId <-. [recipientId],
                    ProposedEventConfirmed <-. [True]
                ] []
            confirmedEvents <- Import.mapM getConfirmedEventFromProposedEvent allProposedEventEntries
            returnJson $ catMaybes $ Import.map (\x -> convertConfirmedEventToLocal x timezone) (catMaybes confirmedEvents)
        (_, _) -> invalidArgs ["Failed to parse timezone"]
