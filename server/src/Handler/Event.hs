{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.Event where 

import Import
import Database.Persist.Sql (toSqlKey)
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Text.Read
import qualified Database

data ConfirmedEventData = ConfirmedEventData ProposedEventId Text Text (Maybe Text) (Maybe Text) Day TimeOfDay TimeOfDay

instance ToJSON ConfirmedEventData where 
    toJSON (ConfirmedEventData eventId creatorId recipientId name description date fromTime toTime) =
        object [
            "eventId" .= eventId, 
            "creatorId" .= creatorId,
            "recipientId" .= recipientId,
            "name" .= name,
            "description" .= description,
            "date" .= date,
            "fromTime" .= fromTime,
            "toTime" .= toTime
        ]

convertConfirmedEventToLocal :: ConfirmedEventData -> Text -> Maybe (ConfirmedEventData)
convertConfirmedEventToLocal 
    (ConfirmedEventData proposedEventId creatorId recipientId name description date fromTime toTime) timezone = do 
        let maybeLocalFromTime = Database.convertUTCToLocal fromTime timezone
        let maybeLocalToTime = Database.convertUTCToLocal toTime timezone
        case (maybeLocalFromTime, maybeLocalToTime) of 
            (Just (fromTimeDayOffset, localFromTime), Just (_, localToTime)) -> do 
                -- The database will hold in the day of fromTime if the event is staggered 
                -- between to two days 
                let localDate = addDays fromTimeDayOffset date
                return $ ConfirmedEventData proposedEventId creatorId recipientId name description localDate localFromTime localToTime
            (_, _) -> Nothing

getConfirmedEventFromProposedEvent :: Entity ProposedEvent -> Handler (Maybe ConfirmedEventData)
getConfirmedEventFromProposedEvent 
    (Entity proposedEventId (ProposedEvent creatorId recipientId name description _ _ _)) = do 
        allEvents <- runDB $ selectList [ConfirmedEventProposedEventId ==. proposedEventId] []
        case allEvents of 
            [Entity _ (ConfirmedEvent _ date fromTime toTime)] -> 
                return $ Just $ ConfirmedEventData proposedEventId creatorId recipientId name description date fromTime toTime
            _ -> return $ Nothing

getProposedEventCreatorR :: Text -> Handler Value
getProposedEventCreatorR creatorId = do 
    allEventEntries <- runDB $ 
        selectList [
            ProposedEventCreatorId <-. [creatorId],
            ProposedEventConfirmed <-. [False]
        ] []
    returnJson allEventEntries

postProposedEventCreatorR :: Text -> Handler Value 
postProposedEventCreatorR creatorId = do 
    maybeRecipientId <- lookupPostParam "recipient_id"
    maybeName <- lookupPostParam "name"
    maybeDescription <- lookupPostParam "description"
    maybeFromDateText <- lookupPostParam "from_date"
    maybeToDateText <- lookupPostParam "to_date"
    let confirmed = False
    let maybeFromDate = Database.convertTextToDate maybeFromDateText
    let maybeToDate = Database.convertTextToDate maybeToDateText
    case (maybeRecipientId, maybeFromDate, maybeToDate) of
        (Just recipientId, Just fromDate, Just toDate) -> do
            let event' = ProposedEvent creatorId recipientId maybeName maybeDescription fromDate toDate confirmed
            insertedEvent <- runDB $ insertEntity event'
            returnJson insertedEvent
        (_, _, _) -> invalidArgs ["Failed to parse arguments. Check API documentation for valid formatting"]

putProposedEventCreatorR :: Text -> Handler Value 
putProposedEventCreatorR eventIdText = do 
    maybeRecipientId <- lookupPostParam "recipient_id"
    maybeNameText <- lookupPostParam "name"
    maybeDescriptionText <- lookupPostParam "description"
    maybeFromDateText <- lookupPostParam "from_date"
    maybeToDateText <- lookupPostParam "to_date"
    maybeConfirmedText <- lookupPostParam "confirmed"
    let maybeFromDate = Database.convertTextToDate maybeFromDateText
    let maybeToDate = Database.convertTextToDate maybeToDateText
    let maybeConfirmed = Database.convertTextToBool maybeConfirmedText
    let eitherEventId = decimal eventIdText
    case (maybeRecipientId, maybeFromDate, maybeToDate, maybeConfirmed, eitherEventId) of
        (Just recipientId, Just fromDate, Just toDate, Just confirmed, Right (eventIdInt, "")) -> do
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
        (_, _, _, _, _) -> invalidArgs ["Failed to parse arguments. Check API documentation for valid formatting"]

deleteProposedEventCreatorR :: Text -> Handler Value 
deleteProposedEventCreatorR eventIdText = do 
    let eitherEventId = decimal eventIdText 
    case eitherEventId of 
        Right (eventIdInt, "") -> do 
            runDB $ deleteWhere [ProposedEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer))]
            return Null
        _ -> badMethod

getProposedEventRecipientR :: Text -> Handler Value 
getProposedEventRecipientR recipientId = do 
    allProposedEvents <- runDB $ 
        selectList [
            ProposedEventRecipientId <-. [recipientId],
            ProposedEventConfirmed <-. [False]
        ] []
    returnJson allProposedEvents

getConfirmedEventCreatorR :: Text -> Handler Value
getConfirmedEventCreatorR creatorId = do 
    maybeTimeZone <- lookupGetParam "timezone"
    case maybeTimeZone of
        Just timezone -> do 
            allProposedEventEntries <- runDB $ 
                selectList [
                    ProposedEventCreatorId <-. [creatorId],
                    ProposedEventConfirmed <-. [True]
                ] []
            confirmedEvents <- Import.mapM getConfirmedEventFromProposedEvent allProposedEventEntries
            returnJson $ catMaybes $ Import.map (\x -> convertConfirmedEventToLocal x timezone) (catMaybes confirmedEvents)
        _ -> invalidArgs ["Failed to parse timezone"]

postConfirmedEventCreatorR :: Text -> Handler Value 
postConfirmedEventCreatorR eventIdText = do 
    -- Find the corresponding ProposedEvent object
    let eitherEventId = decimal eventIdText
    case eitherEventId of 
        Right (eventIdInt, "") -> do
            allProposedEvents <- runDB $ selectList [ProposedEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer))] []
            case allProposedEvents of 
                [Entity eventId (ProposedEvent creatorId recipientId name description _ _ _)] -> do 
                    maybeDateText <- lookupPostParam "date"
                    maybeTimezone <- lookupPostParam "timezone"
                    maybeFromTimeText <- lookupPostParam "from_time"
                    maybeToTimeText <- lookupPostParam "to_time"
                    let maybeDate = Database.convertTextToDate maybeDateText
                    let maybeUTCFromTime = Database.convertTextToTime maybeFromTimeText maybeTimezone
                    let maybeUTCToTime = Database.convertTextToTime maybeToTimeText maybeTimezone
                    case (maybeDate, maybeUTCFromTime, maybeUTCToTime) of 
                        (Just date, Just (fromTimeDayOffset, fromTime), Just (_, toTime)) -> do 
                            -- The database will hold in the date of fromTime if the event is staggered 
                            -- between to two days
                            let utcDate = addDays fromTimeDayOffset date
                            let event' = ConfirmedEvent eventId utcDate fromTime toTime
                            _ <- runDB $ insertEntity event'
                            runDB $ update eventId [ProposedEventConfirmed =. True]
                            returnJson $ ConfirmedEventData eventId creatorId recipientId name description date fromTime toTime
                        (_, _, _) -> invalidArgs ["Failed to parse date, from_time and/or to_time"]
                _ -> invalidArgs ["Failed to find event with id: " Import.++ eventIdText]
        _ -> invalidArgs ["Failed to find event with id: " Import.++ eventIdText]

deleteConfirmedEventCreatorR :: Text -> Handler Value 
deleteConfirmedEventCreatorR eventIdText = do 
    let eitherEventId = decimal eventIdText 
    case eitherEventId of 
        Right (eventIdInt, "") -> do 
            runDB $ deleteWhere [ConfirmedEventId ==. toSqlKey (fromIntegral (eventIdInt::Integer))]
            return Null
        _ -> badMethod

getConfirmedEventRecipientR :: Text -> Handler Value
getConfirmedEventRecipientR recipientId = do 
    maybeTimeZone <- lookupGetParam "timezone"
    case maybeTimeZone of
        Just timezone -> do 
            allProposedEventEntries <- runDB $ 
                selectList [
                    ProposedEventCreatorId <-. [recipientId],
                    ProposedEventConfirmed <-. [True]
                ] []
            confirmedEvents <- Import.mapM getConfirmedEventFromProposedEvent allProposedEventEntries
            returnJson $ catMaybes $ Import.map (\x -> convertConfirmedEventToLocal x timezone) (catMaybes confirmedEvents)
        _ -> invalidArgs ["Failed to parse timezone"]
