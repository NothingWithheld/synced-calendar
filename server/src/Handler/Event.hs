{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.Event where 

import Import 
import qualified Database

getProposedEventR :: Text -> Handler Value 
getProposedEventR recipientId = do 
    allProposedEvents <- runDB $ selectList [ProposedEventRecipientId <-. [recipientId]] []
    returnJson allProposedEvents

postProposedEventR :: Text -> Handler Value 
postProposedEventR userId = do 
    maybeRecipient <- lookupPostParam "recipient_id"
    maybeName <- lookupPostParam "name"
    maybeDescription <- lookupPostParam "description"
    maybeFromDateText <- lookupPostParam "from_date"
    maybeToDateText <- lookupPostParam "to_date"
    let confirmed = False
    let maybeFromDate = Database.convertTextToDate maybeFromDateText
    let maybeToDate = Database.convertTextToDate maybeToDateText
    case (maybeRecipient, maybeFromDate, maybeToDate) of
        (Just recipient, Just fromDate, Just toDate) -> do
            let event' = ProposedEvent userId recipient maybeName maybeDescription fromDate toDate confirmed
            insertedEvent <- runDB $ insertEntity event'
            returnJson insertedEvent
        (_, _, _) -> invalidArgs ["Failed to parse arguments. Check API documentation for valid formatting"]
