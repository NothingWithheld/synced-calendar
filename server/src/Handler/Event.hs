{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Handler.Event where 

import Import 
import Data.Dates
import Data.Text.Read
import qualified Data.Text as T

postEventR :: Text -> Handler Value 
postEventR userId = do 
    maybeRecipient <- lookupPostParam "recipient_id"
    maybeName <- lookupPostParam "name"
    maybeDescription <- lookupPostParam "description"
    maybeFromDateText <- lookupPostParam "from_date"
    maybeToDateText <- lookupPostParam "to_date"
    let confirmed = False
    let maybeFromDate = convertTextToDate maybeFromDateText
    let maybeToDate = convertTextToDate maybeToDateText
    case (maybeRecipient, maybeFromDate, maybeToDate) of
        (Just recipient, Just fromDate, Just toDate) -> do
            let event' = Event userId recipient maybeName maybeDescription fromDate toDate confirmed
            insertedEvent <- runDB $ insertEntity event'
            returnJson insertedEvent
        (_, _, _) -> invalidArgs ["Failed to parse arguments. Check API documentation for valid formatting"]

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
