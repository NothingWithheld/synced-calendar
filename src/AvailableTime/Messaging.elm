module AvailableTime.Messaging exposing (getMultipleAvailableTimesQueryString)

import AvailableTime.AvailableTime exposing (AvailableTimeDetails)
import Session exposing (WithSession)
import TimeSlots.Time as TSTime
import Url.Builder as Builder


type alias ServerAvailableTime =
    { date : String
    , fromTime : String
    , toTime : String
    }


getMultipleAvailableTimesQueryString : WithSession a -> Int -> List AvailableTimeDetails -> String
getMultipleAvailableTimesQueryString model eventId availableTimes =
    let
        timezoneOffset =
            Session.getOffset model.session

        serverAvailableTimes =
            List.map
                (\{ date, startSlot, endSlot } ->
                    ServerAvailableTime
                        (TSTime.dateToString date)
                        (TSTime.slotNumToMilitary startSlot False)
                        (TSTime.slotNumToMilitary endSlot True)
                )
                availableTimes
    in
    String.dropLeft 1 <|
        Builder.toQuery
            [ Builder.int "event_id" eventId
            , Builder.int "timezone" timezoneOffset
            , Builder.string "dates" <|
                String.join "," <|
                    List.map .date serverAvailableTimes
            , Builder.string "from_times" <|
                String.join
                    ","
                <|
                    List.map .fromTime serverAvailableTimes
            , Builder.string "to_times" <|
                String.join "," <|
                    List.map .toTime serverAvailableTimes
            ]
