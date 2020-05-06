module AvailableTime.Commands exposing (requestAvailableTimesForUser, saveAvailableTimes)

import AvailableTime.AvailableTime exposing (AvailableTimeDetails)
import AvailableTime.Messaging as ATMessaging
import Http
import Session exposing (WithSession)
import Url.Builder as Builder
import Utils exposing (NoData)


saveAvailableTimes :
    WithSession a
    -> (Result Http.Error NoData -> msg)
    -> Int
    -> List AvailableTimeDetails
    -> Cmd msg
saveAvailableTimes model onSubmit eventId availableTimes =
    let
        userId =
            Session.getUserId model.session

        queryString =
            ATMessaging.getMultipleAvailableTimesQueryString model eventId availableTimes
    in
    Http.post
        { url = "http://localhost:3000/api/" ++ userId ++ "/available-times/multiple"
        , body = Http.stringBody "application/x-www-form-urlencoded" queryString
        , expect = Http.expectJson onSubmit Utils.noDataDecoder
        }


requestAvailableTimesForUser : WithSession a -> (Result Http.Error (List AvailableTimeDetails) -> msg) -> Int -> Cmd msg
requestAvailableTimesForUser model onResult eventId =
    let
        userId =
            Session.getUserId model.session

        timezoneOffset =
            Session.getOffset model.session

        queryString =
            Builder.toQuery
                [ Builder.int "timezone" timezoneOffset
                , Builder.int "event_id" eventId
                ]
    in
    Http.get
        { url = "http://localhost:3000/api/" ++ userId ++ "/available-times" ++ queryString
        , expect = Http.expectJson onResult ATMessaging.availableTimeDetailsListDecoder
        }
