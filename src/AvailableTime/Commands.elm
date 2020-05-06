module AvailableTime.Commands exposing (submitAvailableTimes)

import AvailableTime.AvailableTime exposing (AvailableTimeDetails)
import AvailableTime.Messaging as ATMessaging
import Http
import Session exposing (WithSession)
import Utils exposing (NoData)


submitAvailableTimes :
    WithSession a
    -> (Result Http.Error NoData -> msg)
    -> String
    -> Int
    -> List AvailableTimeDetails
    -> Cmd msg
submitAvailableTimes model onSubmit userId eventId availableTimes =
    let
        queryString =
            ATMessaging.getMultipleAvailableTimesQueryString model eventId availableTimes
    in
    Http.post
        { url = "http://localhost:3000/api/" ++ userId ++ "/available-times/multiple"
        , body = Http.stringBody "application/x-www-form-urlencoded" queryString
        , expect = Http.expectJson onSubmit Utils.noDataDecoder
        }
