module TimeSlots.Commands exposing
    ( deleteWeeklyTimeSlot
    , requestConfirmedEventsBy
    , requestConfirmedEventsFor
    , requestCurrentDay
    , requestSavedWeeklyTimeSlots
    , requestTimeSlotPositions
    , requestTimeSlotsElement
    , saveConfirmedEvent
    , saveWeeklyTimeSlot
    , updateWeeklyTimeSlot
    )

import Browser.Dom as Dom
import Date exposing (Date)
import Http
import Session exposing (WithSession)
import Task
import Time exposing (Posix)
import TimeSlots.Messaging as TSMessaging
import TimeSlots.TimeSlots as TS
import Url.Builder as Builder
import Utils exposing (NoData)


requestCurrentDay : (Result Never Posix -> msg) -> Cmd msg
requestCurrentDay setInitialTime =
    Task.attempt setInitialTime Time.now


requestTimeSlotPositions : (Result Dom.Error (List Dom.Element) -> msg) -> Cmd msg
requestTimeSlotPositions setTimeSlotPositions =
    let
        getTimeSlotPosition slotNum =
            Dom.getElement (TS.getTimeSlotId 1 slotNum)
    in
    Task.attempt setTimeSlotPositions (Task.sequence (List.map getTimeSlotPosition TS.slotNumRange))


requestTimeSlotsElement : (Result Dom.Error Dom.Element -> msg) -> Cmd msg
requestTimeSlotsElement setTimeSlotsElement =
    Task.attempt setTimeSlotsElement (Dom.getElement TS.scrollableTimeSlotsId)



-- Weekly Free Times


requestSavedWeeklyTimeSlots :
    (Result Http.Error (List TSMessaging.ServerTimeSlot) -> msg)
    -> String
    -> Int
    -> Cmd msg
requestSavedWeeklyTimeSlots onResult userId timeZoneOffset =
    let
        queryString =
            Builder.toQuery
                [ Builder.int "timezone" timeZoneOffset
                ]
    in
    Http.get
        { url = "http://localhost:3000/api/" ++ userId ++ "/free-times" ++ queryString
        , expect = Http.expectJson onResult TSMessaging.serverTimeSlotListDecoder
        }


saveWeeklyTimeSlot :
    (Result Http.Error Int -> msg)
    -> String
    -> Int
    -> TS.DayNum
    -> TS.SlotNum
    -> TS.SlotNum
    -> Cmd msg
saveWeeklyTimeSlot onSave userId timeZoneOffset dayNum startSlot endSlot =
    case TSMessaging.getFreeTimesQueryString timeZoneOffset dayNum startSlot endSlot of
        Just queryString ->
            Http.post
                { url = "http://localhost:3000/api/" ++ userId ++ "/free-times"
                , body = Http.stringBody "application/x-www-form-urlencoded" queryString
                , expect = Http.expectJson onSave TSMessaging.idDecoder
                }

        Nothing ->
            Cmd.none


updateWeeklyTimeSlot :
    (Result Http.Error NoData -> msg)
    -> Int
    -> Int
    -> TS.DayNum
    -> TS.SlotNum
    -> TS.SlotNum
    -> Cmd msg
updateWeeklyTimeSlot onUpdate timeSlotId timeZoneOffset dayNum startSlot endSlot =
    case TSMessaging.getFreeTimesQueryString timeZoneOffset dayNum startSlot endSlot of
        Just queryString ->
            Http.request
                { method = "PUT"
                , headers = []
                , url = "http://localhost:3000/api/" ++ String.fromInt timeSlotId ++ "/free-times"
                , body = Http.stringBody "application/x-www-form-urlencoded" queryString
                , expect = Http.expectJson onUpdate TSMessaging.noDataDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none


deleteWeeklyTimeSlot : (Result Http.Error NoData -> msg) -> Int -> Cmd msg
deleteWeeklyTimeSlot onDelete timeSlotId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/api/" ++ String.fromInt timeSlotId ++ "/free-times"
        , body = Http.emptyBody
        , expect = Http.expectJson onDelete TSMessaging.noDataDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- Confirmed Events


requestConfirmedEventsBy :
    (Result Http.Error (List TSMessaging.ServerConfirmedEvent) -> msg)
    -> String
    -> Int
    -> Cmd msg
requestConfirmedEventsBy onResult userId timeZoneOffset =
    let
        queryString =
            Builder.toQuery
                [ Builder.int "timezone" timeZoneOffset
                ]
    in
    Http.get
        { url = "http://localhost:3000/api/" ++ userId ++ "/confirmed-event/creator" ++ queryString
        , expect = Http.expectJson onResult TSMessaging.serverConfirmedEventListDecoder
        }


requestConfirmedEventsFor :
    (Result Http.Error (List TSMessaging.ServerConfirmedEvent) -> msg)
    -> String
    -> Int
    -> Cmd msg
requestConfirmedEventsFor onResult userId timeZoneOffset =
    let
        queryString =
            Builder.toQuery
                [ Builder.int "timezone" timeZoneOffset
                ]
    in
    Http.get
        { url = "http://localhost:3000/api/" ++ userId ++ "/confirmed-event/recipient" ++ queryString
        , expect = Http.expectJson onResult TSMessaging.serverConfirmedEventListDecoder
        }


saveConfirmedEvent :
    WithSession a
    -> (Result Http.Error NoData -> msg)
    -> Int
    -> Date
    -> TS.SlotNum
    -> TS.SlotNum
    -> Cmd msg
saveConfirmedEvent model onResult eventId date startSlot endSlot =
    let
        queryString =
            TSMessaging.getConfirmedEventQueryString model date startSlot endSlot
    in
    Http.post
        { url = "http://localhost:3000/api/" ++ String.fromInt eventId ++ "/confirmed-event/creator"
        , body = Http.stringBody "application/x-www-form-urlencoded" queryString
        , expect = Http.expectJson onResult Utils.noDataDecoder
        }
