module TimeSlots.Commands exposing
    ( deleteWeeklyTimeSlot
    , requestSavedWeeklyTimeSlots
    , requestTimeSlotPositions
    , requestTimeSlotsElement
    , saveWeeklyTimeSlot
    , updateWeeklyTimeSlot
    )

import Browser.Dom as Dom
import Http
import Task
import TimeSlots.Messaging as TSMessaging
import TimeSlots.TimeSlots as TS
import WeeklyFreeTimes.MainMsg exposing (Msg(..))


requestTimeSlotPositions : Cmd Msg
requestTimeSlotPositions =
    let
        getTimeSlotPosition slotNum =
            Dom.getElement (TS.getTimeSlotId 1 slotNum)
    in
    Task.attempt SetTimeSlotPositions (Task.sequence (List.map getTimeSlotPosition TS.slotNumRange))


requestTimeSlotsElement : Cmd Msg
requestTimeSlotsElement =
    Task.attempt SetTimeSlotsElement (Dom.getElement TS.scrollableTimeSlotsId)


requestSavedWeeklyTimeSlots : String -> Cmd Msg
requestSavedWeeklyTimeSlots userId =
    Http.get
        { url = "http://localhost:3000/api/" ++ userId ++ "/free-times?timezone=-6"
        , expect = Http.expectJson SetSavedWeeklyTimeSlots TSMessaging.serverTimeSlotListDecoder
        }


saveWeeklyTimeSlot : String -> TS.DayNum -> TS.SlotNum -> TS.SlotNum -> Cmd Msg
saveWeeklyTimeSlot userId dayNum startSlot endSlot =
    case TSMessaging.getFreeTimesQueryString dayNum startSlot endSlot of
        Just queryString ->
            Http.post
                { url = "http://localhost:3000/api/" ++ userId ++ "/free-times"
                , body = Http.stringBody "application/x-www-form-urlencoded" queryString
                , expect = Http.expectJson SetSelectedTimeSlotAfterCreation TSMessaging.idDecoder
                }

        Nothing ->
            Cmd.none


updateWeeklyTimeSlot : Int -> TS.DayNum -> TS.SlotNum -> TS.SlotNum -> Cmd Msg
updateWeeklyTimeSlot timeSlotId dayNum startSlot endSlot =
    case TSMessaging.getFreeTimesQueryString dayNum startSlot endSlot of
        Just queryString ->
            Http.request
                { method = "PUT"
                , headers = []
                , url = "http://localhost:3000/api/" ++ String.fromInt timeSlotId ++ "/free-times"
                , body = Http.stringBody "application/x-www-form-urlencoded" queryString
                , expect = Http.expectJson SetSelectedTimeSlotAfterEditing TSMessaging.noDataDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none


deleteWeeklyTimeSlot : Int -> Cmd Msg
deleteWeeklyTimeSlot timeSlotId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/api/" ++ String.fromInt timeSlotId ++ "/free-times"
        , body = Http.emptyBody
        , expect = Http.expectJson DeleteTimeSlot TSMessaging.noDataDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
