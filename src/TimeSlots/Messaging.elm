module TimeSlots.Messaging exposing
    ( ServerConfirmedEvent
    , ServerTimeSlot
    , getFreeTimesQueryString
    , getPostFreeTimesJson
    , idDecoder
    , noDataDecoder
    , serverConfirmedEventListDecoder
    , serverTimeSlotDecoder
    , serverTimeSlotListDecoder
    )

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import TimeSlots.Time as TSTime
import TimeSlots.TimeSlots as TS
import Url.Builder as Builder
import Utils exposing (NoData(..), getListItemAt)


type alias ServerTimeSlot =
    { dayNum : TS.DayNum
    , id : Int
    , startSlot : TS.SlotNum
    , endSlot : TS.SlotNum
    }


serverTimeSlotListDecoder : Decoder (List ServerTimeSlot)
serverTimeSlotListDecoder =
    Decode.map (List.filterMap identity) <| Decode.list serverTimeSlotDecoder


serverTimeSlotDecoder : Decoder (Maybe ServerTimeSlot)
serverTimeSlotDecoder =
    Decode.map4 (Maybe.map4 ServerTimeSlot)
        (Decode.map dayToDayNum <| Decode.field "day" Decode.string)
        (Decode.map Just <| Decode.field "id" Decode.int)
        (Decode.map (militaryToSlotNum False) <| Decode.field "fromTime" Decode.string)
        (Decode.map (militaryToSlotNum True) <| Decode.field "toTime" Decode.string)


type alias ServerConfirmedEvent =
    { eventId : Int
    , recipientIds : List String
    , creatorId : String
    , title : String
    , description : String
    , date : Date
    , dayNum : TS.DayNum
    , startSlot : TS.SlotNum
    , endSlot : TS.SlotNum
    }


serverConfirmedEventListDecoder : Decoder (List ServerConfirmedEvent)
serverConfirmedEventListDecoder =
    Decode.map (List.filterMap identity) <| Decode.list serverConfirmedEventDecoder


serverConfirmedEventDecoder : Decoder (Maybe ServerConfirmedEvent)
serverConfirmedEventDecoder =
    Decode.succeed toServerConfirmedEvent
        |> required "eventId" Decode.int
        |> required "recipientId" (Decode.map (String.split ",") Decode.string)
        |> required "creatorId" Decode.string
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "date" (Decode.map TSTime.stringToDate Decode.string)
        |> required "fromTime" (Decode.map (militaryToSlotNum False) Decode.string)
        |> required "toTime" (Decode.map (militaryToSlotNum True) Decode.string)


toServerConfirmedEvent : Int -> List String -> String -> String -> String -> Maybe Date -> Maybe TS.SlotNum -> Maybe TS.SlotNum -> Maybe ServerConfirmedEvent
toServerConfirmedEvent eventId recipientIds creatorId title description date startSlot endSlot =
    Maybe.map4
        (ServerConfirmedEvent eventId recipientIds creatorId title description)
        date
        (Maybe.map TSTime.dateToDayNum date)
        startSlot
        endSlot


idDecoder : Decoder Int
idDecoder =
    Decode.field "id" Decode.int


noDataDecoder : Decoder NoData
noDataDecoder =
    Decode.succeed NoData


getPostFreeTimesJson : TS.DayNum -> TS.SlotNum -> TS.SlotNum -> Maybe Value
getPostFreeTimesJson dayNum startSlotNum endSlotNum =
    let
        mapFunc day =
            Encode.object
                [ ( "day", Encode.string day )
                , ( "from_time", Encode.string <| slotNumToMilitary startSlotNum False )
                , ( "to_time", Encode.string <| slotNumToMilitary endSlotNum True )
                ]
    in
    Maybe.map mapFunc <| dayNumToDay dayNum


getFreeTimesQueryString : Int -> TS.DayNum -> TS.SlotNum -> TS.SlotNum -> Maybe String
getFreeTimesQueryString timeZoneOffset dayNum startSlotNum endSlotNum =
    let
        mapFunc day =
            String.dropLeft 1 <|
                Builder.toQuery
                    [ Builder.string "day" day
                    , Builder.string "from_time" <| slotNumToMilitary startSlotNum False
                    , Builder.string "to_time" <| slotNumToMilitary endSlotNum True
                    , Builder.int "timezone" timeZoneOffset
                    ]
    in
    Maybe.map mapFunc <| dayNumToDay dayNum


dayNames : List String
dayNames =
    [ "sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday" ]


dayToDayNum : String -> Maybe TS.DayNum
dayToDayNum dayString =
    let
        takeIfEquals ( ind, elem ) acc =
            if elem == dayString then
                Just ind

            else
                acc
    in
    List.foldl takeIfEquals Nothing <| List.indexedMap Tuple.pair dayNames


dayNumToDay : TS.DayNum -> Maybe String
dayNumToDay dayNum =
    getListItemAt dayNum dayNames


militaryToSlotNum : Bool -> String -> Maybe TS.SlotNum
militaryToSlotNum isEndSlot militaryTime =
    let
        splitTime =
            String.split ":" militaryTime
    in
    case splitTime of
        [ hourString, minuteString ] ->
            let
                hours =
                    String.toInt hourString

                minutes =
                    String.toInt minuteString
            in
            Maybe.withDefault Nothing <|
                Maybe.map2 (hoursMinutesToSlotNum isEndSlot) hours minutes

        _ ->
            Nothing


hoursMinutesToSlotNum : Bool -> Int -> Int -> Maybe TS.SlotNum
hoursMinutesToSlotNum isEndSlot hours minutes =
    let
        minute15Interval =
            minutes // 15
    in
    if hours >= 0 && hours < 24 && minute15Interval >= 0 && minute15Interval < 4 then
        Just <|
            4
                * hours
                + minute15Interval
                + (if isEndSlot then
                    -1

                   else
                    0
                  )

    else
        Nothing


slotNumToMilitary : TS.SlotNum -> Bool -> String
slotNumToMilitary slotNum isEndSlot =
    let
        adjustedSlotNum =
            if isEndSlot then
                slotNum + 1

            else
                slotNum

        hours =
            adjustedSlotNum // 4

        minutes =
            15 * modBy 4 adjustedSlotNum
    in
    String.fromInt hours ++ ":" ++ String.fromInt minutes
