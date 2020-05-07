module TimeSlots.Messaging exposing
    ( ServerConfirmedEvent
    , ServerTimeSlot
    , getConfirmedEventQueryString
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
import Session exposing (WithSession)
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
        (Decode.map (TSTime.militaryToSlotNum False) <| Decode.field "fromTime" Decode.string)
        (Decode.map (TSTime.militaryToSlotNum True) <| Decode.field "toTime" Decode.string)


getConfirmedEventQueryString : WithSession a -> Date -> TS.SlotNum -> TS.SlotNum -> String
getConfirmedEventQueryString model date startSlot endSlot =
    String.dropLeft 1 <|
        Builder.toQuery
            [ Builder.string "date" <| TSTime.dateToString date
            , Builder.string "from_time" <| TSTime.slotNumToMilitary startSlot False
            , Builder.string "to_time" <| TSTime.slotNumToMilitary endSlot True
            , Builder.int "timezone" <| Session.getOffset model.session
            ]


type alias ServerConfirmedEvent =
    { eventId : Int
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
        |> required "creatorId" Decode.string
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "date" (Decode.map TSTime.isoStringToDate Decode.string)
        |> required "fromTime" (Decode.map (TSTime.militaryToSlotNum False) Decode.string)
        |> required "toTime" (Decode.map (TSTime.militaryToSlotNum True) Decode.string)


toServerConfirmedEvent :
    Int
    -> String
    -> String
    -> String
    -> Maybe Date
    -> Maybe TS.SlotNum
    -> Maybe TS.SlotNum
    -> Maybe ServerConfirmedEvent
toServerConfirmedEvent eventId creatorId title description date startSlot endSlot =
    Maybe.map4
        (ServerConfirmedEvent eventId creatorId title description)
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
                , ( "from_time", Encode.string <| TSTime.slotNumToMilitary startSlotNum False )
                , ( "to_time", Encode.string <| TSTime.slotNumToMilitary endSlotNum True )
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
                    , Builder.string "from_time" <| TSTime.slotNumToMilitary startSlotNum False
                    , Builder.string "to_time" <| TSTime.slotNumToMilitary endSlotNum True
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
