module ProposeEvent.Messaging exposing
    ( ServerProposedEvent
    , getEventProposalQueryString
    , noDataDecoder
    , serverProposedEventListDecoder
    )

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import TimeSlots.Time as TSTime
import Url.Builder as Builder
import Utils exposing (NoData(..))


noDataDecoder : Decoder NoData
noDataDecoder =
    Decode.succeed NoData


getEventProposalQueryString :
    String
    -> Maybe String
    -> Maybe String
    -> String
    -> String
    -> Maybe String
getEventProposalQueryString recipientId fromDate toDate name description =
    let
        mapFunc fromDateString toDateString =
            String.dropLeft 1 <|
                Builder.toQuery
                    [ Builder.string "recipient_id" recipientId
                    , Builder.string "from_date" <| slashesToDashesDate fromDateString
                    , Builder.string "to_date" <| slashesToDashesDate toDateString
                    , Builder.string "name" name
                    , Builder.string "description" description
                    ]
    in
    Maybe.map2 mapFunc fromDate toDate


slashesToDashesDate : String -> String
slashesToDashesDate date =
    String.replace "/" "-" date


type alias ServerProposedEvent =
    { title : String
    , description : String
    , creatorId : String
    , eventId : Int
    , recipientIds : List String
    , fromDate : Date
    , toDate : Date
    }


serverProposedEventListDecoder : Decoder (List ServerProposedEvent)
serverProposedEventListDecoder =
    Decode.map (List.filterMap identity) <| Decode.list serverProposedEventDecoder


serverProposedEventDecoder : Decoder (Maybe ServerProposedEvent)
serverProposedEventDecoder =
    Decode.succeed toServerProposedEvent
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "creatorId" Decode.string
        |> required "id" Decode.int
        |> required "recipientId" (Decode.map (String.split ",") Decode.string)
        |> required "fromDate" (Decode.map TSTime.stringToDate Decode.string)
        |> required "toDate" (Decode.map TSTime.stringToDate Decode.string)


toServerProposedEvent :
    String
    -> String
    -> String
    -> Int
    -> List String
    -> Maybe Date
    -> Maybe Date
    -> Maybe ServerProposedEvent
toServerProposedEvent title description creatorId eventId recipientIds fromDate toDate =
    Maybe.map2
        (ServerProposedEvent title description creatorId eventId recipientIds)
        fromDate
        toDate
