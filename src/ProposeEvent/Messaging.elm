module ProposeEvent.Messaging exposing
    ( getEventProposalQueryString
    , noDataDecoder
    , proposedEventListDecoder
    , proposedEventQueryDecoder
    , proposedEventToQueryParams
    )

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import ProposeEvent.ProposeEvent exposing (ProposedEvent)
import TimeSlots.Time as TSTime
import Url.Builder as Builder exposing (QueryParameter)
import Url.Parser.Query as Query
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


proposedEventListDecoder : Decoder (List ProposedEvent)
proposedEventListDecoder =
    Decode.map (List.filterMap identity) <| Decode.list proposedEventDecoder


proposedEventDecoder : Decoder (Maybe ProposedEvent)
proposedEventDecoder =
    Decode.succeed toProposedEvent
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "creatorId" Decode.string
        |> required "id" Decode.int
        |> required "recipientId" (Decode.map (String.split ",") Decode.string)
        |> required "fromDate" (Decode.map TSTime.isoStringToDate Decode.string)
        |> required "toDate" (Decode.map TSTime.isoStringToDate Decode.string)


toProposedEvent :
    String
    -> String
    -> String
    -> Int
    -> List String
    -> Maybe Date
    -> Maybe Date
    -> Maybe ProposedEvent
toProposedEvent title description creatorId eventId recipientIds fromDate toDate =
    Maybe.map2
        (ProposedEvent title description creatorId eventId recipientIds)
        fromDate
        toDate


proposedEventToQueryParams : ProposedEvent -> List QueryParameter
proposedEventToQueryParams proposedEvent =
    [ Builder.string "title" proposedEvent.title
    , Builder.string "description" proposedEvent.description
    , Builder.string "creatorId" proposedEvent.creatorId
    , Builder.int "eventId" proposedEvent.eventId
    , Builder.string "recipientIds" <| String.join "," proposedEvent.recipientIds
    , Builder.string "fromDate" <| TSTime.dateToString proposedEvent.fromDate
    , Builder.string "toDate" <| TSTime.dateToString proposedEvent.toDate
    ]


proposedEventQueryDecoder : Query.Parser (Maybe ProposedEvent)
proposedEventQueryDecoder =
    Query.map7 (Utils.maybeMap7 ProposedEvent)
        (Query.string "title")
        (Query.string "description")
        (Query.string "creatorId")
        (Query.int "eventId")
        (Query.map (Maybe.map (String.split ",")) (Query.string "recipientIds"))
        (Query.map (Maybe.andThen TSTime.stringToDate) (Query.string "fromDate"))
        (Query.map (Maybe.andThen TSTime.stringToDate) (Query.string "toDate"))
