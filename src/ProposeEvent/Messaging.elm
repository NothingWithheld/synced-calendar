module ProposeEvent.Messaging exposing
    ( getEventProposalQueryString
    , noDataDecoder
    )

import Json.Decode as Decode exposing (Decoder)
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
