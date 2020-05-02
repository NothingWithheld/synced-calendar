module ProposeEvent.Commands exposing
    ( requestProposedEventsBy
    , requestProposedEventsFor
    , submitEventProposal
    )

import Http
import ProposeEvent.Messaging as PEMessaging
import ProposeEvent.ProposeEvent exposing (ProposedEvent)
import Utils exposing (NoData)


submitEventProposal :
    (Result Http.Error NoData -> msg)
    -> String
    -> String
    -> Maybe String
    -> Maybe String
    -> String
    -> String
    -> Cmd msg
submitEventProposal onSubmitResult userId recipientId fromDate toDate name description =
    case
        PEMessaging.getEventProposalQueryString recipientId
            fromDate
            toDate
            name
            description
    of
        Just queryString ->
            Http.post
                { url = "http://localhost:3000/api/" ++ userId ++ "/proposed-event/creator"
                , body = Http.stringBody "application/x-www-form-urlencoded" queryString
                , expect = Http.expectJson onSubmitResult PEMessaging.noDataDecoder
                }

        Nothing ->
            Cmd.none


requestProposedEventsBy :
    (Result Http.Error (List ProposedEvent) -> msg)
    -> String
    -> Cmd msg
requestProposedEventsBy onResult userId =
    Http.get
        { url = "http://localhost:3000/api/" ++ userId ++ "/proposed-event/creator"
        , expect = Http.expectJson onResult PEMessaging.proposedEventListDecoder
        }


requestProposedEventsFor :
    (Result Http.Error (List ProposedEvent) -> msg)
    -> String
    -> Cmd msg
requestProposedEventsFor onResult userId =
    Http.get
        { url = "http://localhost:3000/api/" ++ userId ++ "/proposed-event/recipient"
        , expect = Http.expectJson onResult PEMessaging.proposedEventListDecoder
        }
