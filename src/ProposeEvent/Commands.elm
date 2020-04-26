module ProposeEvent.Commands exposing (submitEventProposal)

import Http
import ProposeEvent.Messaging as PEMessaging
import Utils exposing (NoData)
import WeeklyFreeTimes.MainMsg exposing (Msg(..))


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
