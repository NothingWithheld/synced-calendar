module Route exposing (Route(..), fromUrl, replaceUrl, routeToString, viewHomeButton)

import Browser.Navigation as Nav
import Html exposing (Html, text)
import Material
import Material.Button as Button
import ProposeEvent.Messaging as PEMessaging
import ProposeEvent.ProposeEvent exposing (ProposedEvent)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((<?>), Parser)
import Utils exposing (WithMdc)


type Route
    = NotFound
    | Home
    | Login
    | WeeklyFreeTimes
    | EventCalendar
    | SubmitAvailability ProposedEvent
    | ProposeEvent
    | Logout


viewHomeButton : WithMdc msg a -> (Material.Msg msg -> msg) -> Html msg
viewHomeButton model onMdc =
    Button.view onMdc
        "back-button"
        model.mdc
        [ Button.ripple
        , Button.icon "arrow_back"
        , Button.link <| routeToString Home
        ]
        [ text "Home" ]


parser : Parser (Maybe Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map (Just Home) Parser.top
        , Parser.map (Just Login) <| Parser.s loginString
        , Parser.map (Just WeeklyFreeTimes) <| Parser.s weeklyFreeTimesString
        , Parser.map (Just EventCalendar) <| Parser.s eventCalendarString
        , Parser.map (Maybe.map SubmitAvailability) <|
            Parser.s submitAvailiabilityString
                <?> PEMessaging.proposedEventQueryDecoder
        , Parser.map (Just ProposeEvent) <| Parser.s proposeEventString
        , Parser.map (Just Logout) <| Parser.s logoutString
        ]


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault NotFound <|
        Maybe.withDefault Nothing <|
            Parser.parse parser url


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key <| routeToString route


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            Builder.absolute [ "404" ] []

        Home ->
            Builder.absolute [] []

        Login ->
            Builder.absolute [ loginString ] []

        WeeklyFreeTimes ->
            Builder.absolute [ weeklyFreeTimesString ] []

        EventCalendar ->
            Builder.absolute [ eventCalendarString ] []

        SubmitAvailability proposedEvent ->
            Builder.absolute [ submitAvailiabilityString ] <|
                PEMessaging.proposedEventToQueryParams proposedEvent

        ProposeEvent ->
            Builder.absolute [ proposeEventString ] []

        Logout ->
            Builder.absolute [ logoutString ] []


loginString : String
loginString =
    "login"


weeklyFreeTimesString : String
weeklyFreeTimesString =
    "weekly-free-times"


eventCalendarString : String
eventCalendarString =
    "event-calendar"


submitAvailiabilityString : String
submitAvailiabilityString =
    "submit-availability"


proposeEventString : String
proposeEventString =
    "propose-event"


logoutString : String
logoutString =
    "logout"
