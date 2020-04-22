module Route exposing (Route(..), fromUrl, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = NotFound
    | Home
    | Login
    | WeeklyFreeTimes


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login <| Parser.s loginString
        , Parser.map WeeklyFreeTimes <| Parser.s weeklyFreeTimesString
        ]


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault NotFound <| Parser.parse parser url


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key <| routeToString route


routeToString : Route -> String
routeToString route =
    "/" ++ String.join "/" (routeToPieces route)


routeToPieces : Route -> List String
routeToPieces route =
    case route of
        NotFound ->
            [ "404" ]

        Home ->
            []

        Login ->
            [ loginString ]

        WeeklyFreeTimes ->
            [ weeklyFreeTimesString ]


loginString : String
loginString =
    "login"


weeklyFreeTimesString : String
weeklyFreeTimesString =
    "weekly-free-times"
