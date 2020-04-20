module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Html
import Url exposing (Url)
import WeeklyFreeTimes.Main as WeeklyFreeTimes
import WeeklyFreeTimes.MainMsg as WeeklyFreeTimesMsg



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = \_ _ _ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



-- MODEL


type Model
    = WeeklyFreeTimes WeeklyFreeTimes.Model


init : ( Model, Cmd Msg )
init =
    updateWith WeeklyFreeTimes WeeklyFreeTimesMsg <| WeeklyFreeTimes.init



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink UrlRequest
    | WeeklyFreeTimesMsg WeeklyFreeTimesMsg.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl _, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink _, _ ) ->
            ( model, Cmd.none )

        ( WeeklyFreeTimesMsg subMsg, WeeklyFreeTimes weeklyFreeTimes ) ->
            updateWith WeeklyFreeTimes WeeklyFreeTimesMsg <|
                WeeklyFreeTimes.update subMsg weeklyFreeTimes


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        WeeklyFreeTimes weeklyFreeTimes ->
            viewWith WeeklyFreeTimesMsg <|
                WeeklyFreeTimes.view weeklyFreeTimes


viewWith : (subMsg -> Msg) -> Document subMsg -> Document Msg
viewWith toMsg { title, body } =
    { title = title
    , body = List.map (Html.map toMsg) body
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        WeeklyFreeTimes weeklyFreeTimes ->
            Sub.map WeeklyFreeTimesMsg <|
                WeeklyFreeTimes.subscriptions weeklyFreeTimes
