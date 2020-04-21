module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Login.Main
import Session
import Url exposing (Url)
import WeeklyFreeTimes.Main
import WeeklyFreeTimes.MainMsg



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



-- MODEL


type Model
    = Login Login.Main.Model
    | WeeklyFreeTimes WeeklyFreeTimes.Main.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    updateWith Login LoginMsg <| Login.Main.init <| Session.init key



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink UrlRequest
    | LoginMsg Login.Main.Msg
    | WeeklyFreeTimesMsg WeeklyFreeTimes.MainMsg.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl _, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink _, _ ) ->
            ( model, Cmd.none )

        ( LoginMsg subMsg, Login login ) ->
            updateWith Login LoginMsg <|
                Login.Main.update subMsg login

        ( WeeklyFreeTimesMsg subMsg, WeeklyFreeTimes weeklyFreeTimes ) ->
            updateWith WeeklyFreeTimes WeeklyFreeTimesMsg <|
                WeeklyFreeTimes.Main.update subMsg weeklyFreeTimes

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        Login login ->
            viewWith LoginMsg <| Login.Main.view login

        WeeklyFreeTimes weeklyFreeTimes ->
            viewWith WeeklyFreeTimesMsg <|
                WeeklyFreeTimes.Main.view weeklyFreeTimes


viewWith : (subMsg -> Msg) -> Document subMsg -> Document Msg
viewWith toMsg { title, body } =
    { title = title
    , body = List.map (Html.map toMsg) body
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Login login ->
            Sub.map LoginMsg <|
                Login.Main.subscriptions login

        WeeklyFreeTimes weeklyFreeTimes ->
            Sub.map WeeklyFreeTimesMsg <|
                WeeklyFreeTimes.Main.subscriptions weeklyFreeTimes
