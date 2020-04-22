module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Login.Main
import Route exposing (Route)
import Session exposing (Session)
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
        , onUrlRequest = ClickLink
        , onUrlChange = ChangeUrl
        }



-- MODEL


type Model
    = Login Login.Main.Model
    | WeeklyFreeTimes WeeklyFreeTimes.Main.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    handleUrlChange (Route.fromUrl url) <|
        Tuple.first <|
            updateWith Login LoginMsg <|
                Login.Main.init <|
                    Session.init key


getSession : Model -> Session
getSession model =
    case model of
        Login login ->
            login.session

        WeeklyFreeTimes weeklyFreeTimes ->
            weeklyFreeTimes.session



-- UPDATE


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest
    | LoginMsg Login.Main.Msg
    | WeeklyFreeTimesMsg WeeklyFreeTimes.MainMsg.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            getSession model
    in
    case ( msg, model ) of
        ( ChangeUrl url, _ ) ->
            handleUrlChange (Route.fromUrl url) model

        ( ClickLink urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl (Session.getKey session) <| Url.toString url )

                External href ->
                    ( model, Nav.load href )

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


handleUrlChange : Route -> Model -> ( Model, Cmd Msg )
handleUrlChange route model =
    let
        session =
            getSession model

        hasUserId =
            Session.hasUserId session
    in
    case ( route, hasUserId ) of
        ( Route.NotFound, _ ) ->
            ( model, Cmd.none )

        ( Route.Login, False ) ->
            ( model, Cmd.none )

        ( _, False ) ->
            ( model, Route.replaceUrl (Session.getKey session) Route.Login )

        ( Route.Home, True ) ->
            ( model, Cmd.none )

        ( Route.Login, True ) ->
            ( model, Cmd.none )

        ( Route.WeeklyFreeTimes, True ) ->
            ( model, Cmd.none )



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
