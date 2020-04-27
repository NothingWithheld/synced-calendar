module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Error
import Html
import Json.Decode as Decode
import Pages.Home
import Pages.Login
import Pages.ProposeEvent
import Pages.WeeklyFreeTimes
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)



-- MAIN


main : Program Decode.Value Model Msg
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
    = NotFound Session
    | Redirect Session
    | Login Pages.Login.Model
    | Home Pages.Home.Model
    | WeeklyFreeTimes Pages.WeeklyFreeTimes.Model
    | ProposeEvent Pages.ProposeEvent.Model


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init offsetFlag url key =
    handleUrlChange (Route.fromUrl url) <|
        Redirect <|
            Session.init key offsetFlag


getSession : Model -> Session
getSession model =
    case model of
        NotFound session ->
            session

        Redirect session ->
            session

        Login login ->
            login.session

        Home home ->
            home.session

        WeeklyFreeTimes weeklyFreeTimes ->
            weeklyFreeTimes.session

        ProposeEvent proposeEvent ->
            proposeEvent.session



-- UPDATE


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest
    | LoginMsg Pages.Login.Msg
    | HomeMsg Pages.Home.Msg
    | WeeklyFreeTimesMsg Pages.WeeklyFreeTimes.Msg
    | ProposeEventMsg Pages.ProposeEvent.Msg


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
                Pages.Login.update subMsg login

        ( HomeMsg subMsg, Home home ) ->
            updateWith Home HomeMsg <|
                Pages.Home.update subMsg home

        ( WeeklyFreeTimesMsg subMsg, WeeklyFreeTimes weeklyFreeTimes ) ->
            updateWith WeeklyFreeTimes WeeklyFreeTimesMsg <|
                Pages.WeeklyFreeTimes.update subMsg weeklyFreeTimes

        ( ProposeEventMsg subMsg, ProposeEvent proposeEvent ) ->
            updateWith ProposeEvent ProposeEventMsg <|
                Pages.ProposeEvent.update subMsg proposeEvent

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

        key =
            Session.getKey session

        hasUserId =
            Session.hasUserId session
    in
    case ( route, hasUserId ) of
        ( Route.NotFound, _ ) ->
            ( NotFound session, Cmd.none )

        ( Route.Login, False ) ->
            updateWith Login LoginMsg <|
                Pages.Login.init session

        ( _, False ) ->
            ( model, Route.replaceUrl key Route.Login )

        ( Route.Home, True ) ->
            updateWith Home HomeMsg <|
                Pages.Home.init session

        ( Route.Login, True ) ->
            updateWith Login LoginMsg <|
                Pages.Login.init session

        ( Route.WeeklyFreeTimes, True ) ->
            updateWith WeeklyFreeTimes WeeklyFreeTimesMsg <|
                Pages.WeeklyFreeTimes.init session

        ( Route.ProposeEvent, True ) ->
            updateWith ProposeEvent ProposeEventMsg <|
                Pages.ProposeEvent.init session

        ( Route.Logout, True ) ->
            ( Redirect <| Session.signOut session, Route.replaceUrl key Route.Login )



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        NotFound _ ->
            Error.view404

        Redirect _ ->
            { title = "", body = [] }

        Login login ->
            viewWith LoginMsg <| Pages.Login.view login

        Home home ->
            viewWith HomeMsg <| Pages.Home.view home

        WeeklyFreeTimes weeklyFreeTimes ->
            viewWith WeeklyFreeTimesMsg <|
                Pages.WeeklyFreeTimes.view weeklyFreeTimes

        ProposeEvent proposeEvent ->
            viewWith ProposeEventMsg <|
                Pages.ProposeEvent.view proposeEvent


viewWith : (subMsg -> Msg) -> Document subMsg -> Document Msg
viewWith toMsg { title, body } =
    { title = title
    , body = List.map (Html.map toMsg) body
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Sub.none

        Login login ->
            Sub.map LoginMsg <|
                Pages.Login.subscriptions login

        Home home ->
            Sub.map HomeMsg <|
                Pages.Home.subscriptions home

        WeeklyFreeTimes weeklyFreeTimes ->
            Sub.map WeeklyFreeTimesMsg <|
                Pages.WeeklyFreeTimes.subscriptions weeklyFreeTimes

        ProposeEvent proposeEvent ->
            Sub.map ProposeEventMsg <|
                Pages.ProposeEvent.subscriptions proposeEvent
