module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Error
import Html
import Json.Decode as Decode
import Pages.CreateEvent
import Pages.EventCalendar
import Pages.Home
import Pages.Login
import Pages.ProposeEvent
import Pages.SubmitAvailability
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
    | EventCalendar Pages.EventCalendar.Model
    | ProposeEvent Pages.ProposeEvent.Model
    | SubmitAvailability Pages.SubmitAvailability.Model
    | CreateEvent Pages.CreateEvent.Model


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

        Login model_ ->
            model_.session

        Home model_ ->
            model_.session

        WeeklyFreeTimes model_ ->
            model_.session

        EventCalendar model_ ->
            model_.session

        ProposeEvent model_ ->
            model_.session

        SubmitAvailability model_ ->
            model_.session

        CreateEvent model_ ->
            model_.session



-- UPDATE


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest
    | LoginMsg Pages.Login.Msg
    | HomeMsg Pages.Home.Msg
    | WeeklyFreeTimesMsg Pages.WeeklyFreeTimes.Msg
    | EventCalendarMsg Pages.EventCalendar.Msg
    | ProposeEventMsg Pages.ProposeEvent.Msg
    | SubmitAvailabilityMsg Pages.SubmitAvailability.Msg
    | CreateEventMsg Pages.CreateEvent.Msg


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

        ( LoginMsg subMsg, Login model_ ) ->
            updateWith Login LoginMsg <|
                Pages.Login.update subMsg model_

        ( HomeMsg subMsg, Home model_ ) ->
            updateWith Home HomeMsg <|
                Pages.Home.update subMsg model_

        ( WeeklyFreeTimesMsg subMsg, WeeklyFreeTimes model_ ) ->
            updateWith WeeklyFreeTimes WeeklyFreeTimesMsg <|
                Pages.WeeklyFreeTimes.update subMsg model_

        ( EventCalendarMsg subMsg, EventCalendar model_ ) ->
            updateWith EventCalendar EventCalendarMsg <|
                Pages.EventCalendar.update subMsg model_

        ( ProposeEventMsg subMsg, ProposeEvent model_ ) ->
            updateWith ProposeEvent ProposeEventMsg <|
                Pages.ProposeEvent.update subMsg model_

        ( SubmitAvailabilityMsg subMsg, SubmitAvailability model_ ) ->
            updateWith SubmitAvailability SubmitAvailabilityMsg <|
                Pages.SubmitAvailability.update subMsg model_

        ( CreateEventMsg subMsg, CreateEvent model_ ) ->
            updateWith CreateEvent CreateEventMsg <|
                Pages.CreateEvent.update subMsg model_

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

        ( Route.EventCalendar, True ) ->
            updateWith EventCalendar EventCalendarMsg <|
                Pages.EventCalendar.init session

        ( Route.ProposeEvent, True ) ->
            updateWith ProposeEvent ProposeEventMsg <|
                Pages.ProposeEvent.init session

        ( Route.SubmitAvailability proposedEvent, True ) ->
            updateWith SubmitAvailability SubmitAvailabilityMsg <|
                Pages.SubmitAvailability.init session proposedEvent

        ( Route.CreateEvent proposedEvent, True ) ->
            updateWith CreateEvent CreateEventMsg <|
                Pages.CreateEvent.init session proposedEvent

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

        Login model_ ->
            viewWith LoginMsg <| Pages.Login.view model_

        Home model_ ->
            viewWith HomeMsg <| Pages.Home.view model_

        WeeklyFreeTimes model_ ->
            viewWith WeeklyFreeTimesMsg <|
                Pages.WeeklyFreeTimes.view model_

        EventCalendar model_ ->
            viewWith EventCalendarMsg <|
                Pages.EventCalendar.view model_

        ProposeEvent model_ ->
            viewWith ProposeEventMsg <|
                Pages.ProposeEvent.view model_

        SubmitAvailability model_ ->
            viewWith SubmitAvailabilityMsg <|
                Pages.SubmitAvailability.view model_

        CreateEvent model_ ->
            viewWith CreateEventMsg <|
                Pages.CreateEvent.view model_


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

        Login model_ ->
            Sub.map LoginMsg <|
                Pages.Login.subscriptions model_

        Home model_ ->
            Sub.map HomeMsg <|
                Pages.Home.subscriptions model_

        WeeklyFreeTimes model_ ->
            Sub.map WeeklyFreeTimesMsg <|
                Pages.WeeklyFreeTimes.subscriptions model_

        EventCalendar model_ ->
            Sub.map EventCalendarMsg <|
                Pages.EventCalendar.subscriptions model_

        ProposeEvent model_ ->
            Sub.map ProposeEventMsg <|
                Pages.ProposeEvent.subscriptions model_

        SubmitAvailability model_ ->
            Sub.map SubmitAvailabilityMsg <|
                Pages.SubmitAvailability.subscriptions model_

        CreateEvent model_ ->
            Sub.map CreateEventMsg <|
                Pages.CreateEvent.subscriptions model_
