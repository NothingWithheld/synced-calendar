module Pages.Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Date
import Html exposing (Html, div, text)
import Html.Entity as Entity
import Http
import Material
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options exposing (css, styled, when)
import Material.Typography as Typography
import ProposeEvent.Commands exposing (requestProposedEventsBy, requestProposedEventsFor)
import ProposeEvent.Messaging exposing (ProposedEvent)
import Route
import Session exposing (Session)
import Utils exposing (limitToNChars)



-- MODEL


type alias Model =
    { session : Session
    , mdc : Material.Model Msg
    , receivedProposedEvents : List ProposedEvent
    , createdProposedEvents : List ProposedEvent
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , mdc = Material.defaultModel
      , receivedProposedEvents = []
      , createdProposedEvents = []
      }
    , Cmd.batch
        [ Material.init Mdc
        , requestProposedEventsBy SetCreatedProposedEvents (Session.getUserId session)
        , requestProposedEventsFor SetReceivedProposedEvents (Session.getUserId session)
        ]
    )



-- UPDATE


type Msg
    = Mdc (Material.Msg Msg)
    | SetReceivedProposedEvents (Result Http.Error (List ProposedEvent))
    | SetCreatedProposedEvents (Result Http.Error (List ProposedEvent))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        SetReceivedProposedEvents result ->
            case result of
                Ok receivedProposedEvents ->
                    ( { model | receivedProposedEvents = receivedProposedEvents }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SetCreatedProposedEvents result ->
            case result of
                Ok createdProposedEvents ->
                    ( { model | createdProposedEvents = createdProposedEvents }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Synced Calendar - Home"
    , body =
        [ styled div
            [ css "display" "flex"
            , css "justify-content" "center"
            , css "align-items" "center"
            , css "min-height" "600px"
            , css "height" "100%"
            ]
            [ styled div
                [ css "display" "flex", css "justify-content" "center" ]
                [ viewContainerCard Nothing (viewMainActionLineItems model)
                , if List.length model.receivedProposedEvents > 0 then
                    viewContainerCard (Just 350) (viewReceivedProposedEventsLineItems model)

                  else
                    text ""
                ]
            ]
        ]
    }


viewMainActionLineItems : Model -> List (Html Msg)
viewMainActionLineItems model =
    [ viewLineEntry model
        False
        "Edit when you're free each week"
        "weekly-free-times-button"
      <|
        Route.routeToString Route.WeeklyFreeTimes
    , styled Html.hr
        [ css "margin" "0" ]
        []
    , viewLineEntry
        model
        False
        "Create and invite people to an event"
        "create-event-button"
      <|
        Route.routeToString Route.ProposeEvent
    , styled Html.hr
        [ css "margin" "0" ]
        []
    , viewLineEntry
        model
        False
        "View events you're scheduled for"
        "event-calendar-button"
      <|
        Route.routeToString Route.EventCalendar
    , styled Html.hr
        [ css "margin" "0" ]
        []
    , viewLogoutEntry model
    ]


viewReceivedProposedEventsLineItems : Model -> List (Html Msg)
viewReceivedProposedEventsLineItems model =
    [ styled Html.h3
        [ Typography.body1
        , css "margin-left" "16px"
        ]
        [ text "Add your availability to these events" ]
    , styled Html.hr
        [ css "margin" "0" ]
        []
    ]
        ++ List.map (viewProposedEventLineItem model) model.receivedProposedEvents


viewProposedEventLineItem : Model -> ProposedEvent -> Html Msg
viewProposedEventLineItem model ({ title, fromDate, toDate, eventId } as proposedEvent) =
    let
        lineText =
            limitToNChars 13 title
                ++ " "
                ++ Entity.mdash
                ++ " "
                ++ Date.format "MMM d" fromDate
                ++ " "
                ++ Entity.ndash
                ++ " "
                ++ Date.format "MMM d" toDate

        buttonLabel =
            "Submit availability for event " ++ String.fromInt eventId
    in
    viewLineEntry model True lineText buttonLabel <|
        Route.routeToString (Route.SubmitAvailability proposedEvent)


viewContainerCard : Maybe Int -> List (Html Msg) -> Html Msg
viewContainerCard pixelWidth children =
    styled div
        [ css "width"
            (Maybe.withDefault "450px" <|
                Maybe.map ((\w -> w ++ "px") << String.fromInt) pixelWidth
            )
        , css "height" "max-content"
        , Elevation.z8
        , css "border-radius" "8px"
        , css "margin" "0 8px"
        ]
        children


viewLineEntry : Model -> Bool -> String -> String -> String -> Html Msg
viewLineEntry model isCondensed description buttonLabel href =
    styled div
        [ css "padding"
            (if isCondensed then
                "4px 0 4px 16px"

             else
                "4px 16px"
            )
        , css "display" "flex"
        , css "justify-content" "space-between"
        , css "align-items" "center"
        ]
        [ styled Html.p
            [ Typography.body1
            , when isCondensed <| css "margin" "0"
            ]
            [ text description ]
        , Button.view Mdc
            buttonLabel
            model.mdc
            [ Button.ripple
            , Button.link href
            , Button.icon "arrow_forward"
            ]
            [ if isCondensed then
                text ""

              else
                text "Ok"
            ]
        ]


viewLogoutEntry : Model -> Html Msg
viewLogoutEntry model =
    styled div
        [ css "padding" "4px 16px"
        , css "display" "flex"
        , css "justify-content" "space-between"
        , css "align-items" "center"
        ]
        [ styled Html.p
            [ Typography.body1
            ]
            [ text "Log Out" ]
        , Button.view Mdc
            "log-out-button"
            model.mdc
            [ Button.ripple
            , Button.link <| Route.routeToString Route.Logout
            , Button.icon "arrow_back"
            ]
            [ text "Ok" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
