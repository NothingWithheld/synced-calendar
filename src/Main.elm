module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Flip
import Html exposing (Html, div, text)
import Json.Decode as Decode exposing (field, float)
import MainMsg exposing (Msg(..))
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Options as Options exposing (css, styled, when)
import Material.TextField as TextField
import Material.Typography as Typography
import Task
import TimeSlots as TS
import ViewTimeSlots exposing (viewDayHeadings, viewScrollableTimeSlots)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = \_ -> init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { numDays : Int
    , numSlotsInDay : Int
    , timeSlotPositions : List TS.TimeSlotBoundaryPosition
    , timeSlotsElement : Maybe Element
    , timeSlotSelection : TS.TimeSlotSelection
    , userEventCreation : UserEventCreation
    , selectedTimeSlots : List EventSelectedTimeSlot
    , mdc : Material.Model Msg
    }


type alias Element =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias WithName a =
    { a | name : String }


type alias EventSelectedTimeSlot =
    WithName TS.SelectedTimeSlot


type UserEventCreation
    = NotCreating
    | CurrentlyCreatingEvent EventCreationDetails EventCreationPosition


type alias EventCreationDetails =
    { title : String
    , description : String
    }


type alias EventCreationPosition =
    { x : Float
    , y : Float
    }


type alias PointerPosition =
    { pageX : Float
    , pageY : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { numDays = 7
      , numSlotsInDay = defaultNumSlots
      , timeSlotPositions = []
      , timeSlotsElement = Nothing
      , timeSlotSelection = TS.NotSelecting
      , userEventCreation = NotCreating
      , selectedTimeSlots = []
      , mdc = Material.defaultModel
      }
    , Cmd.batch [ Material.init Mdc, requestTimeSlotPositions defaultNumSlots, requestTimeSlotsElement ]
    )


defaultNumSlots : Int
defaultNumSlots =
    24 * 4


eventDetailsPromptWidth : Float
eventDetailsPromptWidth =
    300



-- UPDATE


defaultWithoutData : ( a, b ) -> Maybe c -> (c -> ( a, b )) -> ( a, b )
defaultWithoutData default maybeData mapFunc =
    case maybeData of
        Just data ->
            mapFunc data

        Nothing ->
            default


defaultOnError : ( a, b ) -> Result e c -> (c -> ( a, b )) -> ( a, b )
defaultOnError default result mapFunc =
    case result of
        Ok data ->
            mapFunc data

        Err _ ->
            default


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noUpdateWithoutData =
            defaultWithoutData ( model, Cmd.none )

        noUpdateOnError =
            defaultOnError ( model, Cmd.none )

        useWithoutCmdMsg fn =
            Flip.flip Tuple.pair Cmd.none << fn

        noUpdateIfIntersectsSelectedTS dayNum startBound endBound updatedModel =
            if intersectsCurrentlySelectedTimeSlots model.selectedTimeSlots dayNum startBound endBound then
                model

            else
                updatedModel
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Mdc msg_ ->
            Material.update Mdc msg_ model

        TimeSlotMsg timeSlotMsg ->
            let
                ( newModel, newMsg ) =
                    TS.update timeSlotMsg model
            in
            ( newModel, Cmd.map TimeSlotMsg newMsg )

        InitiateUserPromptForEventDetails ->
            case ( model.userEventCreation, model.timeSlotSelection ) of
                ( NotCreating, TS.CurrentlySelecting { dayNum, startBound, endBound } ) ->
                    let
                        minSlotNum =
                            min startBound.slotNum endBound.slotNum
                    in
                    ( model
                    , Task.attempt
                        PromptUserForEventDetails
                        (Dom.getElement (getTimeSlotId dayNum minSlotNum))
                    )

                ( _, _ ) ->
                    ( model, Cmd.none )

        PromptUserForEventDetails result ->
            case result of
                Ok { viewport, element } ->
                    let
                        leftSpace =
                            element.x - viewport.x

                        promptMargin =
                            12

                        promptSpace =
                            eventDetailsPromptWidth + 2 * promptMargin

                        x =
                            if leftSpace >= promptSpace then
                                element.x - eventDetailsPromptWidth - promptMargin

                            else
                                element.x + element.width + promptMargin
                    in
                    ( { model
                        | userEventCreation =
                            CurrentlyCreatingEvent { title = "", description = "" } { x = x, y = element.y }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        AdjustEventTitle title ->
            case model.userEventCreation of
                CurrentlyCreatingEvent eventDetails eventPosition ->
                    ( { model
                        | userEventCreation =
                            CurrentlyCreatingEvent
                                { eventDetails
                                    | title = title
                                }
                                eventPosition
                      }
                    , Cmd.none
                    )

                NotCreating ->
                    ( model, Cmd.none )

        AdjustEventDescription description ->
            case model.userEventCreation of
                CurrentlyCreatingEvent eventDetails eventPosition ->
                    ( { model
                        | userEventCreation =
                            CurrentlyCreatingEvent
                                { eventDetails
                                    | description = description
                                }
                                eventPosition
                      }
                    , Cmd.none
                    )

                NotCreating ->
                    ( model, Cmd.none )

        CloseUserPromptForEventDetails ->
            ( { model | timeSlotSelection = TS.NotSelecting, userEventCreation = NotCreating }, Cmd.none )

        SetSelectedTimeSlot ->
            case ( model.userEventCreation, model.timeSlotSelection ) of
                ( CurrentlyCreatingEvent { title } _, TS.CurrentlySelecting { dayNum, startBound, endBound } ) ->
                    let
                        ( startSlot, endSlot ) =
                            if startBound.slotNum <= endBound.slotNum then
                                ( startBound, endBound )

                            else
                                ( endBound, startBound )

                        selectedTimeSlot =
                            { dayNum = dayNum
                            , name = title
                            , startBound = startSlot
                            , endBound = endSlot
                            }
                    in
                    ( { model
                        | selectedTimeSlots = selectedTimeSlot :: model.selectedTimeSlots
                        , timeSlotSelection = TS.NotSelecting
                        , userEventCreation = NotCreating
                      }
                    , Cmd.none
                    )

                ( _, _ ) ->
                    ( model, Cmd.none )


onTimeSlotMouseMove : Options.Property c Msg
onTimeSlotMouseMove =
    Options.on "mousemove"
        (Decode.map (TimeSlotMsg << TS.HandleTimeSlotMouseMove)
            (Decode.map2
                PointerPosition
                (field "pageX" float)
                (field "pageY" float)
            )
        )


requestTimeSlotPositions : Int -> Cmd Msg
requestTimeSlotPositions numSlots =
    let
        slotNumList =
            List.range 0 (numSlots - 1)

        getTimeSlotPosition slotNum =
            Dom.getElement (getTimeSlotId 1 slotNum)
    in
    Task.attempt (TimeSlotMsg << TS.SetTimeSlotPositions) (Task.sequence (List.map getTimeSlotPosition slotNumList))


requestTimeSlotsElement : Cmd Msg
requestTimeSlotsElement =
    Task.attempt (TimeSlotMsg << TS.SetTimeSlotsElement) (Dom.getElement TS.scrollableTimeSlotsId)


intersectsCurrentlySelectedTimeSlots : List (TS.WithSelectedTimeSlot a) -> TS.DayNum -> Int -> Int -> Bool
intersectsCurrentlySelectedTimeSlots currentTimeSlots dayNum startSlotNum endSlotNum =
    let
        ( lowerSlotNum, higherSlotNum ) =
            if startSlotNum < endSlotNum then
                ( startSlotNum, endSlotNum )

            else
                ( endSlotNum, startSlotNum )

        selectedTimeSlotsForThisDay =
            List.filter (\timeSlot -> timeSlot.dayNum == dayNum) currentTimeSlots

        isTimeSlotTaken timeSlot =
            List.any
                (\selected ->
                    timeSlot >= selected.startBound.slotNum && timeSlot <= selected.endBound.slotNum
                )
                selectedTimeSlotsForThisDay
    in
    List.any isTimeSlotTaken (List.range lowerSlotNum higherSlotNum)



-- VIEW


view : Model -> Html Msg
view model =
    styled div
        [ css "position" "relative"
        , css "display" "flex"
        , css "flex-direction" "column-reverse"
        , css "height" "100vh"
        ]
        [ styled div
            []
            [ viewDayHeadings
            , viewScrollableTimeSlots model
            ]
        , viewUserRequest model
        ]


viewSelectedTimeSlot : EventSelectedTimeSlot -> Html Msg
viewSelectedTimeSlot selectedTimeSlot =
    let
        cardDimensions =
            getCardDimensions selectedTimeSlot.startBound selectedTimeSlot.endBound
    in
    Card.view
        [ css "background-color" "red"
        , css "top" (String.fromFloat cardDimensions.y ++ "px")
        , css "height" (String.fromFloat (cardDimensions.height - 4) ++ "px")
        , css "position" "absolute"
        , css "width" "95%"
        , css "z-index" "4"
        , css "border-radius" "8px"
        ]
        [ styled div [ Typography.subheading1 ] [ text selectedTimeSlot.name ] ]


viewUserRequest : Model -> Html Msg
viewUserRequest model =
    case model.userEventCreation of
        NotCreating ->
            text ""

        CurrentlyCreatingEvent eventCreationDetails eventCreationPosition ->
            styled div
                [ css "position" "absolute"
                , css "top" "0"
                , css "width" "100%"
                , css "height" "100%"
                , css "z-index" "100"
                , Options.onClick CloseUserPromptForEventDetails
                ]
                [ viewUserRequestForm model eventCreationDetails eventCreationPosition ]


viewUserRequestForm : Model -> EventCreationDetails -> EventCreationPosition -> Html Msg
viewUserRequestForm model eventCreationDetails eventCreationPosition =
    let
        positionFromTop =
            Basics.max eventCreationPosition.y 50
    in
    Card.view
        [ css "position" "absolute"
        , css "width" (String.fromFloat eventDetailsPromptWidth ++ "px")
        , css "left" (String.fromFloat eventCreationPosition.x ++ "px")
        , css "top" (String.fromFloat positionFromTop ++ "px")
        , css "padding" "12px 8px 0px"
        , css "box-shadow" "0 24px 38px 3px rgba(0,0,0,0.14), 0 9px 46px 8px rgba(0,0,0,0.12), 0 11px 15px -7px rgba(0,0,0,0.2)"
        , Options.onWithOptions "click"
            (Decode.succeed
                { message = NoOp
                , preventDefault = False
                , stopPropagation = True
                }
            )
        ]
        [ TextField.view Mdc
            "event-title"
            model.mdc
            [ TextField.label "Title"
            , TextField.value eventCreationDetails.title
            , Options.onInput AdjustEventTitle
            ]
            []
        , TextField.view Mdc
            "event-description"
            model.mdc
            [ TextField.label "Description"
            , TextField.value eventCreationDetails.description
            , Options.onInput AdjustEventDescription
            ]
            []
        , Card.actions [ css "display" "flex", css "flex-direction" "row-reverse" ]
            [ Card.actionButtons []
                [ Button.view Mdc
                    "close-event-button"
                    model.mdc
                    [ Card.actionButton
                    , Button.ripple
                    , Options.onClick CloseUserPromptForEventDetails
                    , css "margin-right" "8px"
                    ]
                    [ text "Cancel" ]
                , Button.view Mdc
                    "set-event-button"
                    model.mdc
                    [ Card.actionButton
                    , Button.ripple
                    , Button.unelevated
                    , Options.onClick SetSelectedTimeSlot
                    ]
                    [ text "Submit" ]
                ]
            ]
        ]


getCardDimensions : TS.TimeSlotBoundaryPosition -> TS.TimeSlotBoundaryPosition -> CardDimensions
getCardDimensions boundA boundB =
    let
        ( higherBound, lowerBound ) =
            if boundA.y < boundB.y then
                ( boundA, boundB )

            else
                ( boundB, boundA )

        totalHeight =
            lowerBound.y + lowerBound.height - higherBound.y
    in
    { y = higherBound.y, height = totalHeight }


type alias CardDimensions =
    { y : Float
    , height : Float
    }


getTimeSlotId : Int -> Int -> String
getTimeSlotId dayNum slotNum =
    "time-slot-" ++ String.fromInt dayNum ++ "--" ++ String.fromInt slotNum



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
