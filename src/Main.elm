module Main exposing (main)

import Browser
import Browser.Dom as Dom
import EventCreation.EventCreation as EC
import EventCreation.Update as ECUpdate
import EventCreation.View exposing (viewUserRequest)
import Html exposing (Html, div, text)
import MainMsg exposing (Msg(..))
import Material
import Material.Card as Card
import Material.Options exposing (css, styled)
import Material.Typography as Typography
import Task
import TimeSlots.TimeSlots as TS
import TimeSlots.Update as TSUpdate
import TimeSlots.View exposing (viewDayHeadings, viewScrollableTimeSlots)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = \_ -> init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { timeSlotPositions : List TS.TimeSlotBoundaryPosition
    , timeSlotsElement : Maybe Element
    , timeSlotSelection : TS.TimeSlotSelection
    , eventCreation : EC.EventCreation
    , selectedTimeSlots : List TS.SelectedTimeSlotDetails
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


init : ( Model, Cmd Msg )
init =
    ( { timeSlotPositions = []
      , timeSlotsElement = Nothing
      , timeSlotSelection = TS.NotSelecting
      , eventCreation = EC.NotCreating
      , selectedTimeSlots = []
      , mdc = Material.defaultModel
      }
    , Cmd.batch [ Material.init Mdc, requestTimeSlotPositions defaultNumSlots, requestTimeSlotsElement ]
    )


defaultNumSlots : Int
defaultNumSlots =
    24 * 4



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Mdc msg_ ->
            Material.update Mdc msg_ model

        -- TimeSlots
        SetTimeSlotPositions result ->
            TSUpdate.setTimeSlotPositions model result

        SetTimeSlotsElement result ->
            TSUpdate.setTimeSlotsElement model result

        StartSelectingTimeSlot dayNum slotNum ->
            TSUpdate.startSelectingTimeSlot model dayNum slotNum

        HandleTimeSlotMouseMove pointerPosition ->
            TSUpdate.handleTimeSlotMouseMove model pointerPosition

        AdjustTimeSlotSelection pointerPosition result ->
            TSUpdate.adjustTimeSlotSelection model pointerPosition result

        SetSelectedTimeSlot ->
            TSUpdate.setSelectedTimeSlot model

        HandleTimeSlotMouseUp ->
            TSUpdate.handleTimeSlotMouseUp model

        -- EventCreation
        PromptUserForEventDetails result ->
            ECUpdate.promptUserForEventDetails model result

        AdjustEventTitle title ->
            ECUpdate.adjustEventTitle model title

        AdjustEventDescription description ->
            ECUpdate.adjustEventDescription model description

        CloseUserPromptForEventDetails ->
            ECUpdate.closeUserPromptForEventDetails model


requestTimeSlotPositions : Int -> Cmd Msg
requestTimeSlotPositions numSlots =
    let
        slotNumList =
            List.range 0 (numSlots - 1)

        getTimeSlotPosition slotNum =
            Dom.getElement (getTimeSlotId 1 slotNum)
    in
    Task.attempt SetTimeSlotPositions (Task.sequence (List.map getTimeSlotPosition slotNumList))


requestTimeSlotsElement : Cmd Msg
requestTimeSlotsElement =
    Task.attempt SetTimeSlotsElement (Dom.getElement TS.scrollableTimeSlotsId)



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
