module Main exposing (main)

import Browser
import EventCreation.EventCreation as EC
import EventCreation.Update as ECUpdate
import EventCreation.View exposing (viewUserRequest)
import Html exposing (Html, div)
import MainMsg exposing (Msg(..))
import Material
import Material.Options exposing (css, styled)
import TimeSlots.Commands exposing (requestTimeSlotPositions, requestTimeSlotsElement)
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
    , timeSlotsElement : Maybe TS.Element
    , timeSlotSelection : TS.TimeSlotSelection
    , eventCreation : EC.EventCreation
    , selectedTimeSlots : List TS.SelectedTimeSlotDetails
    , mdc : Material.Model Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { timeSlotPositions = []
      , timeSlotsElement = Nothing
      , timeSlotSelection = TS.NotSelecting
      , eventCreation = EC.NotCreating
      , selectedTimeSlots = []
      , mdc = Material.defaultModel
      }
    , Cmd.batch [ Material.init Mdc, requestTimeSlotPositions, requestTimeSlotsElement ]
    )



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

        EditTimeSlotSelection selectedTimeslotDetails ->
            TSUpdate.editTimeSlotSelection model selectedTimeslotDetails

        -- EventCreation
        PromptUserForEventDetails result ->
            ECUpdate.promptUserForEventDetails model result

        AdjustEventTitle title ->
            ECUpdate.adjustEventTitle model title

        AdjustEventDescription description ->
            ECUpdate.adjustEventDescription model description

        ChangeSelectionDayNum dayNum ->
            ECUpdate.changeSelectionDayNum model dayNum

        ChangeSelectionStartSlot startSlot ->
            ECUpdate.changeSelectionStartSlot model startSlot

        ChangeSelectionEndSlot endSlot ->
            ECUpdate.changeSelectionEndSlot model endSlot

        CloseUserPromptForEventDetails ->
            ECUpdate.closeUserPromptForEventDetails model



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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
