module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Http
import Json.Decode exposing (Decoder, field, int)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.List as Lists
import Material.Options as Options exposing (css, styled, when)
import Material.Typography as Typography
import Task



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { numDays : Int
    , numSlotsInDay : Int
    , timeSlotSelection : TimeSlotSelection
    , selectedTimeSlots2 : List SelectedTimeSlot2
    , mdc : Material.Model Msg
    }


type alias SelectedTimeSlot =
    { name : String
    , number : Int
    }


type alias SelectedTimeSlot2 =
    { dayNum : Int
    , name : String
    , startSlot : TimeSlotBoundaryPosition
    , endSlot : TimeSlotBoundaryPosition
    }


type alias TimeSlotBoundaryPosition =
    { dayNum : Int
    , slotNum : Int
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type BoundaryTimeSlotElem
    = BoundaryTimeSlotNoPosition { dayNum : Int, slotNum : Int }
    | BoundaryTimeSlotWithPosition TimeSlotBoundaryPosition


type TimeSlotSelection
    = NotSelecting
    | CurrentlySelecting
        { startBound : TimeSlotBoundaryPosition
        , curEndBound : TimeSlotBoundaryPosition
        }


type EditCard
    = IsClosed
    | IsOpen String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { numDays = 5
      , numSlotsInDay = 12
      , timeSlotSelection = NotSelecting
      , selectedTimeSlots2 = [ { dayNum = 2, name = "test", startSlot = { dayNum = 2, slotNum = 2, x = 0, y = 48, width = 0, height = 48 }, endSlot = { dayNum = 2, slotNum = 4, x = 0, y = 144, width = 0, height = 48 } } ]
      , mdc = Material.defaultModel
      }
    , Material.init Mdc
    )



-- UPDATE


type Msg
    = StartSelectingTimeSlot Int Int
    | SetInitialTimeSlotSelection Int Int (Result Dom.Error Dom.Element)
    | AdjustTimeSlotSelection Int
    | SetAdjustedTimeSlotSelection TimeSlotBoundaryPosition Int Int (Result Dom.Error Dom.Element)
    | Mdc (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        StartSelectingTimeSlot dayNum slotNum ->
            case model.timeSlotSelection of
                CurrentlySelecting _ ->
                    ( model, Cmd.none )

                NotSelecting ->
                    if intersectsCurrentlySelectedTimeSlots model.selectedTimeSlots2 dayNum slotNum slotNum then
                        ( model, Cmd.none )

                    else
                        let
                            timeSlotId =
                                getTimeSlotId dayNum slotNum
                        in
                        ( model, Task.attempt (SetInitialTimeSlotSelection dayNum slotNum) (Dom.getElement timeSlotId) )

        SetInitialTimeSlotSelection dayNum slotNum result ->
            case result of
                Ok { element } ->
                    let
                        timeSlotPosition =
                            { dayNum = dayNum
                            , slotNum = slotNum
                            , x = element.x
                            , y = element.y
                            , width = element.width
                            , height = element.height
                            }
                    in
                    ( { model | timeSlotSelection = CurrentlySelecting { startBound = timeSlotPosition, curEndBound = timeSlotPosition } }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AdjustTimeSlotSelection slotNum ->
            case model.timeSlotSelection of
                NotSelecting ->
                    ( model, Cmd.none )

                CurrentlySelecting { startBound } ->
                    if intersectsCurrentlySelectedTimeSlots model.selectedTimeSlots2 startBound.dayNum startBound.slotNum slotNum then
                        ( model, Cmd.none )

                    else
                        let
                            dayNum =
                                startBound.dayNum

                            timeSlotId =
                                getTimeSlotId dayNum slotNum
                        in
                        ( model, Task.attempt (SetAdjustedTimeSlotSelection startBound dayNum slotNum) (Dom.getElement timeSlotId) )

        SetAdjustedTimeSlotSelection startBound dayNum slotNum result ->
            case result of
                Ok { element } ->
                    let
                        endTimeSlotPosition =
                            { dayNum = dayNum
                            , slotNum = slotNum
                            , x = element.x
                            , y = element.y
                            , width = element.width
                            , height = element.height
                            }
                    in
                    ( { model | timeSlotSelection = CurrentlySelecting { startBound = startBound, curEndBound = endTimeSlotPosition } }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


intersectsCurrentlySelectedTimeSlots : List SelectedTimeSlot2 -> Int -> Int -> Int -> Bool
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
            List.any (\selected -> timeSlot >= selected.startSlot.slotNum && timeSlot <= selected.endSlot.slotNum) selectedTimeSlotsForThisDay
    in
    List.any isTimeSlotTaken (List.range lowerSlotNum higherSlotNum)


getTimeSlotKey : Int -> Int -> String
getTimeSlotKey dayId timeSlotId =
    String.fromInt dayId ++ "-" ++ String.fromInt timeSlotId



-- VIEW


view : Model -> Html Msg
view model =
    styled div
        [ css "display" "flex" ]
        (List.append
            (List.map
                (viewSingleDayTimeSlots model)
                (List.range 1 model.numDays)
            )
            []
        )


viewSingleDayTimeSlots : Model -> Int -> Html Msg
viewSingleDayTimeSlots model dayNum =
    let
        selectedTimeSlotsForThisDay =
            List.filter (\timeSlot -> timeSlot.dayNum == dayNum) model.selectedTimeSlots2
    in
    styled div
        [ css "flex-grow" "1", css "position" "relative" ]
        (List.append
            [ Lists.ul Mdc
                (getTimeSlotIdFrontHalf dayNum)
                model.mdc
                []
                (List.map (viewTimeSlot model dayNum) (List.range 1 model.numSlotsInDay))
            , viewCurrentlySelectingTimeSlot model dayNum
            ]
            (List.map viewSelectedTimeSlot selectedTimeSlotsForThisDay)
        )


viewTimeSlot : Model -> Int -> Int -> Lists.ListItem Msg
viewTimeSlot _ dayNum slotNum =
    Lists.li
        [ css "border" "thin solid black"
        , Options.onMouseDown (StartSelectingTimeSlot dayNum slotNum)
        , Options.onMouseEnter (AdjustTimeSlotSelection slotNum)
        ]
        []


viewSelectedTimeSlot : SelectedTimeSlot2 -> Html Msg
viewSelectedTimeSlot selectedTimeSlot =
    let
        cardDimensions =
            getCardDimensions selectedTimeSlot.startSlot selectedTimeSlot.endSlot
    in
    Card.view
        [ css "background-color" "red"
        , css "top" (String.fromFloat cardDimensions.y ++ "px")
        , css "height" (String.fromFloat cardDimensions.height ++ "px")
        , css "position" "absolute"
        , css "width" "100%"
        , css "z-index" "4"
        ]
        [ styled div [ Typography.subheading1 ] [ text selectedTimeSlot.name ] ]


viewCurrentlySelectingTimeSlot : Model -> Int -> Html Msg
viewCurrentlySelectingTimeSlot model dayNum =
    case model.timeSlotSelection of
        NotSelecting ->
            text ""

        CurrentlySelecting { startBound, curEndBound } ->
            let
                cardDimensions =
                    getCardDimensions startBound curEndBound

                dayNumCurrentlySelected =
                    startBound.dayNum
            in
            if dayNum == dayNumCurrentlySelected then
                Card.view
                    [ css "background-color" "green"
                    , css "top" (String.fromFloat cardDimensions.y ++ "px")
                    , css "height" (String.fromFloat cardDimensions.height ++ "px")
                    , css "position" "absolute"
                    , css "width" "100%"
                    , css "z-index" "4"
                    ]
                    [ styled div [ Typography.subheading1 ] [ text "(group name)" ] ]

            else
                text ""


getCardDimensions : TimeSlotBoundaryPosition -> TimeSlotBoundaryPosition -> CardDimensions
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
    getTimeSlotIdFrontHalf dayNum ++ getTimeSlotIdBackHalf slotNum


getTimeSlotIdFrontHalf : Int -> String
getTimeSlotIdFrontHalf dayNum =
    "time-slot-" ++ String.fromInt dayNum


getTimeSlotIdBackHalf : Int -> String
getTimeSlotIdBackHalf slotNum =
    "--" ++ String.fromInt (slotNum - 1)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
