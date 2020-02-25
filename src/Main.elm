module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, div, text)
import Json.Decode as Decode exposing (field, float)
import Material
import Material.Card as Card
import Material.List as Lists
import Material.Options as Options exposing (css, styled, when)
import Material.Typography as Typography
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = \_ -> init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { numDays : Int
    , numSlotsInDay : Int
    , timeSlotPositions : TimeSlotPositions
    , timeSlotSelection : TimeSlotSelection
    , selectedTimeSlots : List SelectedTimeSlot
    , mdc : Material.Model Msg
    }


type alias TimeSlotPositions =
    List TimeSlotPosition


type alias TimeSlotPosition =
    { slotNum : Int
    , y : Float
    , height : Float
    }


type alias SelectedTimeSlot =
    { dayNum : Int
    , name : String
    , startSlot : TimeSlotBoundaryPosition
    , endSlot : TimeSlotBoundaryPosition
    }


type alias TimeSlotBoundaryPosition =
    { slotNum : Int
    , y : Float
    , height : Float
    }


type TimeSlotSelection
    = NotSelecting
    | CurrentlySelecting
        { dayNum : Int
        , startBound : TimeSlotBoundaryPosition
        , curEndBound : TimeSlotBoundaryPosition
        }


type alias PointerPosition =
    { pageX : Float
    , pageY : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { numDays = 5
      , numSlotsInDay = defaultNumSlots
      , timeSlotPositions = []
      , timeSlotSelection = NotSelecting
      , selectedTimeSlots = []
      , mdc = Material.defaultModel
      }
    , Cmd.batch [ Material.init Mdc, requestTimeSlotPositions defaultNumSlots ]
    )


defaultNumSlots : Int
defaultNumSlots =
    15



-- UPDATE


type Msg
    = StartSelectingTimeSlot Int Int
    | SetTimeSlotPositions (Result Dom.Error (List Dom.Element))
    | HandleTimeSlotMouseMove PointerPosition
    | SetSelectedTimeSlot TimeSlotSelection
    | Mdc (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        SetTimeSlotPositions result ->
            case result of
                Ok elementList ->
                    let
                        setTimeSlotPosition ind { element } =
                            { slotNum = ind + 1
                            , y = element.y
                            , height = element.height
                            }
                    in
                    ( { model | timeSlotPositions = List.indexedMap setTimeSlotPosition elementList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        HandleTimeSlotMouseMove { pageY } ->
            case model.timeSlotSelection of
                NotSelecting ->
                    ( model, Cmd.none )

                CurrentlySelecting { dayNum, startBound } ->
                    let
                        maybePointerTSPosition =
                            getTimeSlotPositionOfPointer model.timeSlotPositions pageY
                    in
                    case maybePointerTSPosition of
                        Nothing ->
                            ( model, Cmd.none )

                        Just pointerTimeSlotPosition ->
                            if intersectsCurrentlySelectedTimeSlots model.selectedTimeSlots dayNum startBound.slotNum pointerTimeSlotPosition.slotNum then
                                ( model, Cmd.none )

                            else
                                ( { model
                                    | timeSlotSelection =
                                        CurrentlySelecting
                                            { dayNum = dayNum
                                            , startBound = startBound
                                            , curEndBound = pointerTimeSlotPosition
                                            }
                                  }
                                , Cmd.none
                                )

        StartSelectingTimeSlot dayNum slotNum ->
            case model.timeSlotSelection of
                CurrentlySelecting _ ->
                    ( model, Cmd.none )

                NotSelecting ->
                    if intersectsCurrentlySelectedTimeSlots model.selectedTimeSlots dayNum slotNum slotNum then
                        ( model, Cmd.none )

                    else
                        let
                            timeSlotPosition =
                                getListItemAt slotNum model.timeSlotPositions
                        in
                        case timeSlotPosition of
                            Just positionVal ->
                                ( { model
                                    | timeSlotSelection =
                                        CurrentlySelecting
                                            { dayNum = dayNum
                                            , startBound = positionVal
                                            , curEndBound = positionVal
                                            }
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( model, Cmd.none )

        SetSelectedTimeSlot timeSlotSelection ->
            case timeSlotSelection of
                CurrentlySelecting { dayNum, startBound, curEndBound } ->
                    let
                        ( startSlot, endSlot ) =
                            if startBound.slotNum <= curEndBound.slotNum then
                                ( startBound, curEndBound )

                            else
                                ( curEndBound, startBound )

                        selectedTimeSlot =
                            { dayNum = dayNum
                            , name = "no name"
                            , startSlot = startSlot
                            , endSlot = endSlot
                            }
                    in
                    ( { model
                        | selectedTimeSlots = selectedTimeSlot :: model.selectedTimeSlots
                        , timeSlotSelection = NotSelecting
                      }
                    , Cmd.none
                    )

                NotSelecting ->
                    ( model, Cmd.none )


getListItemAt : Int -> List a -> Maybe a
getListItemAt index list =
    let
        getListItemAtHelper curList curIndex =
            case curList of
                [] ->
                    Nothing

                head :: rest ->
                    if curIndex == index then
                        Just head

                    else
                        getListItemAtHelper rest (curIndex + 1)
    in
    getListItemAtHelper list 0


getTimeSlotPositionOfPointer : List TimeSlotPosition -> Float -> Maybe TimeSlotPosition
getTimeSlotPositionOfPointer timeSlotPositions pageY =
    let
        getTSPOPHelper curTSPosList =
            case curTSPosList of
                [] ->
                    Nothing

                curTSPos :: rest ->
                    if (curTSPos.y <= pageY) && (pageY <= curTSPos.y + curTSPos.height) then
                        Just curTSPos

                    else
                        getTSPOPHelper rest
    in
    getTSPOPHelper timeSlotPositions


onTimeSlotMouseMove : Options.Property c Msg
onTimeSlotMouseMove =
    Options.on "mousemove"
        (Decode.map HandleTimeSlotMouseMove
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
    Task.attempt SetTimeSlotPositions (Task.sequence (List.map getTimeSlotPosition slotNumList))


intersectsCurrentlySelectedTimeSlots : List SelectedTimeSlot -> Int -> Int -> Int -> Bool
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



-- VIEW


view : Model -> Html Msg
view model =
    let
        isSelectingTimeSlots =
            case model.timeSlotSelection of
                NotSelecting ->
                    False

                CurrentlySelecting _ ->
                    True
    in
    styled div
        [ css "display" "flex"
        , when isSelectingTimeSlots onTimeSlotMouseMove
        , Options.onMouseUp (SetSelectedTimeSlot model.timeSlotSelection)
        ]
        (List.append
            (List.map
                (viewSingleDayTimeSlots model)
                (List.range 0 (model.numDays - 1))
            )
            []
        )


viewSingleDayTimeSlots : Model -> Int -> Html Msg
viewSingleDayTimeSlots model dayNum =
    let
        selectedTimeSlotsForThisDay =
            List.filter (\timeSlot -> timeSlot.dayNum == dayNum) model.selectedTimeSlots
    in
    styled div
        [ css "flex-grow" "1", css "position" "relative" ]
        (List.append
            [ Lists.ul Mdc
                (getTimeSlotIdFrontHalf dayNum)
                model.mdc
                []
                (List.map (viewTimeSlot model dayNum) (List.range 0 (model.numSlotsInDay - 1)))
            , viewCurrentlySelectingTimeSlot model dayNum
            ]
            (List.map viewSelectedTimeSlot selectedTimeSlotsForThisDay)
        )


viewTimeSlot : Model -> Int -> Int -> Lists.ListItem Msg
viewTimeSlot _ dayNum slotNum =
    Lists.li
        [ css "border" "thin solid black"
        , Options.onMouseDown (StartSelectingTimeSlot dayNum slotNum)
        ]
        []


viewSelectedTimeSlot : SelectedTimeSlot -> Html Msg
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

        CurrentlySelecting ({ startBound, curEndBound } as selectionDetails) ->
            let
                cardDimensions =
                    getCardDimensions startBound curEndBound

                dayNumCurrentlySelected =
                    selectionDetails.dayNum
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
    "--" ++ String.fromInt slotNum



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
