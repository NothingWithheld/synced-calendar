module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Flip
import Html exposing (Html, div, text)
import Json.Decode as Decode exposing (field, float)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Options as Options exposing (css, styled, when)
import Material.TextField as TextField
import Material.Typography as Typography
import Task
import TimeSlots as TS



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


type Msg
    = NoOp
    | StartSelectingTimeSlot Int Int
    | SetTimeSlotPositions (Result Dom.Error (List Dom.Element))
    | SetTimeSlotsElement (Result Dom.Error Dom.Element)
    | HandleTimeSlotMouseMove PointerPosition
    | AdjustTimeSlotSelection PointerPosition (Result Dom.Error Dom.Viewport)
    | InitiateUserPromptForEventDetails
    | PromptUserForEventDetails (Result Dom.Error Dom.Element)
    | AdjustEventTitle String
    | AdjustEventDescription String
    | CloseUserPromptForEventDetails
    | SetSelectedTimeSlot
    | Mdc (Material.Msg Msg)


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

        SetTimeSlotPositions result ->
            let
                setTimeSlotPositions elementList =
                    let
                        setTimeSlotPosition ind curYOffset elements =
                            case elements of
                                [] ->
                                    []

                                { element } :: xs ->
                                    { slotNum = ind
                                    , y = curYOffset
                                    , height = element.height
                                    }
                                        :: setTimeSlotPosition (ind + 1) (curYOffset + element.height) xs
                    in
                    ( { model | timeSlotPositions = setTimeSlotPosition TS.startingSlotNum 0 elementList }, Cmd.none )
            in
            noUpdateOnError result setTimeSlotPositions

        SetTimeSlotsElement result ->
            case result of
                Ok { element } ->
                    ( { model | timeSlotsElement = Just element }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        HandleTimeSlotMouseMove pointerPosition ->
            ( model, Task.attempt (AdjustTimeSlotSelection pointerPosition) (Dom.getViewportOf scrollableTimeSlotsId) )

        AdjustTimeSlotSelection { pageY } result ->
            case result of
                Ok { viewport } ->
                    case ( model.userEventCreation, model.timeSlotSelection, model.timeSlotsElement ) of
                        ( NotCreating, TS.CurrentlySelecting { dayNum, startBound }, Just { y } ) ->
                            let
                                yPositionInTimeSlots =
                                    pageY - y + viewport.y

                                maybePointerTSPosition =
                                    getTimeSlotPositionOfPointer model.timeSlotPositions yPositionInTimeSlots

                                updateSelectionWithPointerPosition pointerTSPosition =
                                    noUpdateIfIntersectsSelectedTS
                                        dayNum
                                        startBound.slotNum
                                        pointerTSPosition.slotNum
                                    <|
                                        TS.useTSPositionForEndSelectionBound
                                            model
                                            dayNum
                                            startBound
                                            pointerTSPosition
                            in
                            noUpdateWithoutData
                                maybePointerTSPosition
                            <|
                                useWithoutCmdMsg <|
                                    updateSelectionWithPointerPosition

                        ( _, _, _ ) ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        StartSelectingTimeSlot dayNum slotNum ->
            case model.timeSlotSelection of
                TS.NotSelecting ->
                    let
                        timeSlotPosition =
                            getListItemAt slotNum model.timeSlotPositions
                    in
                    noUpdateWithoutData
                        timeSlotPosition
                    <|
                        useWithoutCmdMsg <|
                            noUpdateIfIntersectsSelectedTS dayNum slotNum slotNum
                                << TS.useTSPositionForBothSelectionBounds model dayNum

                TS.CurrentlySelecting _ ->
                    ( model, Cmd.none )

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


getTimeSlotPositionOfPointer : List TS.TimeSlotBoundaryPosition -> Float -> Maybe TS.TimeSlotBoundaryPosition
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


requestTimeSlotsElement : Cmd Msg
requestTimeSlotsElement =
    Task.attempt SetTimeSlotsElement (Dom.getElement scrollableTimeSlotsId)


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


viewDayHeadings : Html Msg
viewDayHeadings =
    let
        dayAbbreviations =
            [ "SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT" ]
    in
    styled div
        [ css "display" "flex"
        , css "height" "10vh"
        , css "margin-left" "70px"
        , css "margin-right" "16px"
        , css "border-bottom" "1px solid #829AB1"
        , css "position" "relative"
        ]
        (styled div
            [ css "height" "30%"
            , css "width" "0"
            , css "border-right" "1px solid #829AB1"
            , css "position" "absolute"
            , css "bottom" "0"
            ]
            []
            :: List.map
                viewDayHeading
                dayAbbreviations
        )


viewDayHeading : String -> Html Msg
viewDayHeading dayAbbreviation =
    styled div
        [ css "flex-grow" "1"
        , css "flex-basis" "100%"
        , css "display" "flex"
        , css "justify-content" "flex-end"
        , css "position" "relative"
        ]
        [ styled Html.h2
            [ Typography.headline6, css "margin" "auto" ]
            [ text dayAbbreviation ]
        , styled div
            [ css "height" "30%"
            , css "width" "0"
            , css "border-right" "1px solid #829AB1"
            , css "position" "absolute"
            , css "bottom" "0"
            ]
            []
        ]


viewScrollableTimeSlots : Model -> Html Msg
viewScrollableTimeSlots model =
    let
        isSelectingTimeSlots =
            case model.timeSlotSelection of
                TS.NotSelecting ->
                    False

                TS.CurrentlySelecting _ ->
                    True
    in
    styled div
        [ css "height" "80vh"
        , css "overflow-y" "scroll"
        , Options.id scrollableTimeSlotsId
        ]
        [ styled div
            [ css "display" "flex"
            , css "overflow" "hidden"
            , when isSelectingTimeSlots (Options.onMouseUp InitiateUserPromptForEventDetails)
            ]
            (viewTimeSlotTimes
                :: List.map
                    (viewSingleDayTimeSlots model)
                    (List.range 0 (model.numDays - 1))
            )
        ]


scrollableTimeSlotsId : String
scrollableTimeSlotsId =
    "scrollable-time-slots"


viewTimeSlotTimes : Html Msg
viewTimeSlotTimes =
    styled div
        [ css "width" "70px"
        , css "border-right" "1px solid #829AB1"
        ]
        (styled div [ css "height" "32px" ] []
            :: List.map viewTimeSlotTime (List.range 1 (defaultNumSlots // 4 - 1))
        )


viewTimeSlotTime : Int -> Html Msg
viewTimeSlotTime hour =
    let
        dayPeriod =
            if hour < 12 then
                "AM"

            else
                "PM"

        adjustedHour =
            if hour > 12 then
                hour - 12

            else
                hour
    in
    styled
        div
        [ css "height" "65px"
        , css "display" "flex"
        , css "flex-direction" "row-reverse"
        , css "align-items" "center"
        , Typography.caption
        ]
        [ styled div
            [ css "width" "8px"
            , css "border-bottom" "1px solid #829AB1"
            , css "margin-left" "6px"
            ]
            []
        , text
            (String.fromInt adjustedHour ++ " " ++ dayPeriod)
        ]


viewSingleDayTimeSlots : Model -> Int -> Html Msg
viewSingleDayTimeSlots model dayNum =
    let
        selectedTimeSlotsForThisDay =
            List.filter (\timeSlot -> timeSlot.dayNum == dayNum) model.selectedTimeSlots

        isSelectingTimeSlots =
            case model.timeSlotSelection of
                TS.NotSelecting ->
                    False

                TS.CurrentlySelecting _ ->
                    True
    in
    styled div
        [ css "flex-grow" "1", css "position" "relative", when isSelectingTimeSlots onTimeSlotMouseMove ]
        (List.append
            [ div
                []
                (List.map (viewTimeSlot model dayNum) (List.range 0 (model.numSlotsInDay - 1)))
            , viewCurrentlySelectingTimeSlot model dayNum
            ]
            (List.map viewSelectedTimeSlot selectedTimeSlotsForThisDay)
        )


viewTimeSlot : Model -> Int -> Int -> Html Msg
viewTimeSlot _ dayNum slotNum =
    styled div
        [ css "border-right" "1px solid #829AB1"
        , when (modBy 4 slotNum == 3) (css "border-bottom" "1px solid #829AB1")
        , css "height" "16px"
        , Options.onMouseDown (StartSelectingTimeSlot dayNum slotNum)
        , Options.id (getTimeSlotId dayNum slotNum)
        ]
        []


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


viewCurrentlySelectingTimeSlot : Model -> Int -> Html Msg
viewCurrentlySelectingTimeSlot model dayNum =
    case model.timeSlotSelection of
        TS.NotSelecting ->
            text ""

        TS.CurrentlySelecting ({ startBound, endBound } as selectionDetails) ->
            let
                cardDimensions =
                    getCardDimensions startBound endBound

                dayNumCurrentlySelected =
                    selectionDetails.dayNum
            in
            if dayNum == dayNumCurrentlySelected then
                Card.view
                    [ css "background-color" "green"
                    , css "top" (String.fromFloat cardDimensions.y ++ "px")
                    , css "height" (String.fromFloat (cardDimensions.height - 4) ++ "px")
                    , css "position" "absolute"
                    , css "width" "95%"
                    , css "z-index" "4"
                    , css "box-shadow" "0 6px 10px 0 rgba(0,0,0,0.14), 0 1px 18px 0 rgba(0,0,0,0.12), 0 3px 5px -1px rgba(0,0,0,0.2)"
                    , css "border-radius" "8px"
                    ]
                    [ styled div [ Typography.subheading1 ] [ text "(group name)" ] ]

            else
                text ""


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
