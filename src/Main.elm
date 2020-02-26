module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, div, text)
import Json.Decode as Decode exposing (field, float)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.List as Lists
import Material.Options as Options exposing (css, styled, when)
import Material.TextField as TextField
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
    , userEventCreation : UserEventCreation
    , selectedTimeSlots : List SelectedTimeSlot
    , mdc : Material.Model Msg
    }


type alias TimeSlotPositions =
    List TimeSlotPosition


type alias TimeSlotPosition =
    { slotNum : Int
    , x : Float
    , y : Float
    , width : Float
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
    ( { numDays = 5
      , numSlotsInDay = defaultNumSlots
      , timeSlotPositions = []
      , timeSlotSelection = NotSelecting
      , userEventCreation = NotCreating
      , selectedTimeSlots = []
      , mdc = Material.defaultModel
      }
    , Cmd.batch [ Material.init Mdc, requestTimeSlotPositions defaultNumSlots ]
    )


defaultNumSlots : Int
defaultNumSlots =
    15


eventDetailsPromptWidth : Float
eventDetailsPromptWidth =
    300



-- UPDATE


type Msg
    = NoOp
    | StartSelectingTimeSlot Int Int
    | SetTimeSlotPositions (Result Dom.Error (List Dom.Element))
    | HandleTimeSlotMouseMove PointerPosition
    | InitiateUserPromptForEventDetails
    | PromptUserForEventDetails (Result Dom.Error Dom.Element)
    | AdjustEventTitle String
    | AdjustEventDescription String
    | CloseUserPromptForEventDetails
    | SetSelectedTimeSlot
    | Mdc (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Mdc msg_ ->
            Material.update Mdc msg_ model

        SetTimeSlotPositions result ->
            case result of
                Ok elementList ->
                    let
                        setTimeSlotPosition ind { element } =
                            { slotNum = ind + 1
                            , x = element.x
                            , y = element.y
                            , width = element.width
                            , height = element.height
                            }
                    in
                    ( { model | timeSlotPositions = List.indexedMap setTimeSlotPosition elementList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        HandleTimeSlotMouseMove { pageY } ->
            case ( model.userEventCreation, model.timeSlotSelection ) of
                ( NotCreating, CurrentlySelecting { dayNum, startBound } ) ->
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
                                            , curEndBound =
                                                { slotNum = pointerTimeSlotPosition.slotNum
                                                , y = pointerTimeSlotPosition.y
                                                , height = pointerTimeSlotPosition.height
                                                }
                                            }
                                  }
                                , Cmd.none
                                )

                ( _, _ ) ->
                    ( model, Cmd.none )

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
                                let
                                    bound =
                                        { slotNum = positionVal.slotNum
                                        , y = positionVal.y
                                        , height = positionVal.height
                                        }
                                in
                                ( { model
                                    | timeSlotSelection =
                                        CurrentlySelecting
                                            { dayNum = dayNum
                                            , startBound = bound
                                            , curEndBound = bound
                                            }
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( model, Cmd.none )

        InitiateUserPromptForEventDetails ->
            case ( model.userEventCreation, model.timeSlotSelection ) of
                ( NotCreating, CurrentlySelecting { dayNum, startBound, curEndBound } ) ->
                    let
                        minSlotNum =
                            min startBound.slotNum curEndBound.slotNum
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
            ( { model | timeSlotSelection = NotSelecting, userEventCreation = NotCreating }, Cmd.none )

        SetSelectedTimeSlot ->
            case ( model.userEventCreation, model.timeSlotSelection ) of
                ( CurrentlyCreatingEvent { title } _, CurrentlySelecting { dayNum, startBound, curEndBound } ) ->
                    let
                        ( startSlot, endSlot ) =
                            if startBound.slotNum <= curEndBound.slotNum then
                                ( startBound, curEndBound )

                            else
                                ( curEndBound, startBound )

                        selectedTimeSlot =
                            { dayNum = dayNum
                            , name = title
                            , startSlot = startSlot
                            , endSlot = endSlot
                            }
                    in
                    ( { model
                        | selectedTimeSlots = selectedTimeSlot :: model.selectedTimeSlots
                        , timeSlotSelection = NotSelecting
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
            List.range 1 numSlots

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
        , Options.onMouseUp InitiateUserPromptForEventDetails
        ]
        (List.append
            (List.map
                (viewSingleDayTimeSlots model)
                (List.range 0 (model.numDays - 1))
            )
            [ viewUserRequest model ]
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


viewUserRequest : Model -> Html Msg
viewUserRequest model =
    case model.userEventCreation of
        NotCreating ->
            text ""

        CurrentlyCreatingEvent eventCreationDetails eventCreationPosition ->
            styled div
                [ css "position" "absolute"
                , css "width" "100%"
                , css "height" "100%"
                , css "z-index" "100"
                , Options.onClick CloseUserPromptForEventDetails
                ]
                [ viewUserRequestForm model eventCreationDetails eventCreationPosition ]


viewUserRequestForm : Model -> EventCreationDetails -> EventCreationPosition -> Html Msg
viewUserRequestForm model eventCreationDetails eventCreationPosition =
    Card.view
        [ css "position" "absolute"
        , css "width" (String.fromFloat eventDetailsPromptWidth ++ "px")
        , css "left" (String.fromFloat eventCreationPosition.x ++ "px")
        , css "top" (String.fromFloat eventCreationPosition.y ++ "px")
        , css "padding" "12px 8px 0px"
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
