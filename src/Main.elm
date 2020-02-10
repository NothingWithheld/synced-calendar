module Main exposing (main)

import Browser
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



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { selectedTimeSlots : Dict String SelectedTimeSlot
    , numDays : Int
    , numSlotsInDay : Int
    , editCardDetails : EditCard
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
        { startBound : BoundaryTimeSlotElem
        , curEndBound : BoundaryTimeSlotElem
        }


type EditCard
    = IsClosed
    | IsOpen String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selectedTimeSlots = Dict.empty
      , numDays = 5
      , numSlotsInDay = 12
      , editCardDetails = IsClosed
      , timeSlotSelection = NotSelecting
      , selectedTimeSlots2 = [ { dayNum = 2, name = "test", startSlot = { dayNum = 1, slotNum = 2, x = 0, y = 48, width = 0, height = 48 }, endSlot = { dayNum = 1, slotNum = 4, x = 0, y = 144, width = 0, height = 48 } } ]
      , mdc = Material.defaultModel
      }
    , Material.init Mdc
    )



-- UPDATE


type Msg
    = PromptUserForTimeSlot String
    | TimeSlotNotSelected
    | RequestNumber String
    | GotNumber String (Result Http.Error Int)
    | SelectTimeSlot String Int
    | Mdc (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        PromptUserForTimeSlot timeSlotKey ->
            ( { model | editCardDetails = IsOpen timeSlotKey }, Cmd.none )

        TimeSlotNotSelected ->
            ( { model | editCardDetails = IsClosed }, Cmd.none )

        SelectTimeSlot timeSlotKey number ->
            ( { model
                | editCardDetails = IsClosed
                , selectedTimeSlots = Dict.insert timeSlotKey { name = "hi", number = number } model.selectedTimeSlots
              }
            , Cmd.none
            )

        RequestNumber timeSlotKey ->
            ( model, getNumber timeSlotKey )

        GotNumber timeSlotKey result ->
            case result of
                Ok number ->
                    ( { model
                        | editCardDetails = IsClosed
                        , selectedTimeSlots = Dict.insert timeSlotKey { name = "hi", number = number } model.selectedTimeSlots
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | editCardDetails = IsClosed }, Cmd.none )


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
            [ viewEditCard model ]
        )


viewSingleDayTimeSlots : Model -> Int -> Html Msg
viewSingleDayTimeSlots model dayId =
    let
        selectedTimeSlotsForThisDay =
            List.filter (\timeSlot -> timeSlot.dayNum == dayId) model.selectedTimeSlots2
    in
    styled div
        [ css "flex-grow" "1", css "position" "relative" ]
        (List.append
            [ Lists.ul Mdc
                ("list-" ++ String.fromInt dayId)
                model.mdc
                []
                (List.map (viewTimeSlot model dayId) (List.range 1 model.numSlotsInDay))
            ]
            (List.map viewSelectedTimeSlot selectedTimeSlotsForThisDay)
        )


viewTimeSlot : Model -> Int -> Int -> Lists.ListItem Msg
viewTimeSlot model dayId timeSlotId =
    let
        timeSlotKey =
            getTimeSlotKey dayId timeSlotId

        isSelected =
            Dict.member timeSlotKey model.selectedTimeSlots

        number =
            Maybe.map .number (Dict.get timeSlotKey model.selectedTimeSlots)
    in
    Lists.li
        [ when isSelected (css "background-color" "red")
        , css "border" "thin solid black"
        , Options.onClick (PromptUserForTimeSlot timeSlotKey)
        ]
        (case number of
            Just value ->
                [ text (String.fromInt value) ]

            Nothing ->
                []
        )


viewEditCard : Model -> Html Msg
viewEditCard model =
    case model.editCardDetails of
        IsClosed ->
            text ""

        IsOpen timeSlotKey ->
            Card.view []
                [ text "Select?"
                , Card.actions []
                    [ Card.actionButtons []
                        [ Button.view Mdc
                            "select-time-slot-button"
                            model.mdc
                            [ Card.actionButton
                            , Button.ripple
                            , Options.onClick (RequestNumber timeSlotKey)
                            ]
                            [ text "Yes" ]
                        , Button.view Mdc
                            "dont-select-time-slot-button"
                            model.mdc
                            [ Card.actionButton
                            , Button.ripple
                            , Options.onClick TimeSlotNotSelected
                            ]
                            [ text "No" ]
                        ]
                    ]
                ]


viewSelectedTimeSlot : SelectedTimeSlot2 -> Html Msg
viewSelectedTimeSlot selectedTimeSlot =
    let
        selectedTimeSlotHeight =
            selectedTimeSlot.endSlot.y + selectedTimeSlot.endSlot.height - selectedTimeSlot.startSlot.y
    in
    Card.view
        [ css "background-color" "red"
        , css "top" (String.fromFloat selectedTimeSlot.startSlot.y ++ "px")
        , css "height" (String.fromFloat selectedTimeSlotHeight ++ "px")
        , css "position" "absolute"
        , css "width" "100%"
        , css "z-index" "4"
        ]
        [ styled div [ Typography.subheading1 ] [ text selectedTimeSlot.name ] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model



-- HTTP


getNumber : String -> Cmd Msg
getNumber timeSlotKey =
    Http.get { url = "http://localhost:3000/number", expect = Http.expectJson (GotNumber timeSlotKey) numberDecoder }


numberDecoder : Decoder Int
numberDecoder =
    field "number" int
