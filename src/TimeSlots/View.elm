module TimeSlots.View exposing (viewDayHeadings, viewScrollableTimeSlots)

import Html exposing (Html, div, text)
import Html.Entity as Entity
import Json.Decode as Decode exposing (field, float)
import MainMsg exposing (Msg(..))
import Material.Card as Card
import Material.Options as Options exposing (css, styled, when)
import Material.Typography as Typography
import TimeSlots.TimeSlots as TS


onTimeSlotMouseMove : Options.Property c Msg
onTimeSlotMouseMove =
    Options.on "mousemove"
        (Decode.map HandleTimeSlotMouseMove
            (Decode.map2
                TS.PointerPosition
                (field "pageX" float)
                (field "pageY" float)
            )
        )


viewDayHeadings : Html Msg
viewDayHeadings =
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
                TS.dayAbbreviations
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


viewTimeSlotTimes : Html Msg
viewTimeSlotTimes =
    styled div
        [ css "width" "70px"
        , css "border-right" "1px solid #829AB1"
        ]
        (styled div [ css "height" "32px" ] []
            :: List.map viewTimeSlotTime (List.range 1 (TS.defaultNumSlots // 4 - 1))
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


viewScrollableTimeSlots : TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a) -> Html Msg
viewScrollableTimeSlots model =
    let
        isSelectingTimeSlots =
            case model.timeSlotSelection of
                TS.InitialPressNoMove _ ->
                    True

                TS.CurrentlySelecting _ ->
                    True

                _ ->
                    False
    in
    styled div
        [ css "height" "80vh"
        , css "overflow-y" "scroll"
        , Options.id TS.scrollableTimeSlotsId
        ]
        [ styled div
            [ css "display" "flex"
            , css "overflow" "hidden"
            , when isSelectingTimeSlots <| css "cursor" "move"
            , when isSelectingTimeSlots <| Options.onMouseUp HandleTimeSlotMouseUp
            ]
            (viewTimeSlotTimes
                :: List.map
                    (viewSingleDayTimeSlots model)
                    (List.range TS.startingDayNum (TS.startingDayNum + TS.defaultNumDays - 1))
            )
        ]


viewSingleDayTimeSlots : TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a) -> Int -> Html Msg
viewSingleDayTimeSlots model dayNum =
    let
        selectedTimeSlotsForThisDay =
            List.map TS.getTimeSlotFromDetails model.selectedTimeSlots
                |> List.filter (\timeSlot -> timeSlot.dayNum == dayNum)

        isSelectingTimeSlots =
            case model.timeSlotSelection of
                TS.InitialPressNoMove _ ->
                    True

                TS.CurrentlySelecting _ ->
                    True

                _ ->
                    False
    in
    styled div
        [ css "flex-grow" "1", css "position" "relative", when isSelectingTimeSlots onTimeSlotMouseMove ]
        (List.append
            [ div
                []
                (List.map (viewTimeSlot dayNum) (List.range TS.startingSlotNum (TS.startingSlotNum + TS.defaultNumSlots - 1)))
            , viewCurrentlySelectingTimeSlot model dayNum
            ]
            (List.map viewSelectedTimeSlot selectedTimeSlotsForThisDay)
        )


viewTimeSlot : Int -> Int -> Html Msg
viewTimeSlot dayNum slotNum =
    styled div
        [ css "border-right" "1px solid #829AB1"
        , when (modBy 4 slotNum == 3) (css "border-bottom" "1px solid #829AB1")
        , css "height" "16px"
        , Options.onMouseDown (StartSelectingTimeSlot dayNum slotNum)
        , Options.id (TS.getTimeSlotId dayNum slotNum)
        ]
        []


viewSelectedTimeSlot : TS.WithSelectedTimeSlot a -> Html Msg
viewSelectedTimeSlot selectedTimeSlot =
    let
        cardDimensions =
            getCardDimensions selectedTimeSlot.startBound selectedTimeSlot.endBound
    in
    Card.view
        [ css "background-color" "#147D64"
        , css "top" (String.fromFloat cardDimensions.y ++ "px")
        , css "height" (String.fromFloat (cardDimensions.height - 4) ++ "px")
        , css "position" "absolute"
        , css "width" "95%"
        , css "z-index" "4"
        , css "border-radius" "8px"
        , css "user-select" "none"
        ]
        [ viewTimeSlotDuration selectedTimeSlot ]


viewTimeSlotDuration : TS.WithSelectedTimeSlot a -> Html Msg
viewTimeSlotDuration { startBound, endBound } =
    let
        ( startTime, startAmOrPm ) =
            TS.getTimeForSlotNum startBound.slotNum False

        ( endTime, endAmOrPm ) =
            TS.getTimeForSlotNum endBound.slotNum True

        startsAndEndsSameHalfOfDay =
            startAmOrPm == endAmOrPm
    in
    styled Html.p
        [ Typography.caption
        , css "color" "white"
        , css "margin" "2px 8px"
        ]
        [ text <|
            startTime
                ++ (if startsAndEndsSameHalfOfDay then
                        ""

                    else
                        startAmOrPm
                   )
                ++ " "
                ++ Entity.mdash
                ++ " "
                ++ endTime
                ++ endAmOrPm
        ]


viewCurrentlySelectingTimeSlot : TS.WithTimeSlotSelection a -> Int -> Html Msg
viewCurrentlySelectingTimeSlot model dayNum =
    case model.timeSlotSelection of
        TS.CurrentlySelecting ({ startBound, endBound } as selectionDetails) ->
            let
                cardDimensions =
                    getCardDimensions startBound endBound

                dayNumCurrentlySelected =
                    selectionDetails.dayNum
            in
            if dayNum == dayNumCurrentlySelected then
                Card.view
                    [ css "background-color" "#2680C2"
                    , css "top" (String.fromFloat cardDimensions.y ++ "px")
                    , css "height" (String.fromFloat (cardDimensions.height - 4) ++ "px")
                    , css "position" "absolute"
                    , css "width" "95%"
                    , css "z-index" "4"
                    , css "box-shadow" "0 6px 10px 0 rgba(0,0,0,0.14), 0 1px 18px 0 rgba(0,0,0,0.12), 0 3px 5px -1px rgba(0,0,0,0.2)"
                    , css "border-radius" "8px"
                    , css "user-select" "none"
                    ]
                    [ viewTimeSlotDuration <| TS.getOrderedTimeSlot selectionDetails
                    ]

            else
                text ""

        _ ->
            text ""


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
