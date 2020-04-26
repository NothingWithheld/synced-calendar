module TimeSlots.View exposing (viewCalendarHeading, viewDayHeadings, viewScrollableTimeSlots)

import Html exposing (Html, div, text)
import Html.Entity as Entity
import Json.Decode as Decode exposing (field, float)
import Material
import Material.Card as Card
import Material.Menu
import Material.Options as Options exposing (css, styled, when)
import Material.Select as Select
import Material.Typography as Typography
import Route
import Session exposing (WithSession)
import TimeSlots.TimeSlots as TS
import Utils exposing (WithMdc)
import WeeklyFreeTimes.MainMsg exposing (Msg(..))


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


viewCalendarHeading : WithMdc msg (WithSession a) -> (Material.Msg msg -> msg) -> Html msg
viewCalendarHeading model onMdc =
    styled div
        [ css "height" "10vh"
        , css "display" "flex"
        , css "padding-left" "12px"
        , css "align-items" "center"
        ]
        [ Route.viewHomeButton model onMdc
        , styled div
            [ css "margin-left" "12px" ]
            [ viewTimeZoneSelect model onMdc ]
        ]


viewTimeZoneSelect : WithMdc msg (WithSession a) -> (Material.Msg msg -> msg) -> Html msg
viewTimeZoneSelect model onMdc =
    let
        selectedTimeZone =
            Session.labelForTimeZone model.session
    in
    Select.view onMdc
        "time-zone-select"
        model.mdc
        [ Select.required
        , Select.label "Time Zone"
        , Select.selectedText selectedTimeZone
        ]
        (List.map
            (\timeZoneLabel ->
                viewTimeZoneSelectOption
                    timeZoneLabel
                    (timeZoneLabel == selectedTimeZone)
            )
         <|
            Session.getTimeZoneLabels model.session
        )


viewTimeZoneSelectOption : String -> Bool -> Material.Menu.Item m
viewTimeZoneSelectOption timeZoneLabel isSelected =
    Select.option
        [ Select.value timeZoneLabel
        , when isSelected Select.selected
        ]
        [ text timeZoneLabel ]


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


viewScrollableTimeSlots : TS.WithLoadingTimeSlots (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a)) -> Html Msg
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
        , css "position" "relative"
        , Options.id TS.scrollableTimeSlotsId
        ]
        [ if model.loadingTimeSlots then
            viewLoader

          else
            text ""
        , styled div
            [ css "display" "flex"
            , css "overflow" "hidden"
            , when isSelectingTimeSlots <| css "cursor" "move"
            , when isSelectingTimeSlots <| Options.onMouseUp HandleTimeSlotMouseUp
            ]
            (viewTimeSlotTimes
                :: List.map
                    (viewSingleDayTimeSlots model)
                    TS.dayNumRange
            )
        ]


viewSingleDayTimeSlots : TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a) -> Int -> Html Msg
viewSingleDayTimeSlots model dayNum =
    let
        selectedTimeSlotsForThisDay =
            List.filter ((\timeSlot -> timeSlot.dayNum == dayNum) << TS.getTimeSlotFromDetails) model.selectedTimeSlots

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
                (List.map (viewTimeSlot dayNum) TS.slotNumRange)
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


viewSelectedTimeSlot : TS.SelectedTimeSlotDetails -> Html Msg
viewSelectedTimeSlot selectedTimeSlotDetails =
    let
        (TS.SelectedTimeSlotDetails selectedTimeSlot _) =
            selectedTimeSlotDetails

        cardDimensions =
            getCardDimensions selectedTimeSlot
    in
    Card.view
        [ css "background-color" "#147D64"
        , css "top" (String.fromFloat cardDimensions.y ++ "px")
        , css "height" (String.fromFloat (cardDimensions.height - 4) ++ "px")
        , css "position" "absolute"
        , css "width" "95%"
        , css "border-radius" "8px"
        , css "user-select" "none"
        , Options.onClick <| EditTimeSlotSelection selectedTimeSlotDetails
        ]
        [ viewTimeSlotDuration selectedTimeSlot ]


viewTimeSlotDuration : TS.WithSelectingTimeSlot a -> Html Msg
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


viewCurrentlySelectingTimeSlot : TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a) -> TS.DayNum -> Html Msg
viewCurrentlySelectingTimeSlot model dayNum =
    case model.timeSlotSelection of
        TS.CurrentlySelecting timeSlotSelection ->
            viewUserChangingTimeSlot model timeSlotSelection dayNum

        TS.EditingSelection timeSlotSelection _ ->
            viewUserChangingTimeSlot model timeSlotSelection dayNum

        _ ->
            text ""


viewUserChangingTimeSlot :
    TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a)
    -> TS.WithSelectingTimeSlot b
    -> TS.DayNum
    -> Html Msg
viewUserChangingTimeSlot model timeSlotSelection dayNum =
    let
        cardDimensions =
            getCardDimensions timeSlotSelection

        dayNumCurrentlySelected =
            timeSlotSelection.dayNum

        intersectsTimeSlots =
            TS.doesTSSelectionIntersectSelectedTimeSlots
                model.selectedTimeSlots
                model.timeSlotSelection
    in
    if dayNum == dayNumCurrentlySelected then
        Card.view
            [ if intersectsTimeSlots then
                css "background-color" "#D64545"

              else
                css "background-color" "#2680C2"
            , css "top" (String.fromFloat cardDimensions.y ++ "px")
            , css "height" (String.fromFloat (cardDimensions.height - 4) ++ "px")
            , css "position" "absolute"
            , css "width" "95%"
            , css "z-index" "4"
            , css "box-shadow" "0 6px 10px 0 rgba(0,0,0,0.14), 0 1px 18px 0 rgba(0,0,0,0.12), 0 3px 5px -1px rgba(0,0,0,0.2)"
            , css "border-radius" "8px"
            , css "user-select" "none"
            ]
            [ viewTimeSlotDuration <| TS.getOrderedTimeSlot timeSlotSelection
            ]

    else
        text ""


getCardDimensions : TS.WithSelectingTimeSlot a -> CardDimensions
getCardDimensions { startBound, endBound } =
    let
        ( higherBound, lowerBound ) =
            if startBound.y < endBound.y then
                ( startBound, endBound )

            else
                ( endBound, startBound )

        totalHeight =
            lowerBound.y + lowerBound.height - higherBound.y
    in
    { y = higherBound.y, height = totalHeight }


type alias CardDimensions =
    { y : Float
    , height : Float
    }


viewLoader : Html Msg
viewLoader =
    let
        viewLoaderDot _ =
            styled div [ Options.cs "loader--dot" ] []
    in
    styled div
        [ css "display" "flex"
        , css "justify-content" "center"
        , css "align-items" "center"
        , css "position" "absolute"
        , css "z-index" "1"
        , css "background-color" "white"
        , css "width" "100%"
        , css "height" "100%"
        ]
        [ styled div
            [ Options.cs "loader" ]
            (List.map
                viewLoaderDot
             <|
                List.range 1 6
            )
        ]
