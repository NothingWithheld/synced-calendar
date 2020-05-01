module TimeSlots.View exposing (viewCalendarHeading, viewDayHeadings, viewScrollableTimeSlots)

import Html exposing (Html, div, text)
import Html.Entity as Entity
import Json.Decode as Decode exposing (field, float)
import Material
import Material.Card as Card
import Material.IconButton as IconButton
import Material.Menu
import Material.Options as Options exposing (css, nop, styled, when)
import Material.Select as Select
import Material.Typography as Typography
import Route
import Session exposing (WithSession)
import Time exposing (Posix)
import TimeSlots.Time as TSTime exposing (TimeDetails(..))
import TimeSlots.TimeSlots as TS exposing (Calendar(..))
import Utils exposing (WithMdc)


onTimeSlotMouseMove : (TS.PointerPosition -> msg) -> Options.Property c msg
onTimeSlotMouseMove handleTimeSlotMouseMove =
    Options.on "mousemove"
        (Decode.map handleTimeSlotMouseMove
            (Decode.map2
                TS.PointerPosition
                (field "pageX" float)
                (field "pageY" float)
            )
        )



-- Calendar Heading


viewCalendarHeading :
    WithMdc msg (WithSession (TS.WithLoadingAll (TSTime.WithTimeDetails a)))
    ->
        Calendar
            { b
                | onMdc : Material.Msg msg -> msg
                , onTimeZoneSelect : String -> msg
            }
            { c
                | onMdc : Material.Msg msg -> msg
                , onTimeZoneSelect : String -> msg
                , moveWeekForward : msg
                , moveWeekBackward : msg
            }
    -> Html msg
viewCalendarHeading model updates =
    case updates of
        WeeklyFreeTimes ({ onMdc } as updates_) ->
            styled div
                [ css "height" "10vh"
                , css "display" "flex"
                , css "padding-left" "12px"
                , css "align-items" "center"
                ]
                [ Route.viewHomeButton model onMdc
                , styled div
                    [ css "margin-left" "12px" ]
                    [ viewTimeZoneSelect model updates_ ]
                ]

        Events ({ onMdc } as updates_) ->
            styled div
                [ css "height" "10vh"
                , css "display" "flex"
                , css "padding-left" "12px"
                , css "align-items" "center"
                ]
                [ Route.viewHomeButton model onMdc
                , styled div [ css "width" "12px" ] []
                , viewTimeZoneSelect model updates_
                , viewWeekHeadingItems model updates_
                ]


viewTimeZoneSelect :
    WithMdc msg (WithSession (TS.WithLoadingAll a))
    -> { b | onMdc : Material.Msg msg -> msg, onTimeZoneSelect : String -> msg }
    -> Html msg
viewTimeZoneSelect model { onMdc, onTimeZoneSelect } =
    let
        selectedTimeZone =
            Session.labelForTimeZone model.session
    in
    Select.view onMdc
        "time-zone-select"
        model.mdc
        [ Select.required
        , when (TS.isLoading model) Select.disabled
        , Select.label "Time Zone"
        , Select.selectedText selectedTimeZone
        , Select.onSelect onTimeZoneSelect
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


viewWeekHeadingItems :
    WithMdc msg (WithSession (TSTime.WithTimeDetails a))
    ->
        { b
            | onMdc : Material.Msg msg -> msg
            , moveWeekForward : msg
            , moveWeekBackward : msg
        }
    -> Html msg
viewWeekHeadingItems model updates =
    case model.timeDetails of
        WithTime { currentDay, weekOffset } ->
            styled div
                [ css "margin-left" "12px"
                , css "display" "flex"
                , css "align-items" "center"
                ]
                [ viewWeekControls model updates
                , styled div [ css "width" "12px" ] []
                , viewMonthHeading model currentDay weekOffset
                ]

        WithoutTime ->
            text ""


viewWeekControls :
    WithMdc msg a
    ->
        { b
            | onMdc : Material.Msg msg -> msg
            , moveWeekForward : msg
            , moveWeekBackward : msg
        }
    -> Html msg
viewWeekControls model { onMdc, moveWeekForward, moveWeekBackward } =
    styled div
        [ css "display" "flex" ]
        [ IconButton.view onMdc
            "back-one-week-button"
            model.mdc
            [ IconButton.icon1 "chevron_left"
            , IconButton.label1 "Go back one week"
            , Options.onClick moveWeekBackward
            ]
            []
        , IconButton.view onMdc
            "forward-one-week-button"
            model.mdc
            [ IconButton.icon1 "chevron_right"
            , IconButton.label1 "Go forward one week"
            , Options.onClick moveWeekForward
            ]
            []
        ]


viewMonthHeading : WithSession a -> Posix -> Int -> Html msg
viewMonthHeading model currentDay weekOffset =
    let
        daysInThatWeek =
            TSTime.getDaysInThatWeek model currentDay weekOffset
    in
    case daysInThatWeek of
        [ firstDay, _, _, _, _, _, lastDay ] ->
            let
                timeZone =
                    Session.getZone model.session

                firstDayMonth =
                    Time.toMonth timeZone firstDay

                firstDayYear =
                    Time.toYear timeZone firstDay

                lastDayMonth =
                    Time.toMonth timeZone lastDay

                lastDayYear =
                    Time.toYear timeZone lastDay

                headingText =
                    if firstDayMonth /= lastDayMonth then
                        if firstDayYear /= lastDayYear then
                            TSTime.monthToShortString firstDayMonth
                                ++ " "
                                ++ String.fromInt firstDayYear
                                ++ " - "
                                ++ TSTime.monthToShortString lastDayMonth
                                ++ " "
                                ++ String.fromInt lastDayYear

                        else
                            TSTime.monthToShortString firstDayMonth
                                ++ " - "
                                ++ TSTime.monthToShortString lastDayMonth
                                ++ " "
                                ++ String.fromInt lastDayYear

                    else
                        TSTime.monthToLongString firstDayMonth
                            ++ " "
                            ++ String.fromInt firstDayYear
            in
            styled Html.h4
                [ Typography.headline5
                ]
                [ text headingText ]

        _ ->
            text ""



-- Day Headings


viewDayHeadings : WithSession (TSTime.WithTimeDetails a) -> Html msg
viewDayHeadings model =
    let
        dayHeadings =
            case model.timeDetails of
                WithTime { currentDay, weekOffset } ->
                    List.map
                        (viewDayHeading << viewDayHeadingCopyForDate model currentDay)
                        (TSTime.getDaysInThatWeek model currentDay weekOffset)

                WithoutTime ->
                    List.map
                        (viewDayHeading << viewDayHeadingCopyForWeekday)
                        TSTime.weekdayStrings
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
            :: dayHeadings
        )


viewDayHeading : Html msg -> Html msg
viewDayHeading dayHeadingCopy =
    styled div
        [ css "flex-grow" "1"
        , css "flex-basis" "100%"
        , css "display" "flex"
        , css "justify-content" "flex-end"
        , css "position" "relative"
        ]
        [ dayHeadingCopy
        , styled div
            [ css "height" "30%"
            , css "width" "0"
            , css "border-right" "1px solid #829AB1"
            , css "position" "absolute"
            , css "bottom" "0"
            ]
            []
        ]


viewDayHeadingCopyForDate : WithSession a -> Posix -> Posix -> Html msg
viewDayHeadingCopyForDate model currentDay date =
    let
        timeZone =
            Session.getZone model.session

        isCurrentDay =
            date == currentDay
    in
    styled div
        [ css "margin" "auto" ]
        [ styled Html.p
            [ css "text-align" "center"
            , css "margin-top" "0"
            , css "margin-bottom" "0"
            , Typography.caption
            , when isCurrentDay <| css "color" "#2680C2"
            ]
            [ text <|
                TSTime.weekdayToString <|
                    Time.toWeekday timeZone date
            ]
        , if isCurrentDay then
            styled div
                [ css "height" "44px"
                , css "width" "44px"
                , css "border-radius" "100%"
                , css "background-color" "#2680C2"
                , css "color" "white"
                , css "display" "flex"
                , css "justify-content" "center"
                , css "align-items" "center"
                , Typography.headline5
                ]
                [ (text << String.fromInt) <|
                    Time.toDay timeZone date
                ]

          else
            styled Html.p
                [ css "text-align" "center"
                , css "margin-top" "8px"
                , Typography.headline5
                ]
                [ (text << String.fromInt) <|
                    Time.toDay timeZone date
                ]
        ]


viewDayHeadingCopyForWeekday : String -> Html msg
viewDayHeadingCopyForWeekday dayAbbreviation =
    styled Html.p
        [ Typography.headline6, css "margin" "auto" ]
        [ text dayAbbreviation ]


viewTimeSlotTimes : Html msg
viewTimeSlotTimes =
    styled div
        [ css "width" "70px"
        , css "border-right" "1px solid #829AB1"
        ]
        (styled div [ css "height" "32px" ] []
            :: List.map viewTimeSlotTime (List.range 1 (TS.defaultNumSlots // 4 - 1))
        )



-- Time Slots


viewScrollableTimeSlots :
    TS.WithLoadingAll (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a))
    ->
        Calendar
            { b
                | handleTimeSlotMouseMove : TS.PointerPosition -> msg
                , startSelectingTimeSlot : TS.DayNum -> TS.SlotNum -> msg
                , editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg
                , handleTimeSlotMouseUp : msg
            }
            c
    -> Html msg
viewScrollableTimeSlots model updates =
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
        [ if TS.isLoading model then
            viewLoader

          else
            text ""
        , styled div
            [ css "display" "flex"
            , css "overflow" "hidden"
            , when isSelectingTimeSlots <| css "cursor" "move"
            , case updates of
                WeeklyFreeTimes { handleTimeSlotMouseUp } ->
                    when isSelectingTimeSlots <| Options.onMouseUp handleTimeSlotMouseUp

                Events _ ->
                    nop
            ]
            (viewTimeSlotTimes
                :: List.map
                    (viewSingleDayTimeSlots model updates)
                    TS.dayNumRange
            )
        ]


viewTimeSlotTime : Int -> Html msg
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


viewSingleDayTimeSlots :
    TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection a)
    ->
        Calendar
            { b
                | handleTimeSlotMouseMove : TS.PointerPosition -> msg
                , startSelectingTimeSlot : TS.DayNum -> TS.SlotNum -> msg
                , editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg
            }
            c
    -> Int
    -> Html msg
viewSingleDayTimeSlots model updates dayNum =
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
        [ css "flex-grow" "1"
        , css "position" "relative"
        , case updates of
            WeeklyFreeTimes { handleTimeSlotMouseMove } ->
                when isSelectingTimeSlots <|
                    onTimeSlotMouseMove handleTimeSlotMouseMove

            Events _ ->
                nop
        ]
        (List.append
            [ div
                []
                (List.map
                    (viewTimeSlot updates dayNum)
                    TS.slotNumRange
                )
            , viewCurrentlySelectingTimeSlot model dayNum
            ]
            (List.map
                (viewSelectedTimeSlot updates)
                selectedTimeSlotsForThisDay
            )
        )


viewTimeSlot :
    Calendar { a | startSelectingTimeSlot : TS.DayNum -> TS.SlotNum -> msg } b
    -> Int
    -> Int
    -> Html msg
viewTimeSlot updates dayNum slotNum =
    styled div
        [ css "border-right" "1px solid #829AB1"
        , when (modBy 4 slotNum == 3) (css "border-bottom" "1px solid #829AB1")
        , css "height" "16px"
        , case updates of
            WeeklyFreeTimes { startSelectingTimeSlot } ->
                Options.onMouseDown (startSelectingTimeSlot dayNum slotNum)

            Events _ ->
                nop
        , Options.id (TS.getTimeSlotId dayNum slotNum)
        ]
        []


viewSelectedTimeSlot :
    Calendar { a | editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg } b
    -> TS.SelectedTimeSlotDetails
    -> Html msg
viewSelectedTimeSlot updates selectedTimeSlotDetails =
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
        , case updates of
            WeeklyFreeTimes { editTimeSlotSelection } ->
                Options.onClick <| editTimeSlotSelection selectedTimeSlotDetails

            Events _ ->
                nop
        ]
        [ viewTimeSlotDuration selectedTimeSlot ]


viewTimeSlotDuration : TS.WithTimeSlot a -> Html msg
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


viewCurrentlySelectingTimeSlot :
    TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots a)
    -> TS.DayNum
    -> Html msg
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
    -> TS.WithTimeSlot b
    -> TS.DayNum
    -> Html msg
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


getCardDimensions : TS.WithTimeSlot a -> CardDimensions
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


viewLoader : Html msg
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
