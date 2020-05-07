module TimeSlots.View exposing (viewCalendarHeading, viewDayHeadings, viewScrollableTimeSlots)

import AvailableTime.AvailableTime as AT
import Constants
import Date
import EventCreation.EventCreation as EC
import Html exposing (Html, div, text)
import Html.Entity as Entity
import Json.Decode as Decode exposing (field, float)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.IconButton as IconButton
import Material.Menu
import Material.Options as Options exposing (css, nop, styled, when)
import Material.Select as Select
import Material.Typography as Typography
import ProposeEvent.ProposeEvent as PE
import Route
import Session exposing (WithSession)
import Time exposing (Posix)
import TimeSlots.Time as TSTime
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
    WithMdc msg (WithSession (TS.WithLoadingAll (TSTime.WithTimeDetails (PE.WithAlreadySubmittedAvailability (TS.WithSelectedTimeSlots (AT.WithAvailableTimesCount (TS.WithLoadingAvailableTimesCount a)))))))
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
            { d
                | onMdc : Material.Msg msg -> msg
                , onTimeZoneSelect : String -> msg
                , moveWeekForward : msg
                , moveWeekBackward : msg
                , submitAvailability : msg
            }
            { e
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

        SubmitAvailability ({ onMdc } as updates_) ->
            styled div
                [ css "height" "10vh"
                , css "display" "flex"
                , css "padding-left" "12px"
                , css "padding-right" "12px"
                , css "justify-content" "space-between"
                , css "align-items" "center"
                ]
                [ styled div
                    [ css "display" "flex", css "align-items" "center" ]
                    [ Route.viewHomeButton model onMdc
                    , styled div [ css "width" "12px" ] []
                    , viewTimeZoneSelect model updates_
                    , viewWeekHeadingItems model updates_
                    ]
                , viewSubmitAvailabilityButton model updates_
                ]

        CreateEvent ({ onMdc } as updates_) ->
            styled div
                [ css "height" "10vh"
                , css "display" "flex"
                , css "padding-left" "12px"
                , css "padding-right" "12px"
                , css "justify-content" "space-between"
                , css "align-items" "center"
                ]
                [ styled div
                    [ css "display" "flex", css "align-items" "center" ]
                    [ Route.viewHomeButton model onMdc
                    , styled div [ css "width" "12px" ] []
                    , viewTimeZoneSelect model updates_
                    , viewWeekHeadingItems model updates_
                    ]
                , viewAvailabilityCountHeading model
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
        Just { currentDay, weekOffset } ->
            styled div
                [ css "margin-left" "12px"
                , css "display" "flex"
                , css "align-items" "center"
                ]
                [ viewWeekControls model updates
                , styled div [ css "width" "12px" ] []
                , viewMonthHeading model currentDay weekOffset
                ]

        Nothing ->
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


viewSubmitAvailabilityButton :
    WithMdc msg (PE.WithAlreadySubmittedAvailability (TS.WithSelectedTimeSlots a))
    ->
        { b
            | onMdc : Material.Msg msg -> msg
            , submitAvailability : msg
        }
    -> Html msg
viewSubmitAvailabilityButton model { onMdc, submitAvailability } =
    let
        isAvailableTS eventDetails =
            case eventDetails of
                EC.AvailableTime _ ->
                    True

                _ ->
                    False

        availableTimeSlots =
            List.filter
                (isAvailableTS << TS.getEventDetailsFromDetails)
                model.selectedTimeSlots

        isDisabled =
            (List.length availableTimeSlots == 0)
                || model.alreadySubmittedAvailability
    in
    styled div
        []
        [ Button.view onMdc
            "submit-availability-button"
            model.mdc
            [ Button.ripple
            , Button.unelevated
            , when isDisabled Button.disabled
            , Options.onClick submitAvailability
            ]
            [ text "Submit Availability" ]
        ]


viewAvailabilityCountHeading : AT.WithAvailableTimesCount (TS.WithLoadingAvailableTimesCount a) -> Html msg
viewAvailabilityCountHeading model =
    let
        headingText =
            String.fromInt model.countSubmitted
                ++ " out of "
                ++ String.fromInt model.totalRecipients
                ++ " have submitted their availability"
    in
    if model.loadingAvailableTimesCount then
        text ""

    else
        styled Html.h4
            [ Typography.headline6
            ]
            [ text headingText ]



-- Day Headings


viewDayHeadings : WithSession (TSTime.WithTimeDetails a) -> Html msg
viewDayHeadings model =
    let
        dayHeadings =
            case model.timeDetails of
                Just { currentDay, weekOffset } ->
                    List.map
                        (viewDayHeading << viewDayHeadingCopyForDate model currentDay)
                        (TSTime.getDaysInThatWeek model currentDay weekOffset)

                Nothing ->
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
    AT.WithAvailabilityMap (AT.WithAvailableTimesCount (PE.WithAlreadySubmittedAvailability (TS.WithLoadingAll (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection (TSTime.WithTimeDetails (WithSession (EC.WithEventCreation (PE.WithProposedEvent a)))))))))
    ->
        Calendar
            { b
                | handleTimeSlotMouseMove : TS.PointerPosition -> msg
                , startSelectingTimeSlot : TS.DayNum -> TS.SlotNum -> msg
                , editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg
                , handleTimeSlotMouseUp : msg
            }
            { c
                | editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg
            }
            { d
                | handleTimeSlotMouseMove : TS.PointerPosition -> msg
                , startSelectingTimeSlot : TS.DayNum -> TS.SlotNum -> msg
                , editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg
                , handleTimeSlotMouseUp : msg
            }
            e
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

        thatWeekDates =
            case model.timeDetails of
                Just { currentDay, weekOffset } ->
                    List.map Just <| TSTime.getDaysInThatWeek model currentDay weekOffset

                Nothing ->
                    List.repeat 7 Nothing
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

                SubmitAvailability { handleTimeSlotMouseUp } ->
                    when (isSelectingTimeSlots && not model.alreadySubmittedAvailability) <|
                        Options.onMouseUp handleTimeSlotMouseUp

                CreateEvent _ ->
                    nop
            ]
            (viewTimeSlotTimes
                :: List.map
                    (viewSingleDayTimeSlots model updates)
                    (List.map2 Tuple.pair TS.dayNumRange thatWeekDates)
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
    AT.WithAvailabilityMap (AT.WithAvailableTimesCount (PE.WithAlreadySubmittedAvailability (TS.WithSelectedTimeSlots (TS.WithTimeSlotSelection (WithSession (EC.WithEventCreation (PE.WithProposedEvent (TSTime.WithTimeDetails a))))))))
    ->
        Calendar
            { b
                | handleTimeSlotMouseMove : TS.PointerPosition -> msg
                , startSelectingTimeSlot : TS.DayNum -> TS.SlotNum -> msg
                , editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg
            }
            { c
                | editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg
            }
            { d
                | handleTimeSlotMouseMove : TS.PointerPosition -> msg
                , startSelectingTimeSlot : TS.DayNum -> TS.SlotNum -> msg
                , editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg
            }
            e
    -> ( TS.DayNum, Maybe Posix )
    -> Html msg
viewSingleDayTimeSlots model updates ( dayNum, maybeDate ) =
    let
        selectedTimeSlotsForThisDayNum =
            List.filter ((\timeSlot -> timeSlot.dayNum == dayNum) << TS.getTimeSlotFromDetails) model.selectedTimeSlots

        selectedTimeSlotsForThisDay =
            Maybe.withDefault selectedTimeSlotsForThisDayNum <|
                Maybe.map
                    (\date ->
                        List.filter
                            ((\eventDetails ->
                                Maybe.withDefault False
                                    (Maybe.map (TSTime.isSameDay model date)
                                        (EC.getDateFromDetails eventDetails)
                                    )
                             )
                                << TS.getEventDetailsFromDetails
                            )
                            selectedTimeSlotsForThisDayNum
                    )
                    maybeDate

        isSelectingTimeSlots =
            case model.timeSlotSelection of
                TS.InitialPressNoMove _ ->
                    True

                TS.CurrentlySelecting _ ->
                    True

                _ ->
                    False

        isOutsideEventRange =
            case ( model.proposedEvent, maybeDate ) of
                ( Just { fromDate, toDate }, Just date ) ->
                    not <|
                        Date.isBetween fromDate toDate <|
                            Date.fromPosix (Session.getZone model.session) date

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

            SubmitAvailability { handleTimeSlotMouseMove } ->
                when (isSelectingTimeSlots && not model.alreadySubmittedAvailability) <|
                    onTimeSlotMouseMove handleTimeSlotMouseMove

            CreateEvent _ ->
                nop
        ]
        ([ div
            []
            (List.map
                (viewTimeSlot model updates isOutsideEventRange maybeDate dayNum)
                TS.slotNumRange
            )
         , viewCurrentlySelectingTimeSlot model dayNum
         ]
            ++ List.map
                (viewSelectedTimeSlot model updates)
                selectedTimeSlotsForThisDay
        )


viewTimeSlot :
    WithSession (AT.WithAvailabilityMap (AT.WithAvailableTimesCount (PE.WithAlreadySubmittedAvailability a)))
    ->
        Calendar { b | startSelectingTimeSlot : TS.DayNum -> TS.SlotNum -> msg }
            c
            { d
                | startSelectingTimeSlot : TS.DayNum -> TS.SlotNum -> msg
            }
            e
    -> Bool
    -> Maybe Posix
    -> Int
    -> Int
    -> Html msg
viewTimeSlot model updates isOutsideEventRange maybeDay dayNum slotNum =
    let
        isAvailable =
            case ( updates, maybeDay ) of
                ( CreateEvent _, Just day ) ->
                    let
                        isSlotAvailable =
                            AT.isSlotAvailable model
                                (Date.fromPosix (Session.getZone model.session) day)
                                slotNum
                    in
                    model.countSubmitted == model.totalRecipients && isSlotAvailable

                _ ->
                    False
    in
    styled div
        [ css "border-right" "1px solid #829AB1"
        , if isOutsideEventRange then
            css "background-color" Constants.disabledColor

          else if isAvailable then
            css "background-color" Constants.availableColor

          else
            nop
        , when (modBy 4 slotNum == 3) (css "border-bottom" "1px solid #829AB1")
        , css "height" "16px"
        , case updates of
            WeeklyFreeTimes { startSelectingTimeSlot } ->
                Options.onMouseDown (startSelectingTimeSlot dayNum slotNum)

            Events _ ->
                nop

            SubmitAvailability { startSelectingTimeSlot } ->
                when (not isOutsideEventRange && not model.alreadySubmittedAvailability) <|
                    Options.onMouseDown (startSelectingTimeSlot dayNum slotNum)

            CreateEvent _ ->
                nop
        , Options.id (TS.getTimeSlotId dayNum slotNum)
        ]
        []


viewSelectedTimeSlot :
    PE.WithAlreadySubmittedAvailability a
    ->
        Calendar { b | editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg }
            { c
                | editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg
            }
            { d
                | editTimeSlotSelection : TS.SelectedTimeSlotDetails -> msg
            }
            e
    -> TS.SelectedTimeSlotDetails
    -> Html msg
viewSelectedTimeSlot model updates selectedTimeSlotDetails =
    let
        (TS.SelectedTimeSlotDetails selectedTimeSlot eventDetails) =
            selectedTimeSlotDetails

        cardDimensions =
            getCardDimensions selectedTimeSlot

        isConfirmedEvent =
            EC.isConfirmedEvent eventDetails

        hasSingleSlotHeight =
            TS.hasSingleSlotHeight selectedTimeSlot
    in
    Card.view
        [ css "background-color"
            (if isConfirmedEvent then
                Constants.confirmedEventColor

             else
                Constants.setTimeSlotColor
            )
        , css "top" (String.fromFloat cardDimensions.y ++ "px")
        , css "height" (String.fromFloat (cardDimensions.height - 4) ++ "px")
        , css "position" "absolute"
        , css "width" "95%"
        , css "border-radius" "8px"
        , css "user-select" "none"
        , case updates of
            WeeklyFreeTimes { editTimeSlotSelection } ->
                Options.onClick <| editTimeSlotSelection selectedTimeSlotDetails

            Events { editTimeSlotSelection } ->
                Options.onClick <| editTimeSlotSelection selectedTimeSlotDetails

            SubmitAvailability { editTimeSlotSelection } ->
                when (not model.alreadySubmittedAvailability) <|
                    Options.onClick <|
                        editTimeSlotSelection selectedTimeSlotDetails

            CreateEvent _ ->
                nop
        ]
        [ viewTimeSlotDuration selectedTimeSlot hasSingleSlotHeight ]


viewTimeSlotDuration : TS.TimeSlot -> Bool -> Html msg
viewTimeSlotDuration { startBound, endBound } hasSingleSlotHeight =
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
        , css "margin-left" "8px"
        , css "margin-top"
            (if hasSingleSlotHeight then
                "0"

             else
                "2px"
            )
        , when hasSingleSlotHeight <| css "line-height" "1rem"
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
    TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots (EC.WithEventCreation (PE.WithProposedEvent (TSTime.WithTimeDetails (WithSession a)))))
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
    TS.WithTimeSlotSelection (TS.WithSelectedTimeSlots (EC.WithEventCreation (PE.WithProposedEvent (TSTime.WithTimeDetails (WithSession a)))))
    -> TS.TimeSlot
    -> TS.DayNum
    -> Html msg
viewUserChangingTimeSlot model timeSlotSelection dayNum =
    let
        cardDimensions =
            getCardDimensions timeSlotSelection

        orderedTimeSlotSelection =
            TS.getOrderedTimeSlot timeSlotSelection

        hasSingleSlotHeight =
            TS.hasSingleSlotHeight orderedTimeSlotSelection

        dayNumCurrentlySelected =
            timeSlotSelection.dayNum

        selectedTimeSlotsThatWeek =
            TSTime.getSelectedTimeSlotsInThatWeek model

        intersectsTimeSlots =
            TS.doesTSSelectionIntersectSelectedTimeSlots
                selectedTimeSlotsThatWeek
                model.timeSlotSelection

        invalidSelection =
            case model.eventCreation of
                EC.CurrentlyCreatingEvent eventDetails _ ->
                    case ( eventDetails, model.proposedEvent ) of
                        ( EC.AvailableTime date, Just { fromDate, toDate } ) ->
                            (not <| Date.isBetween fromDate toDate date)
                                || intersectsTimeSlots

                        ( EC.AvailableTime _, Nothing ) ->
                            True

                        _ ->
                            intersectsTimeSlots

                _ ->
                    intersectsTimeSlots
    in
    if dayNum == dayNumCurrentlySelected then
        Card.view
            [ if invalidSelection then
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
            [ viewTimeSlotDuration orderedTimeSlotSelection hasSingleSlotHeight
            ]

    else
        text ""


getCardDimensions : TS.TimeSlot -> CardDimensions
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
