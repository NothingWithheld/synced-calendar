module TimeSlots.Time exposing
    ( TimeDetails(..)
    , WithTimeDetails
    , dateToDayNum
    , dateToString
    , getDaysInThatWeek
    , isSameDay
    , isoStringToDate
    , monthToLongString
    , monthToShortString
    , stringToDate
    , weekdayStrings
    , weekdayToString
    , weekdaysInOrder
    )

import Date exposing (Date)
import Session exposing (WithSession)
import Time exposing (Month(..), Posix, Weekday(..))
import TimeSlots.TimeSlots as TS
import Utils exposing (getListItemAt)


type TimeDetails
    = WithTime
        { currentDay : Posix
        , weekOffset : Int
        }
    | WithoutTime


type alias WithTimeDetails a =
    { a
        | timeDetails : TimeDetails
    }


getDaysInThatWeek : WithSession a -> Posix -> Int -> List Posix
getDaysInThatWeek model currentDay weeksOffset =
    let
        weekdayNumOfNow =
            getWeekdayNum <| Time.toWeekday (Session.getZone model.session) currentDay

        thisDayMillisWithWeekOffset =
            Time.posixToMillis currentDay + millisInWeek * weeksOffset

        mapFunc weekDayNum =
            Time.millisToPosix <|
                thisDayMillisWithWeekOffset
                    + (weekDayNum - weekdayNumOfNow)
                    * millisInDay
    in
    List.map mapFunc <| List.range 0 6


millisInWeek : Int
millisInWeek =
    millisInDay * 7


millisInDay : Int
millisInDay =
    1000 * 60 * 60 * 24


weekdaysInOrder : List Weekday
weekdaysInOrder =
    [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]


monthsInOrder : List Month
monthsInOrder =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


weekdayToString : Weekday -> String
weekdayToString weekday =
    case weekday of
        Sun ->
            "SUN"

        Mon ->
            "MON"

        Tue ->
            "TUE"

        Wed ->
            "WED"

        Thu ->
            "THU"

        Fri ->
            "FRI"

        Sat ->
            "SAT"


monthToShortString : Month -> String
monthToShortString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


monthToLongString : Month -> String
monthToLongString month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


weekdayStrings : List String
weekdayStrings =
    [ "SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT" ]


getWeekdayNum : Weekday -> Int
getWeekdayNum weekday =
    case weekday of
        Sun ->
            0

        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6


monthNumToMonth : Int -> Maybe Month
monthNumToMonth monthNum =
    getListItemAt (monthNum - 1) monthsInOrder


isoStringToDate : String -> Maybe Date
isoStringToDate iso =
    case List.map String.toInt <| String.split "-" iso of
        [ Just year, Just monthNum, Just day ] ->
            Maybe.map
                (\month -> Date.fromCalendarDate year month day)
                (monthNumToMonth monthNum)

        _ ->
            Nothing


stringToDate : String -> Maybe Date
stringToDate iso =
    case List.map String.toInt <| String.split "-" iso of
        [ Just monthNum, Just day, Just year ] ->
            Maybe.map
                (\month -> Date.fromCalendarDate year month day)
                (monthNumToMonth monthNum)

        _ ->
            Nothing


dateToString : Date -> String
dateToString date =
    Date.format "MM-dd-y" date


dateToDayNum : Date -> TS.DayNum
dateToDayNum date =
    getWeekdayNum <| Date.weekday date


isSameDay : WithSession a -> Posix -> Date -> Bool
isSameDay model posixDate date =
    let
        timeZone =
            Session.getZone model.session

        posixYear =
            Time.toYear timeZone posixDate

        posixMonth =
            Time.toMonth timeZone posixDate

        posixDay =
            Time.toDay timeZone posixDate

        dateYear =
            Date.year date

        dateMonth =
            Date.month date

        dateDay =
            Date.day date
    in
    posixYear == dateYear && posixMonth == dateMonth && posixDay == dateDay
