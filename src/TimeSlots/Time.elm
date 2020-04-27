module TimeSlots.Time exposing
    ( TimeDetails(..)
    , WithTimeDetails
    , getDaysInThatWeek
    , monthToLongString
    , monthToShortString
    , weekdayStrings
    , weekdayToString
    , weekdaysInOrder
    )

import Session exposing (WithSession)
import Time exposing (Month(..), Posix, Weekday(..))


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
