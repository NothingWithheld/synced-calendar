module EventCreation.EventCreation exposing
    ( ConfirmedEventDetails
    , EventCreation(..)
    , EventCreationPosition
    , EventDetails(..)
    , WithDiscardConfirmationModal
    , WithEventCreation
    , areEventCreationsEqual
    , eventDetailsPromptWidth
    , getDateFromDetails
    , isConfirmedEvent
    )

import Date exposing (Date)


type EventCreation
    = NotCreating
    | CurrentlyCreatingEvent EventDetails EventCreationPosition


type alias WithEventCreation a =
    { a | eventCreation : EventCreation }


type EventDetails
    = UnsetWeeklyFreeTime
    | SetWeeklyFreeTime Int -- weekly free time slot ID
    | AvailableTime Date
    | UnsetConfirmedEvent ConfirmedEventDetails
    | ConfirmedEvent ConfirmedEventDetails


type alias ConfirmedEventDetails =
    { eventId : Int
    , recipientIds : List String
    , creatorId : String
    , title : String
    , description : String
    , date : Date
    }


type alias EventCreationPosition =
    { x : Float
    , y : Float
    }


type alias WithDiscardConfirmationModal a =
    { a | isDiscardConfirmationModalOpen : Bool }


areEventCreationsEqual : EventDetails -> EventDetails -> Bool
areEventCreationsEqual eventDetailsA eventDetailsB =
    case ( eventDetailsA, eventDetailsB ) of
        ( UnsetWeeklyFreeTime, UnsetWeeklyFreeTime ) ->
            True

        ( SetWeeklyFreeTime eventA, SetWeeklyFreeTime eventB ) ->
            eventA == eventB

        ( ConfirmedEvent eventA, ConfirmedEvent eventB ) ->
            eventA == eventB

        ( _, _ ) ->
            False


eventDetailsPromptWidth : Float
eventDetailsPromptWidth =
    320


getDateFromDetails : EventDetails -> Maybe Date
getDateFromDetails eventDetails =
    case eventDetails of
        UnsetWeeklyFreeTime ->
            Nothing

        SetWeeklyFreeTime _ ->
            Nothing

        AvailableTime date ->
            Just date

        UnsetConfirmedEvent { date } ->
            Just date

        ConfirmedEvent { date } ->
            Just date


isConfirmedEvent : EventDetails -> Bool
isConfirmedEvent eventDetails =
    case eventDetails of
        ConfirmedEvent _ ->
            True

        UnsetConfirmedEvent _ ->
            True

        _ ->
            False
