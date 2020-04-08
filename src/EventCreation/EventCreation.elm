module EventCreation.EventCreation exposing
    ( EventCreation(..)
    , EventCreationDetails(..)
    , EventCreationPosition
    , EventItems
    , WithEventCreation
    , eventDetailsPromptWidth
    )


type EventCreation
    = NotCreating
    | CurrentlyCreatingEvent EventCreationDetails EventCreationPosition


type alias WithEventCreation a =
    { a | eventCreation : EventCreation }


type EventCreationDetails
    = WeeklyFreeTimes
    | EventDetails EventItems


type alias EventItems =
    { title : String
    , description : String
    }


type alias EventCreationPosition =
    { x : Float
    , y : Float
    }


eventDetailsPromptWidth : Float
eventDetailsPromptWidth =
    320
