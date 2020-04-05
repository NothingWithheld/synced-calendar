module EventCreation.EventCreation exposing
    ( EventCreation(..)
    , EventCreationDetails
    , EventCreationPosition
    , WithEventCreation
    , eventDetailsPromptWidth
    )


type EventCreation
    = NotCreating
    | CurrentlyCreatingEvent EventCreationDetails EventCreationPosition


type alias WithEventCreation a =
    { a | eventCreation : EventCreation }


type alias EventCreationDetails =
    { title : String
    , description : String
    }


type alias EventCreationPosition =
    { x : Float
    , y : Float
    }


eventDetailsPromptWidth : Float
eventDetailsPromptWidth =
    300
