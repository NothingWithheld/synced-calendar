module EventCreation exposing (EventCreationDetails, EventCreationPosition, UserEventCreation(..), WithUserEventCreation, eventDetailsPromptWidth)


type UserEventCreation
    = NotCreating
    | CurrentlyCreatingEvent EventCreationDetails EventCreationPosition


type alias EventCreationDetails =
    { title : String
    , description : String
    }


type alias EventCreationPosition =
    { x : Float
    , y : Float
    }


type alias WithUserEventCreation a =
    { a | userEventCreation : UserEventCreation }


eventDetailsPromptWidth : Float
eventDetailsPromptWidth =
    300
