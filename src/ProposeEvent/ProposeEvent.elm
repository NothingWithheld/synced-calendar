module ProposeEvent.ProposeEvent exposing (ProposedEvent, WithProposedEvent)

import Date exposing (Date)


type alias ProposedEvent =
    { title : String
    , description : String
    , creatorId : String
    , eventId : Int
    , recipientIds : List String
    , fromDate : Date
    , toDate : Date
    }


type alias WithProposedEvent a =
    { a
        | proposedEvent : Maybe ProposedEvent
        , alreadySubmittedAvailability : Bool
    }
