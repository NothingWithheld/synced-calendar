module Session exposing
    ( Session
    , WithSession
    , getKey
    , getOffset
    , getUserId
    , hasUserId
    , init
    , setUserId
    , signOut
    )

import Browser.Navigation as Nav
import Json.Decode as Decode


type Session
    = Session Nav.Key Int String


type alias WithSession a =
    { a
        | session : Session
    }


init : Nav.Key -> Decode.Value -> Session
init key offsetFlag =
    case Decode.decodeValue Decode.int offsetFlag of
        Ok offset ->
            let
                hourOffset =
                    offset * -1 // 60
            in
            -- limit to PST through EST
            if hourOffset >= -8 && hourOffset <= -5 then
                Session key hourOffset ""

            else
                Session key -6 ""

        Err _ ->
            -- default to CST
            Session key -6 ""


signOut : Session -> Session
signOut session =
    Session (getKey session) (getOffset session) ""


getUserId : Session -> String
getUserId (Session _ _ userId) =
    userId


setUserId : Session -> String -> Session
setUserId (Session key offset _) userId =
    Session key offset userId


getKey : Session -> Nav.Key
getKey (Session key _ _) =
    key


getOffset : Session -> Int
getOffset (Session _ offset _) =
    offset


hasUserId : Session -> Bool
hasUserId (Session _ _ userId) =
    userId /= ""
