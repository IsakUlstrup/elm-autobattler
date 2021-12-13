module Entity exposing (Entity, EntityState, moveState, new, toString)

import HexEngine.Point exposing (Point)
import Util


type EntityState
    = Idle
    | Moving Point


type alias Entity =
    { player : Bool
    , health : ( Int, Int )
    , state : EntityState
    }


new : Bool -> Entity
new player =
    Entity player ( 100, 100 ) Idle


moveState : Point -> Entity -> Entity
moveState to entity =
    { entity | state = Moving to }


toString : Entity -> String
toString entity =
    let
        stateString =
            case entity.state of
                Idle ->
                    "Idle"

                Moving p ->
                    "Moving to: " ++ HexEngine.Point.toString p
    in
    "player: " ++ Util.boolToString entity.player ++ ", state: " ++ stateString
