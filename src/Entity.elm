module Entity exposing (Entity, EntityState, new)


type EntityState
    = Idle


type alias Entity =
    { player : Bool
    , health : ( Int, Int )
    , state : EntityState
    }


new : Bool -> Entity
new player =
    Entity player ( 100, 100 ) Idle
