module Entity exposing (Entity, EntityState, moveState, new, render, toString)

import HexEngine.Grid as Grid exposing (HexGrid)
import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render exposing (HexOrientation)
import Svg exposing (Svg)
import Svg.Attributes
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
                    "Moving to: " ++ Point.toString p
    in
    "player: " ++ Util.boolToString entity.player ++ ", state: " ++ stateString


render : HexGrid Entity -> HexOrientation -> ( Point, Entity ) -> Svg msg
render _ _ ( point, entity ) =
    let
        svgText =
            if entity.player then
                "🐼"

            else
                "🦄"

        stateText =
            case entity.state of
                Idle ->
                    "💤"

                Moving _ ->
                    "🐾"

        stateStroke =
            if entity.player then
                "cyan"

            else
                "magenta"
    in
    Svg.g []
        [ Render.text svgText
            [ Svg.Attributes.fontSize "55pt"
            , Svg.Attributes.pointerEvents "none"
            ]
        , Svg.g [ Svg.Attributes.transform "translate(25, -25)", Svg.Attributes.opacity "0.8" ]
            [ Render.circle 15
                [ Svg.Attributes.fill "white"
                , Svg.Attributes.stroke stateStroke
                , Svg.Attributes.strokeWidth "3"
                ]
            , Render.text stateText
                [ Svg.Attributes.fontSize "12pt"
                , Svg.Attributes.pointerEvents "none"
                ]
            ]
        ]
