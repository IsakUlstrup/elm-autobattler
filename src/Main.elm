module Main exposing (..)

import Browser
import Browser.Events
import Dict
import Entity exposing (Entity)
import HexEngine.Grid as Grid exposing (HexGrid)
import HexEngine.GridGenerator as GridGen exposing (MapGenerationConfig)
import HexEngine.Path
import HexEngine.Point exposing (Point)
import HexEngine.Render as Render exposing (HexAppearance, HexOrientation(..), RenderConfig)
import Html exposing (Html, div)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Maybe exposing (withDefault)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes exposing (orientation)
import Svg.Events


renderText : HexOrientation -> Point -> List (Svg Msg)
renderText _ _ =
    let
        svgText =
            "hei"
    in
    [ Render.text svgText
        [ Svg.Attributes.fontSize "48pt"
        , Svg.Attributes.pointerEvents "none"
        ]
    ]


renderDot : HexOrientation -> Point -> List (Svg Msg)
renderDot _ _ =
    [ Render.circle 10
        [ Svg.Attributes.fill "rgba(50, 50, 50, 0.7)"
        , Svg.Attributes.pointerEvents "none"
        ]
    ]


simpleHex : HexGrid TileData -> HexOrientation -> ( Point, TileData ) -> Svg Msg
simpleHex grid orientation ( point, tile ) =
    let
        strokeAttr =
            Render.hexStroke (Grid.neighborValues point grid)

        color t =
            case t of
                Grass ->
                    initColor 39 170 65

                Mountain ->
                    initColor 73 132 44

                Water ->
                    initColor 26 81 158

                ShallowWater ->
                    initColor 42 160 224

                Road ->
                    initColor 76 76 76

                Woods ->
                    initColor 29 160 55

                Sand ->
                    initColor 117 114 58
    in
    Render.hex orientation
        ([ Svg.Attributes.fill (color tile |> toCssString)
         , Svg.Events.onMouseOver (HoverHex point)
         , Svg.Events.onClick (ClickHex point)
         ]
            ++ strokeAttr
        )


hexIcon : HexGrid TileData -> HexOrientation -> ( Point, TileData ) -> Svg Msg
hexIcon _ _ ( point, tile ) =
    let
        svgText =
            case tile of
                Mountain ->
                    "⛰️"

                Woods ->
                    "🌲"

                _ ->
                    ""
    in
    Render.text svgText
        [ Svg.Attributes.fontSize "65pt"
        , Svg.Attributes.pointerEvents "none"
        , Svg.Attributes.y "0"
        ]


debugHex : HexGrid TileData -> HexOrientation -> ( Point, TileData ) -> Svg Msg
debugHex _ _ ( point, tile ) =
    Render.text (HexEngine.Point.toString point) []


highlightHex : HexGrid Highlight -> HexOrientation -> ( Point, Highlight ) -> Svg Msg
highlightHex _ orientation ( point, highlight ) =
    let
        shape h =
            case h of
                Active ->
                    Render.hex orientation
                        [ Svg.Attributes.fill "rgba(255, 100, 100, 0.6)"
                        , Svg.Attributes.pointerEvents "none"
                        ]

                Hover ->
                    Render.hex orientation
                        [ Svg.Attributes.fill "rgba(255, 255, 255, 0.4)"
                        , Svg.Attributes.pointerEvents "none"
                        ]

                Overlay ->
                    Render.hex orientation
                        [ Svg.Attributes.fill "rgba(255, 255, 255, 0.4)"
                        , Svg.Attributes.pointerEvents "none"
                        ]

                Fog ->
                    Render.hex orientation
                        [ Svg.Attributes.fill "rgba(0, 0, 0, 0.4)"
                        , Svg.Attributes.pointerEvents "none"
                        ]

                Dot ->
                    Render.circle 10
                        [ Svg.Attributes.fill "rgba(50, 50, 50, 0.8)"
                        , Svg.Attributes.pointerEvents "none"
                        ]
    in
    shape highlight


type Highlight
    = Active
    | Hover
    | Overlay
    | Dot
    | Fog


type TileData
    = Grass
    | Mountain
    | Water
    | ShallowWater
    | Road
    | Woods
    | Sand


tileToString : TileData -> String
tileToString tile =
    case tile of
        Grass ->
            "Grass"

        Mountain ->
            "Mountain"

        Water ->
            "Water"

        ShallowWater ->
            "Shallw Water"

        Road ->
            "Road"

        Woods ->
            "Woods"

        Sand ->
            "Sand"



---- MODEL ----


type Key
    = Character Char
    | Control String


type ActiveHex
    = Terrain TileData
    | Entity Entity


type InteractionMode
    = Highlight
    | Line
    | Raycast
    | Vision Int
    | Ring
    | Path


type alias Model =
    { renderConfig : Render.RenderConfig
    , hexAppearance : Render.HexAppearance
    , mapGenConfig : GridGen.MapGenerationConfig
    , grid : HexGrid TileData
    , entities : HexGrid Entity
    , activeHex : ( Point, ActiveHex )
    , hoverPoint : Point
    , interactionMode : InteractionMode
    , discoveredTiles : Set Point
    , visibleTiles : Set Point
    , showSidepanel : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        mapGenConfig =
            GridGen.initMapGenConfig |> GridGen.withRadius 30 |> GridGen.withScale 2
    in
    ( { renderConfig = Render.initRenderConfig 1000 1000 |> Render.withCameraZoom 0.6
      , hexAppearance = Render.initAppearance |> Render.withScale 0.98
      , interactionMode = Highlight
      , mapGenConfig = mapGenConfig
      , grid = GridGen.randomHexMap mapGenConfig tileType |> Dict.insert ( 0, 0, 0 ) Grass
      , entities =
            Dict.fromList
                [ ( ( 0, -1, 1 ), Entity.new True |> Entity.moveState ( 2, -2, 0 ) )
                , ( ( -1, 1, 0 ), Entity.new False )
                , ( ( -5, 5, 0 ), Entity.new False )
                , ( ( 5, -5, 0 ), Entity.new True )
                ]
      , activeHex = ( ( 0, 0, 0 ), Terrain Grass )
      , hoverPoint = ( 0, 0, 0 )
      , discoveredTiles = Set.singleton ( 0, 0, 0 )
      , visibleTiles = Set.singleton ( 0, 0, 0 )
      , showSidepanel = True
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetRenderConfig RenderConfig
    | SetHexAppearance HexAppearance
    | ClickHex Point
    | HoverHex Point
    | SetInteractionMode InteractionMode
    | SetMapGenConfig MapGenerationConfig
    | KeyInput Key


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRenderConfig config ->
            ( { model | renderConfig = config }, Cmd.none )

        SetHexAppearance app ->
            ( { model | hexAppearance = app }, Cmd.none )

        ClickHex point ->
            let
                hex p =
                    case Dict.get p model.entities of
                        Just e ->
                            Entity e

                        Nothing ->
                            case Dict.get p model.grid of
                                Just t ->
                                    Terrain t

                                Nothing ->
                                    Terrain Grass
            in
            ( { model | activeHex = ( point, hex point ) }, Cmd.none )

        HoverHex point ->
            case model.interactionMode of
                Vision range ->
                    let
                        visible =
                            Grid.fieldOfVisionWithCost range point (visionCost model.grid model.entities)
                    in
                    ( { model
                        | hoverPoint = point
                        , visibleTiles = visible
                        , discoveredTiles = Set.union model.discoveredTiles visible
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | hoverPoint = point }, Cmd.none )

        SetInteractionMode mode ->
            ( { model
                | interactionMode = mode
              }
            , Cmd.none
            )

        SetMapGenConfig config ->
            ( { model
                | mapGenConfig = config
                , grid = GridGen.randomHexMap config tileType |> Dict.insert ( 0, 0, 0 ) Grass
                , hoverPoint = ( 0, 0, 0 )
                , activeHex = ( ( 0, 0, 0 ), Terrain Grass )
                , visibleTiles = Set.singleton ( 0, 0, 0 )
                , discoveredTiles = Set.singleton ( 0, 0, 0 )
              }
            , Cmd.none
            )

        KeyInput key ->
            case key of
                Character c ->
                    let
                        movementSpeed =
                            40
                    in
                    case c of
                        'w' ->
                            ( { model | renderConfig = model.renderConfig |> Render.withCameraMovementY -movementSpeed }, Cmd.none )

                        's' ->
                            ( { model | renderConfig = model.renderConfig |> Render.withCameraMovementY movementSpeed }, Cmd.none )

                        'a' ->
                            ( { model | renderConfig = model.renderConfig |> Render.withCameraMovementX -movementSpeed }, Cmd.none )

                        'd' ->
                            ( { model | renderConfig = model.renderConfig |> Render.withCameraMovementX movementSpeed }, Cmd.none )

                        '+' ->
                            ( { model | renderConfig = model.renderConfig |> Render.withCameraZoomIn 0.1 }, Cmd.none )

                        '-' ->
                            ( { model | renderConfig = model.renderConfig |> Render.withCameraZoomOut 0.1 }, Cmd.none )

                        'p' ->
                            ( { model | showSidepanel = not model.showSidepanel }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



---- VIEW ----


type alias RGBAColor =
    { r : Int
    , g : Int
    , b : Int
    , a : Float
    }


initColor : Int -> Int -> Int -> RGBAColor
initColor r g b =
    RGBAColor r g b 1


withAlpha : Float -> RGBAColor -> RGBAColor
withAlpha alpha color =
    { color | a = alpha }


toCssString : RGBAColor -> String
toCssString { r, g, b, a } =
    "rgba(" ++ String.fromInt r ++ ", " ++ String.fromInt g ++ ", " ++ String.fromInt b ++ ", " ++ String.fromFloat a ++ ")"


tileType : Float -> Maybe TileData
tileType i =
    if i < -0.2 then
        Nothing

    else if i < 0 then
        Just Sand

    else if i < 0.4 then
        Just Grass

    else if i < 0.55 then
        Just Woods

    else
        Just Mountain


calculateCost : HexGrid TileData -> HexGrid Entity -> Point -> Maybe Float
calculateCost terrain entities point =
    let
        terrainCost =
            Dict.get point terrain
                |> Maybe.andThen
                    (\t ->
                        case t of
                            Grass ->
                                Just 3

                            Mountain ->
                                Nothing

                            Water ->
                                Nothing

                            ShallowWater ->
                                Just 6

                            Road ->
                                Just 0

                            Woods ->
                                Just 5

                            Sand ->
                                Just 4
                    )

        entityCost =
            case Dict.get point entities of
                Just _ ->
                    Nothing

                _ ->
                    Just 0
    in
    Maybe.map2 max entityCost terrainCost


visionCost : HexGrid TileData -> HexGrid Entity -> Point -> Maybe Int
visionCost terrain entities point =
    let
        terrainCost =
            case Dict.get point terrain of
                Just t ->
                    case t of
                        Mountain ->
                            Nothing

                        Woods ->
                            Just 1

                        _ ->
                            Just 0

                Nothing ->
                    Just 0

        entityCost =
            case Dict.get point entities of
                Just _ ->
                    Just 1

                _ ->
                    Just 0
    in
    Maybe.map2 max entityCost terrainCost


{-| obstacles that block vision
-}
obstacles : HexGrid TileData -> Set Point
obstacles grid =
    let
        isObstacle ( point, t ) =
            case t of
                Mountain ->
                    True

                _ ->
                    False
    in
    Grid.filter isObstacle grid |> List.map (\( p, _ ) -> p) |> Set.fromList


renderInteractionModeSelector : InteractionMode -> Html Msg
renderInteractionModeSelector mode =
    let
        radio : InteractionMode -> String -> List (Html Msg)
        radio m label =
            [ Html.input
                [ Html.Attributes.type_ "radio"
                , Html.Attributes.name "interactionMode"
                , Html.Attributes.id label
                , Html.Events.onClick (SetInteractionMode m)
                ]
                []
            , Html.label [ Html.Attributes.for label ] [ Html.text label ]
            , Html.br [] []
            ]

        options : InteractionMode -> List (Html Msg)
        options m =
            case m of
                Vision r ->
                    [ Html.input
                        [ Html.Attributes.type_ "range"
                        , Html.Attributes.max "10"
                        , Html.Attributes.min "2"
                        , Html.Attributes.value (String.fromInt r)
                        , Html.Events.onInput (\v -> SetInteractionMode (Vision (String.toInt v |> withDefault 0)))
                        ]
                        []
                    , Html.text (String.fromInt r)
                    ]

                _ ->
                    []
    in
    div []
        (radio Highlight "Hover"
            ++ radio Line "Line"
            ++ radio Raycast "Raycast"
            ++ radio Ring "Ring"
            ++ radio (Vision 4) "Vision"
            ++ radio Path "Path"
            ++ (Html.br [] []
                    :: options mode
               )
        )


renderActiveHex : ( Point, ActiveHex ) -> Html Msg
renderActiveHex ( point, hex ) =
    let
        hexElem =
            case hex of
                Terrain t ->
                    Html.text ("terrain: " ++ tileToString t)

                Entity e ->
                    Html.text ("entity: " ++ Entity.toString e)
    in
    div []
        [ Html.h4 [] [ Html.text "Active Hex" ]
        , Html.text (HexEngine.Point.toString point)
        , Html.br [] []
        , hexElem
        ]


sidePanel : Model -> Html Msg
sidePanel model =
    div [ Html.Attributes.class "controls" ]
        [ div []
            [ Html.h4 [] [ Html.text "Hex Count" ]
            , Html.text (Dict.size model.grid |> String.fromInt)
            ]
        , renderActiveHex model.activeHex
        , div []
            [ Html.h4 [] [ Html.text "Hover Hex" ]
            , Html.text (HexEngine.Point.toString model.hoverPoint)
            ]
        , Html.details []
            [ Html.summary [] [ Html.text "Camera" ]
            , Html.h4 [] [ Html.text "Zoom" ]
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0.06"
                , Html.Attributes.max "2.5"
                , Html.Attributes.step "0.1"
                , Html.Attributes.value <| String.fromFloat model.renderConfig.cameraZoom
                , Html.Events.onInput (\v -> SetRenderConfig (model.renderConfig |> Render.withCameraZoom (String.toFloat v |> withDefault 0)))
                ]
                []
            , Html.text (String.fromFloat model.renderConfig.cameraZoom)
            , div []
                [ Html.h4 [] [ Html.text "Pan X" ]
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min "-1000"
                    , Html.Attributes.max "1000"
                    , Html.Attributes.step "0.1"
                    , Html.Attributes.value <| String.fromFloat model.renderConfig.cameraX
                    , Html.Events.onInput (\v -> SetRenderConfig (model.renderConfig |> Render.withCameraPositionX (String.toFloat v |> withDefault 0)))
                    ]
                    []
                , Html.text (String.fromFloat model.renderConfig.cameraX)
                ]
            , div []
                [ Html.h4 [] [ Html.text "Pan Y" ]
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min "-1000"
                    , Html.Attributes.max "1000"
                    , Html.Attributes.step "0.1"
                    , Html.Attributes.value <| String.fromFloat model.renderConfig.cameraY
                    , Html.Events.onInput (\v -> SetRenderConfig (model.renderConfig |> Render.withCameraPositionY (String.toFloat v |> withDefault 0)))
                    ]
                    []
                , Html.text (String.fromFloat model.renderConfig.cameraY)
                ]
            ]
        , Html.details []
            [ Html.summary [] [ Html.text "Hex appearance" ]
            , Html.h4 [] [ Html.text "Hex orientation" ]
            , Html.button [ Html.Events.onClick (SetHexAppearance (model.hexAppearance |> Render.withOrientation Render.FlatTop)) ] [ Html.text "Flat" ]
            , Html.button [ Html.Events.onClick (SetHexAppearance (model.hexAppearance |> Render.withOrientation Render.PointyTop)) ] [ Html.text "Pointy" ]
            , Html.h4 [] [ Html.text "Hex Scale" ]
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "1"
                , Html.Attributes.step "0.01"
                , Html.Attributes.value <| String.fromFloat model.hexAppearance.scale
                , Html.Events.onInput (\v -> SetHexAppearance (model.hexAppearance |> Render.withScale (String.toFloat v |> withDefault 0)))
                ]
                []
            , Html.text (String.fromFloat model.hexAppearance.scale)
            ]
        , Html.details []
            [ Html.summary [] [ Html.text "Map generation" ]
            , Html.label [ Html.Attributes.for "map-gen-size" ] [ Html.text "size" ]
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.value (String.fromInt model.mapGenConfig.radius)
                , Html.Attributes.id "map-gen-size"
                , Html.Events.onInput (\v -> SetMapGenConfig (GridGen.withRadius (String.toInt v |> withDefault 0) model.mapGenConfig))
                ]
                []
            , Html.br [] []
            , Html.label [ Html.Attributes.for "map-gen-seed" ] [ Html.text "seed" ]
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.value (String.fromInt model.mapGenConfig.seed)
                , Html.Attributes.id "map-gen-seed"
                , Html.Events.onInput (\v -> SetMapGenConfig (model.mapGenConfig |> GridGen.withPermTable (String.toInt v |> withDefault 0) |> GridGen.withSeed (String.toInt v |> withDefault 0)))
                ]
                []
            , Html.br [] []
            , Html.label [ Html.Attributes.for "map-gen-scale" ] [ Html.text "scale" ]
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.value (String.fromFloat model.mapGenConfig.scale)
                , Html.Attributes.id "map-gen-scale"
                , Html.Events.onInput (\v -> SetMapGenConfig (GridGen.withScale (String.toFloat v |> withDefault 0) model.mapGenConfig))
                ]
                []
            , Html.br [] []
            , Html.label [ Html.Attributes.for "map-gen-steps" ] [ Html.text "steps" ]
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.value (String.fromInt model.mapGenConfig.steps)
                , Html.Attributes.id "map-gen-steps"
                , Html.Events.onInput (\v -> SetMapGenConfig (GridGen.withSteps (String.toInt v |> withDefault 0) model.mapGenConfig))
                ]
                []
            , Html.br [] []
            , Html.label [ Html.Attributes.for "map-gen-step-size" ] [ Html.text ("step size: " ++ String.fromFloat model.mapGenConfig.stepSize) ]
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.max "10"
                , Html.Attributes.step "0.01"
                , Html.Attributes.value (String.fromFloat model.mapGenConfig.stepSize)
                , Html.Attributes.id "map-gen-step-size"
                , Html.Events.onInput (\v -> SetMapGenConfig (GridGen.withStepSize (String.toFloat v |> withDefault 0) model.mapGenConfig))
                ]
                []
            , Html.label [ Html.Attributes.for "map-gen-persistence" ] [ Html.text "persistence" ]
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.value (String.fromFloat model.mapGenConfig.persistence)
                , Html.Attributes.id "map-gen-step-persistence"
                , Html.Events.onInput (\v -> SetMapGenConfig (GridGen.withPersistence (String.toFloat v |> withDefault 0) model.mapGenConfig))
                ]
                []
            ]
        , Html.details []
            [ Html.summary [] [ Html.text "Interaction mode" ]
            , renderInteractionModeSelector model.interactionMode
            ]
        ]


view : Model -> Html Msg
view model =
    let
        highlight =
            (case model.interactionMode of
                Line ->
                    Grid.fromPoints (Grid.line (Tuple.first model.activeHex) model.hoverPoint) Dot

                Raycast ->
                    let
                        ( hit, ray ) =
                            Grid.rayTrace (Tuple.first model.activeHex) model.hoverPoint (obstacles model.grid)
                    in
                    Grid.fromPoints
                        (ray |> Set.toList)
                        Dot
                        |> Dict.insert hit Overlay

                Ring ->
                    Grid.fromPoints
                        (Grid.ring (Grid.distance model.hoverPoint (Tuple.first model.activeHex)) (Tuple.first model.activeHex) |> Set.toList)
                        Overlay

                Vision range ->
                    let
                        visible =
                            Grid.fieldOfVisionWithCost range model.hoverPoint (visionCost model.grid model.entities)
                    in
                    Dict.diff
                        (Dict.filter (\p _ -> Set.member p model.discoveredTiles) model.grid)
                        (Grid.fromPoints (visible |> Set.toList) Overlay)
                        |> Dict.keys
                        |> (\ps -> Grid.fromPoints ps Fog)

                Path ->
                    let
                        ( explored, path ) =
                            HexEngine.Path.path (Tuple.first model.activeHex) model.hoverPoint (calculateCost model.grid model.entities)
                    in
                    Grid.fromPoints (Set.toList explored) Overlay
                        |> Dict.union (Grid.fromPoints (Set.toList path) Dot)

                _ ->
                    Dict.empty
            )
                |> Dict.insert (Tuple.first model.activeHex) Active
    in
    div [ Html.Attributes.id "app" ]
        [ if model.showSidepanel then
            sidePanel model

          else
            Html.div [] []
        , div [ Html.Attributes.class "game" ]
            (case model.interactionMode of
                Vision _ ->
                    [ Render.render4
                        model.renderConfig
                        model.hexAppearance
                        ( model.grid, Just model.discoveredTiles, simpleHex )
                        ( model.grid, Just model.discoveredTiles, hexIcon )
                        ( model.entities, Just model.visibleTiles, Entity.render )
                        ( highlight, Nothing, highlightHex )
                    ]

                _ ->
                    [ Render.render4
                        model.renderConfig
                        model.hexAppearance
                        ( model.grid, Nothing, simpleHex )
                        ( model.grid, Nothing, hexIcon )
                        ( model.entities, Nothing, Entity.render )
                        ( highlight, Nothing, highlightHex )
                    ]
            )
        ]



---- SUBS ----


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            KeyInput (Character char)

        _ ->
            KeyInput (Control string)


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


subs : Model -> Sub Msg
subs _ =
    Browser.Events.onKeyDown keyDecoder



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subs
        }
