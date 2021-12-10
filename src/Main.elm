module Main exposing (..)

import Browser
import Dict
import Entity exposing (Entity)
import HexEngine.Grid as Grid exposing (HexGrid)
import HexEngine.GridGenerator as GridGen exposing (MapGenerationConfig)
import HexEngine.Path
import HexEngine.Point exposing (Point)
import HexEngine.Render exposing (HexAppearance, HexOrientation(..), RenderConfig)
import Html exposing (Html, div)
import Html.Attributes
import Html.Events
import Maybe exposing (withDefault)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes exposing (orientation)
import Svg.Events
import Util


renderText : HexOrientation -> Point -> List (Svg Msg)
renderText _ _ =
    let
        svgText =
            "hei"
    in
    [ HexEngine.Render.text svgText
        [ Svg.Attributes.fontSize "48pt"
        , Svg.Attributes.pointerEvents "none"
        ]
    ]


renderDot : HexOrientation -> Point -> List (Svg Msg)
renderDot _ _ =
    [ HexEngine.Render.circle 10
        [ Svg.Attributes.fill "rgba(50, 50, 50, 0.7)"
        , Svg.Attributes.pointerEvents "none"
        ]
    ]


simpleHex : HexGrid TileData -> HexOrientation -> ( Point, TileData ) -> Svg Msg
simpleHex grid orientation ( point, tile ) =
    let
        strokeAttr =
            HexEngine.Render.hexStroke (Grid.neighborValues point grid)

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
    HexEngine.Render.hex orientation
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
    HexEngine.Render.text svgText
        [ Svg.Attributes.fontSize "65pt"
        , Svg.Attributes.pointerEvents "none"
        , Svg.Attributes.y "0"
        ]


debugHex : HexGrid TileData -> HexOrientation -> ( Point, TileData ) -> Svg Msg
debugHex _ _ ( point, tile ) =
    HexEngine.Render.text (HexEngine.Point.toString point) []


renderEntity : HexGrid Entity -> HexOrientation -> ( Point, Entity ) -> Svg Msg
renderEntity _ _ ( point, entity ) =
    let
        svgText =
            if entity.player then
                "🐼"

            else
                "🦄"
    in
    HexEngine.Render.text svgText
        [ Svg.Attributes.fontSize "55pt"
        , Svg.Attributes.pointerEvents "none"
        ]


highlightHex : HexGrid Highlight -> HexOrientation -> ( Point, Highlight ) -> Svg Msg
highlightHex _ orientation ( point, highlight ) =
    let
        shape h =
            case h of
                Active ->
                    HexEngine.Render.hex orientation
                        [ Svg.Attributes.fill "rgba(255, 100, 100, 0.6)"
                        , Svg.Attributes.pointerEvents "none"
                        ]

                Hover ->
                    HexEngine.Render.hex orientation
                        [ Svg.Attributes.fill "rgba(255, 255, 255, 0.4)"
                        , Svg.Attributes.pointerEvents "none"
                        ]

                Overlay ->
                    HexEngine.Render.hex orientation
                        [ Svg.Attributes.fill "rgba(255, 255, 255, 0.4)"
                        , Svg.Attributes.pointerEvents "none"
                        ]

                Fog ->
                    HexEngine.Render.hex orientation
                        [ Svg.Attributes.fill "rgba(0, 0, 0, 0.4)"
                        , Svg.Attributes.pointerEvents "none"
                        ]

                Dot ->
                    HexEngine.Render.circle 10
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



---- MODEL ----


type InteractionMode
    = Highlight
    | Line
    | Raycast
    | Vision Int
    | Ring
    | Path
    | EntityVision


type alias Model =
    { renderConfig : HexEngine.Render.RenderConfig
    , hexAppearance : HexEngine.Render.HexAppearance
    , mapGenConfig : GridGen.MapGenerationConfig
    , grid : HexGrid TileData
    , entities : HexGrid Entity
    , highlight : HexGrid Highlight

    -- , highlightHexes : Set Point
    , activePoint : Point
    , hoverPoint : Point
    , interactionMode : InteractionMode
    , discoveredTiles : Set Point
    , visibleTiles : Set Point
    }


init : ( Model, Cmd Msg )
init =
    let
        mapGenConfig =
            GridGen.initMapGenConfig |> GridGen.withRadius 30 |> GridGen.withScale 2
    in
    ( { renderConfig = HexEngine.Render.initRenderConfig 1000 1000 |> HexEngine.Render.withCameraZoom 0.6
      , hexAppearance = HexEngine.Render.initAppearance |> HexEngine.Render.withScale 0.98
      , interactionMode = Highlight
      , mapGenConfig = mapGenConfig
      , grid = GridGen.randomHexMap mapGenConfig tileType |> Dict.insert ( 0, 0, 0 ) Grass
      , entities =
            Dict.fromList
                [ ( ( 0, -1, 1 ), Entity.new True )
                , ( ( -1, 1, 0 ), Entity.new False )
                , ( ( -5, 5, 0 ), Entity.new False )
                , ( ( 5, -5, 0 ), Entity.new True )
                ]
      , highlight = Dict.fromList [ ( ( 0, 0, 0 ), Active ) ]

      --   , highlightHexes = Set.fromList []
      , activePoint = ( 0, 0, 0 )
      , hoverPoint = ( 0, 0, 0 )
      , discoveredTiles = Set.singleton ( 0, 0, 0 )
      , visibleTiles = Set.singleton ( 0, 0, 0 )
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRenderConfig config ->
            ( { model | renderConfig = config }, Cmd.none )

        SetHexAppearance app ->
            ( { model | hexAppearance = app }, Cmd.none )

        ClickHex point ->
            -- ( { model | activePoint = point, highlightHexes = Set.empty, layers = model.layers |> updateGrid "highlight" [ point ] |> updateGrid "dots" [] }, Cmd.none )
            ( { model
                | activePoint = point

                -- , highlight = Dict.fromList (List.map hoverHex (model.highlightHexes |> Set.toList)) |> Dict.insert point Active
                , highlight = Dict.insert point Active model.highlight
              }
            , Cmd.none
            )

        HoverHex point ->
            case model.interactionMode of
                Line ->
                    -- ( { model | hoverPoint = point, highlightHexes = HexGrid.line model.activePoint point |> Set.fromList }, Cmd.none )
                    ( { model
                        | hoverPoint = point
                        , highlight =
                            Grid.fromPoints (Grid.line model.activePoint point) Dot
                      }
                    , Cmd.none
                    )

                Raycast ->
                    ( { model
                        | hoverPoint = point

                        -- , highlightHexes = Grid.rayTrace model.activePoint point (obstacles model.grid)
                        , highlight =
                            Grid.fromPoints
                                (Grid.rayTrace model.activePoint point (obstacles model.grid) |> Set.toList)
                                Dot
                      }
                    , Cmd.none
                    )

                Ring ->
                    ( { model
                        | hoverPoint = point

                        -- , highlightHexes = Grid.ring (Grid.distance point model.activePoint) model.activePoint
                        , highlight =
                            Grid.fromPoints (Grid.ring (Grid.distance point model.activePoint) model.activePoint |> Set.toList) Overlay
                      }
                    , Cmd.none
                    )

                Vision range ->
                    let
                        visible =
                            Grid.fieldOfVisionWithCost range point (visionCost model.grid model.entities)

                        discovered =
                            Set.union model.discoveredTiles visible
                    in
                    ( { model
                        | hoverPoint = ( 0, 0, 0 )

                        -- , highlightHexes = discovered
                        , discoveredTiles = discovered
                        , visibleTiles = visible
                        , highlight =
                            -- Grid.fromPoints (visible |> Set.toList) Overlay
                            Dict.diff
                                (Dict.filter (\p _ -> Set.member p model.discoveredTiles) model.grid)
                                (Grid.fromPoints (visible |> Set.toList) Overlay)
                                |> Dict.keys
                                |> (\ps -> Grid.fromPoints ps Fog)
                      }
                    , Cmd.none
                    )

                Highlight ->
                    -- ( { model | hoverPoint = point, layers = updateGrid "highlight" [ point ] model.layers }, Cmd.none )
                    ( { model
                        | highlight =
                            Dict.fromList
                                [ ( point, Hover )
                                , ( model.activePoint, Active )
                                ]
                        , hoverPoint = point

                        -- , highlightHexes = Set.singleton point
                      }
                    , Cmd.none
                    )

                Path ->
                    let
                        ( _, path ) =
                            HexEngine.Path.path model.activePoint point (calculateCost model.grid model.entities)
                    in
                    ( { model
                        | hoverPoint = point

                        -- , highlightHexes = HexEngine.Path.path model.activePoint point (calculateCost model.grid model.entities) |> Tuple.second
                        , highlight =
                            Grid.fromPoints (Set.toList path) Overlay
                                |> Dict.insert model.activePoint Active
                                |> Dict.insert point Hover

                        -- |> Dict.union
                        --     (Grid.fromPoints (Set.toList path) Overlay)
                      }
                    , Cmd.none
                    )

                EntityVision ->
                    let
                        playerPositions =
                            model.entities
                                |> Dict.toList
                                |> List.filterMap
                                    (\( p, e ) ->
                                        if e.player then
                                            Just p

                                        else
                                            Nothing
                                    )

                        fieldOfVision p =
                            Grid.fieldOfVision 5 p (obstacles model.grid) |> Set.toList

                        combinedVision =
                            List.concatMap fieldOfVision playerPositions
                    in
                    ( { model | highlight = Grid.fromPoints combinedVision Overlay }, Cmd.none )

        SetInteractionMode mode ->
            case mode of
                EntityVision ->
                    let
                        playerPositions =
                            model.entities
                                |> Dict.toList
                                |> List.filterMap
                                    (\( p, e ) ->
                                        if e.player then
                                            Just p

                                        else
                                            Nothing
                                    )

                        fieldOfVision p =
                            Grid.fieldOfVision 5 p (obstacles model.grid) |> Set.toList

                        combinedVision =
                            List.concatMap fieldOfVision playerPositions

                        highlightHex3 p =
                            ( p, Overlay )
                    in
                    ( { model | highlight = List.map highlightHex3 combinedVision |> Dict.fromList, interactionMode = EntityVision }, Cmd.none )

                _ ->
                    ( { model
                        | interactionMode = mode
                        , highlight = Dict.fromList [ ( model.activePoint, Active ), ( model.hoverPoint, Hover ) ]
                      }
                    , Cmd.none
                    )

        SetMapGenConfig config ->
            ( { model
                | mapGenConfig = config
                , grid = GridGen.randomHexMap config tileType |> Dict.insert ( 0, 0, 0 ) Grass
                , highlight = Dict.empty
                , hoverPoint = ( 0, 0, 0 )
                , activePoint = ( 0, 0, 0 )
                , visibleTiles = Set.singleton ( 0, 0, 0 )
                , discoveredTiles = Set.singleton ( 0, 0, 0 )
              }
            , Cmd.none
            )



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
            Dict.get point terrain
                |> Maybe.andThen
                    (\t ->
                        case t of
                            Mountain ->
                                Nothing

                            Woods ->
                                Just 2

                            _ ->
                                Just 1
                    )

        entityCost =
            case Dict.get point entities of
                Just _ ->
                    Just 2

                _ ->
                    Just 1
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
        (Html.h4 [] [ Html.text "Interaction Mode" ]
            :: radio Highlight "Hover"
            ++ radio Line "Line"
            ++ radio Raycast "Raycast"
            ++ radio Ring "Ring"
            ++ radio (Vision 4) "Vision"
            ++ radio Path "Path"
            ++ radio EntityVision "Entity Vision"
            ++ (Html.br [] []
                    :: options mode
               )
        )


view : Model -> Html Msg
view model =
    div [ Html.Attributes.id "app" ]
        [ div [ Html.Attributes.class "controls" ]
            [ div []
                [ Html.h4 [] [ Html.text "Zoom" ]
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min "0.06"
                    , Html.Attributes.max "2.5"
                    , Html.Attributes.step "0.1"
                    , Html.Attributes.value <| String.fromFloat model.renderConfig.cameraZoom
                    , Html.Events.onInput (\v -> SetRenderConfig (model.renderConfig |> HexEngine.Render.withCameraZoom (String.toFloat v |> withDefault 0)))
                    ]
                    []
                , Html.text (String.fromFloat model.renderConfig.cameraZoom)
                ]
            , div []
                [ Html.h4 [] [ Html.text "Pan X" ]
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min "-1000"
                    , Html.Attributes.max "1000"
                    , Html.Attributes.step "0.1"
                    , Html.Attributes.value <| String.fromFloat model.renderConfig.cameraX
                    , Html.Events.onInput (\v -> SetRenderConfig (model.renderConfig |> HexEngine.Render.withCameraPositionX (String.toFloat v |> withDefault 0)))
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
                    , Html.Events.onInput (\v -> SetRenderConfig (model.renderConfig |> HexEngine.Render.withCameraPositionY (String.toFloat v |> withDefault 0)))
                    ]
                    []
                , Html.text (String.fromFloat model.renderConfig.cameraY)
                ]
            , div []
                [ Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , Html.Attributes.id "render-debug"
                    , Html.Attributes.value (Util.boolToString model.renderConfig.debug)
                    , Html.Events.onInput (\_ -> SetRenderConfig (model.renderConfig |> HexEngine.Render.withDebug (not model.renderConfig.debug)))
                    ]
                    []
                , Html.label [ Html.Attributes.for "render-debug" ] [ Html.text "Render debug mode" ]
                ]
            , div []
                [ Html.h4 [] [ Html.text "Hex orientation" ]
                , Html.button [ Html.Events.onClick (SetHexAppearance (model.hexAppearance |> HexEngine.Render.withOrientation HexEngine.Render.FlatTop)) ] [ Html.text "Flat" ]
                , Html.button [ Html.Events.onClick (SetHexAppearance (model.hexAppearance |> HexEngine.Render.withOrientation HexEngine.Render.PointyTop)) ] [ Html.text "Pointy" ]
                ]
            , div []
                [ Html.h4 [] [ Html.text "Hex Scale" ]
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min "0"
                    , Html.Attributes.max "1"
                    , Html.Attributes.step "0.01"
                    , Html.Attributes.value <| String.fromFloat model.hexAppearance.scale
                    , Html.Events.onInput (\v -> SetHexAppearance (model.hexAppearance |> HexEngine.Render.withScale (String.toFloat v |> withDefault 0)))
                    ]
                    []
                , Html.text (String.fromFloat model.hexAppearance.scale)
                ]
            , div []
                [ Html.h4 [] [ Html.text "Map generation" ]
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
            , div []
                [ Html.h4 [] [ Html.text "Hex Count" ]
                , Html.text (Dict.size model.grid |> String.fromInt)
                ]
            , div []
                [ Html.h4 [] [ Html.text "Active Hex" ]
                , Html.text (HexEngine.Point.toString model.activePoint)
                ]
            , div []
                [ Html.h4 [] [ Html.text "Hover Hex" ]
                , Html.text (HexEngine.Point.toString model.hoverPoint)
                ]
            , renderInteractionModeSelector model.interactionMode
            ]
        , div [ Html.Attributes.class "game" ]
            (case model.interactionMode of
                Vision _ ->
                    [ HexEngine.Render.render4
                        model.renderConfig
                        model.hexAppearance
                        ( model.grid, Just model.discoveredTiles, simpleHex )
                        ( model.grid, Just model.discoveredTiles, hexIcon )
                        ( model.entities, Just model.visibleTiles, renderEntity )
                        ( model.highlight, Nothing, highlightHex )
                    ]

                _ ->
                    [ HexEngine.Render.render4
                        model.renderConfig
                        model.hexAppearance
                        ( model.grid, Nothing, simpleHex )
                        ( model.grid, Nothing, hexIcon )
                        ( model.entities, Nothing, renderEntity )
                        ( model.highlight, Nothing, highlightHex )
                    ]
            )
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
