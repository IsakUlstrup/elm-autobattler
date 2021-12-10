module TestMap exposing (..)

import Dict
import HexEngine.Grid as Grid exposing (HexGrid)
import HexEngine.Point as Point exposing (Point)
import Set
import Simplex exposing (PermutationTable)


type TileData
    = Grass
    | Mountain
    | Water
    | ShallowWater
    | Road
    | Woods
    | Sand


testGrid : Int -> HexGrid TileData
testGrid radius =
    let
        insertPoint : Int -> List Point -> HexGrid Int -> HexGrid Int
        insertPoint data points grid =
            let
                tileData =
                    if data <= 4 then
                        data

                    else
                        1
            in
            case points of
                [] ->
                    grid

                p :: ps ->
                    Grid.insert ( p, tileData ) grid |> insertPoint (tileData + 1) ps

        coordinates =
            List.range -radius radius

        cartesian : List Int -> List Int -> List Int -> List Point
        cartesian xs ys zs =
            List.concatMap
                -- (\x -> List.map (\y -> ( x, y )) ys)
                (\x ->
                    List.concatMap
                        (\y ->
                            List.filterMap
                                (\z ->
                                    if x + y + z == 0 then
                                        Just ( x, y, z )

                                    else
                                        Nothing
                                )
                                zs
                        )
                        ys
                )
                xs
    in
    List.map (\p -> ( p, Grass )) (cartesian coordinates coordinates coordinates) |> Dict.fromList


testMap : HexGrid TileData
testMap =
    testGrid 50
        |> Grid.insert ( ( 2, 0, -2 ), Mountain )
        |> Grid.insert ( ( 0, 2, -2 ), Mountain )
        |> Grid.insert ( ( -2, 4, -2 ), Mountain )
        |> Grid.insert ( ( -2, 2, 0 ), Mountain )
        |> Grid.insert ( ( -2, 1, 1 ), Mountain )
        |> Grid.insert ( ( -1, 1, 0 ), Mountain )
        |> Grid.insert ( ( -5, 5, 0 ), Mountain )
        |> Grid.insert ( ( -5, 3, 2 ), Mountain )
        |> Grid.insert ( ( 3, -3, 0 ), Mountain )
        |> Grid.insert ( ( 3, -2, -1 ), Mountain )
        |> Grid.insert ( ( 3, -1, -2 ), Mountain )
        |> Grid.insert ( ( 4, -2, -2 ), Mountain )
        |> Grid.insert ( ( 5, -3, -2 ), Mountain )
        |> Grid.insert ( ( 5, -2, -3 ), Mountain )
        |> Grid.insert ( ( 6, -3, -3 ), Mountain )
        |> Grid.insert ( ( 6, -2, -4 ), Mountain )
        |> Grid.insert ( ( 7, -3, -4 ), Mountain )
        |> Grid.insert ( ( 7, -2, -5 ), Mountain )
        |> Grid.insert ( ( 7, -7, 0 ), Mountain )
        |> Grid.insert ( ( 6, -6, 0 ), Mountain )
        |> Grid.insert ( ( 0, -2, 2 ), Road )
        |> Grid.insert ( ( -1, -2, 3 ), Road )
        |> Grid.insert ( ( -1, -3, 4 ), Road )
        |> Grid.insert ( ( -2, -3, 5 ), Road )
        |> Grid.insert ( ( -3, -3, 6 ), Road )
        |> Grid.insert ( ( -5, -2, 7 ), Road )
        |> Grid.insert ( ( -4, -3, 7 ), Road )
        |> Grid.insert ( ( -3, 3, 0 ), Water )
        |> Grid.insert ( ( -4, 4, 0 ), Water )
        |> Grid.insert ( ( -4, 3, 1 ), Water )
        |> Grid.insert ( ( -5, 4, 1 ), ShallowWater )
        |> Grid.insert ( ( -6, 4, 2 ), ShallowWater )
        |> Grid.insert ( ( -7, 4, 3 ), ShallowWater )
        |> Grid.insert ( ( -8, 5, 3 ), ShallowWater )
        |> Grid.insert ( ( -9, 5, 4 ), ShallowWater )
        |> Grid.insert ( ( -10, 5, 5 ), ShallowWater )
        |> Grid.insert ( ( -10, 4, 6 ), ShallowWater )
        |> Grid.insert ( ( -10, 3, 7 ), ShallowWater )
        |> Grid.insert ( ( -10, 2, 8 ), ShallowWater )
        |> Grid.insert ( ( -10, 1, 9 ), ShallowWater )
        |> Grid.insert ( ( -11, 1, 10 ), ShallowWater )
        |> Grid.insert ( ( -12, 1, 11 ), ShallowWater )
        |> Grid.insert ( ( 1, 2, -3 ), Mountain )
        |> Grid.insert ( ( 2, 2, -4 ), Mountain )
        |> Grid.insert ( ( 3, 2, -5 ), Mountain )
        |> Grid.insert ( ( 4, 2, -6 ), Mountain )
        |> Grid.insert ( ( 5, 1, -6 ), Mountain )
        |> Grid.insert ( ( 6, 0, -6 ), Mountain )
        |> Grid.insert ( ( 6, -1, -5 ), Mountain )
        |> Grid.insert ( ( -2, 3, -1 ), ShallowWater )
        |> Grid.insert ( ( -1, 3, -2 ), ShallowWater )
        |> Grid.insert ( ( -1, 4, -3 ), ShallowWater )
        |> Grid.insert ( ( 0, 4, -4 ), ShallowWater )
        |> Grid.insert ( ( -1, 2, -1 ), ShallowWater )
        |> Grid.insert ( ( 0, 1, -1 ), ShallowWater )
        |> Grid.insert ( ( 0, 0, 0 ), ShallowWater )
        |> Grid.insert ( ( -1, 0, 1 ), ShallowWater )
        |> Grid.insert ( ( -2, 0, 2 ), ShallowWater )
        |> Grid.insert ( ( 1, -2, 1 ), Road )
        |> Grid.insert ( ( 2, -3, 1 ), Road )
        |> Grid.insert ( ( 3, -4, 1 ), Road )
        |> Grid.insert ( ( 4, -4, 0 ), Road )


testMap2 : HexGrid TileData
testMap2 =
    testGrid 1
        |> Grid.insert ( ( 2, -2, 0 ), Road )
        |> Grid.insert ( ( 1, -1, 0 ), Road )
        |> Grid.insert ( ( 3, -3, 0 ), Road )
        |> Grid.insert ( ( 3, -2, -1 ), Grass )
        |> Grid.insert ( ( -3, 2, 1 ), Grass )


axialMap : HexGrid TileData
axialMap =
    Grid.empty
        |> Grid.insert ( Point.fromAxial ( -3, -3 ), Grass )
        |> Grid.insert ( Point.fromAxial ( 3, -3 ), Grass )
        |> Grid.insert ( Point.fromAxial ( 3, 3 ), Grass )
        |> Grid.insert ( Point.fromAxial ( -3, 3 ), Grass )



---- Map generation ----


type alias MapGenerationConfig =
    { size : Int
    , seed : Int
    , scale : Float
    , steps : Int
    , stepSize : Float
    , persistence : Float
    , permTable : PermutationTable
    }


initMapGenConfig : MapGenerationConfig
initMapGenConfig =
    MapGenerationConfig 20 42 1.0 3 2.5 6.0 (Simplex.permutationTableFromInt 42)


withSize : Int -> MapGenerationConfig -> MapGenerationConfig
withSize size config =
    { config | size = size }


withSeed : Int -> MapGenerationConfig -> MapGenerationConfig
withSeed seed config =
    { config | seed = seed }


withScale : Float -> MapGenerationConfig -> MapGenerationConfig
withScale scale config =
    { config | scale = scale }


withSteps : Int -> MapGenerationConfig -> MapGenerationConfig
withSteps steps config =
    { config | steps = steps }


withStepSize : Float -> MapGenerationConfig -> MapGenerationConfig
withStepSize stepSize config =
    { config | stepSize = stepSize }


withPersistence : Float -> MapGenerationConfig -> MapGenerationConfig
withPersistence persistence config =
    { config | persistence = persistence }


withPermTable : Int -> MapGenerationConfig -> MapGenerationConfig
withPermTable seed config =
    { config | permTable = Simplex.permutationTableFromInt seed }



-- Create a function for 2D fractal noise


noise : MapGenerationConfig -> Float -> Float -> Float
noise config x y =
    Simplex.fractal2d
        { scale = config.scale
        , steps = config.steps
        , stepSize = config.stepSize
        , persistence = config.persistence
        }
        config.permTable
        x
        y


randomMap : MapGenerationConfig -> HexGrid TileData
randomMap config =
    let
        points =
            List.range (-config.size // 2) (config.size // 2)
                |> List.concatMap
                    (\x ->
                        List.range (-config.size // 2) (config.size // 2)
                            |> List.map
                                (\y ->
                                    ( x, y )
                                )
                    )

        tile ( x, y ) =
            let
                pointValue =
                    noise config (toFloat x) (toFloat y)

                tileType =
                    if pointValue < -0.4 then
                        Nothing

                    else if pointValue < -0.2 then
                        Just Sand

                    else if pointValue < 0.4 then
                        Just Grass

                    else if pointValue < 0.4 then
                        Just Woods

                    else
                        Just Mountain
            in
            tileType
                |> Maybe.andThen (\t -> Just ( Point.fromAxial ( x, y ), t ))
    in
    List.filterMap tile points |> Dict.fromList |> Grid.insert ( ( 0, 0, 0 ), Grass )


randomHexMap : MapGenerationConfig -> HexGrid TileData
randomHexMap config =
    let
        points =
            List.range 1 config.size
                |> List.map (\r -> Grid.ring r ( 0, 0, 0 ))
                |> List.foldl Set.union Set.empty
                |> Set.toList

        tile ( x, y, z ) =
            let
                ( ax, ay ) =
                    Point.toAxial ( x, y, z )

                pointValue =
                    noise config (toFloat ax) (toFloat ay)

                tileType =
                    if pointValue < -0.2 then
                        Nothing

                    else if pointValue < 0 then
                        Just Sand

                    else if pointValue < 0.4 then
                        Just Grass

                    else if pointValue < 0.55 then
                        Just Woods

                    else
                        Just Mountain
            in
            tileType
                |> Maybe.andThen (\t -> Just ( ( x, y, z ), t ))
    in
    List.filterMap tile points
        |> Dict.fromList
        |> Grid.insert ( ( 0, 0, 0 ), Grass )


fillHexMap : MapGenerationConfig -> TileData -> HexGrid TileData
fillHexMap config tile =
    let
        points =
            List.range 1 config.size
                |> List.map (\r -> Grid.ring r ( 0, 0, 0 ))
                |> List.foldl Set.union Set.empty
                |> Set.toList

        grassTile point =
            ( point, tile )
    in
    List.map grassTile points
        |> Dict.fromList
