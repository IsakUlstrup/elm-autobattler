module HexEngine.GridGenerator exposing
    ( MapGenerationConfig
    , initMapGenConfig
    , randomHexMap
    , singleton
    , withPermTable
    , withPersistence
    , withRadius
    , withScale
    , withSeed
    , withStepSize
    , withSteps
    )

import Dict
import HexEngine.Grid as Grid exposing (HexGrid)
import HexEngine.Point as Point
import Set
import Simplex exposing (PermutationTable)


type alias MapGenerationConfig =
    { radius : Int
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


withRadius : Int -> MapGenerationConfig -> MapGenerationConfig
withRadius radius config =
    { config | radius = radius }


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


{-| Create 2D fractal noise
-}
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


{-| Generate a random map given som parameters and a function to determine tile type based on a float in the range -1 to 1
-}
randomHexMap : MapGenerationConfig -> (Float -> Maybe d) -> HexGrid d
randomHexMap config tileType =
    let
        points =
            List.range 1 config.radius
                |> List.map (\r -> Grid.ring r ( 0, 0, 0 ))
                |> List.foldl Set.union Set.empty
                |> Set.toList

        tile ( x, y, z ) =
            let
                ( ax, ay ) =
                    Point.toAxial ( x, y, z )

                pointValue =
                    noise config (toFloat ax) (toFloat ay)
            in
            tileType pointValue
                |> Maybe.andThen (\t -> Just ( ( x, y, z ), t ))
    in
    List.filterMap tile points
        |> Dict.fromList


{-| Generate a hex-shaped map with given radius, all tiles filled with given tile
-}
singleton : MapGenerationConfig -> d -> HexGrid d
singleton config tile =
    let
        points =
            List.range 1 config.radius
                |> List.map (\r -> Grid.ring r ( 0, 0, 0 ))
                |> List.foldl Set.union Set.empty
                |> Set.toList

        grassTile point =
            ( point, tile )
    in
    List.map grassTile points
        |> Dict.fromList
