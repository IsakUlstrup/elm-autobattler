module HexEngine.Grid exposing
    ( HexGrid
    , direction
    , distance
    , distanceFloat
    , empty
    , fieldOfVision
    , fieldOfVisionWithCost
    , filter
    , fromPoints
    , insert
    , line
    , movePoint
    , neighbor
    , neighborValues
    , neighbors
    , points
    , rayTrace
    , ring
    , valueAt
    )

import Dict exposing (Dict)
import HexEngine.Point as Point exposing (Point, valid)
import Set exposing (Set)


{-| main hexmap, a dict with 3d points as keys and d as values
-}
type alias HexGrid d =
    Dict Point d


{-| Return a list of (Point, d) tuples that meet predicate
-}
filter : (( Point, d ) -> Bool) -> HexGrid d -> List ( Point, d )
filter predicate grid =
    Dict.toList grid
        |> List.filter predicate


{-| Get a list of all points in a grid
-}
points : HexGrid d -> List Point
points grid =
    Dict.keys grid


{-| Create a new empty grid
-}
empty : HexGrid d
empty =
    Dict.fromList []


{-| Create a new grid from a list of points
-}
fromPoints : List Point -> d -> HexGrid d
fromPoints ps value =
    List.map (\p -> ( p, value )) ps |> Dict.fromList


{-| Insert data at point, replaces existing data if any
Returns unchanged grid if point is invalid
-}
insert : ( Point, d ) -> HexGrid d -> HexGrid d
insert ( point, data ) grid =
    if valid point then
        Dict.insert point data grid

    else
        grid


{-| Get value at point if it exists
Similar to Dict.get, but the arguments are reversed for easier piping
-}
valueAt : HexGrid d -> Point -> Maybe d
valueAt grid point =
    Dict.get point grid


{-| move a tile on the grid, doesn't change grid if from is not found
-}
movePoint : Point -> Point -> HexGrid d -> HexGrid d
movePoint from to grid =
    let
        tile p =
            Dict.get p grid
    in
    case tile from of
        Just t ->
            Dict.remove from grid |> Dict.insert to t

        Nothing ->
            grid


{-| get a list of all ajacent values
-}
neighborValues : Point -> HexGrid d -> List (Maybe d)
neighborValues point grid =
    List.map (valueAt grid)
        (List.range 0 5
            |> List.map (neighbor point)
        )


{-| Get direction given hex side
-}
direction : Int -> Point
direction dir =
    case dir of
        0 ->
            ( 1, -1, 0 )

        1 ->
            ( 1, 0, -1 )

        2 ->
            ( 0, 1, -1 )

        3 ->
            ( -1, 1, 0 )

        4 ->
            ( -1, 0, 1 )

        _ ->
            ( 0, -1, 1 )


{-| Get the neighbor at direction dir of a given point
-}
neighbor : Point -> Int -> Point
neighbor point dir =
    Point.add point (direction dir)


{-| Get all six neighbors
-}
neighbors : Point -> Set Point
neighbors p =
    [ neighbor p 0
    , neighbor p 1
    , neighbor p 2
    , neighbor p 3
    , neighbor p 4
    , neighbor p 5
    ]
        |> Set.fromList


{-| get distance between two points
-}
distance : Point -> Point -> Int
distance p1 p2 =
    distanceFloat p1 p2 |> round


distanceFloat : Point -> Point -> Float
distanceFloat ( x1, y1, z1 ) ( x2, y2, z2 ) =
    toFloat (abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)) / 2


{-| Return list of points that form a line between two points
-}
line : Point -> Point -> List Point
line from to =
    let
        dist =
            distance from to

        range =
            List.map toFloat (List.range 0 dist)
    in
    if from == to then
        []

    else
        List.map
            (\i -> Point.lerp (1.0 / toFloat dist * i) from to)
            range


{-| Get a line between two points, but stop at any obstacles
-}
rayTrace : Point -> Point -> Set Point -> ( Point, Set Point )
rayTrace from to obstacles =
    let
        ray =
            line from to

        visibleAcum : Point -> ( Bool, Point, List Point ) -> ( Bool, Point, List Point )
        visibleAcum point ( hitWall, hit, path ) =
            -- obstacle hit, return visible
            if hitWall then
                ( True, hit, path )
                -- obstacle hit, add obstacle to visible

            else if Set.member point obstacles then
                ( True, point, point :: path )
                -- obstacle not hit, add point to visible and continue

            else
                ( False, point, point :: path )

        ( _, hitPoint, visible ) =
            List.foldl visibleAcum ( False, from, [] ) ray
    in
    ( hitPoint, Set.union Set.empty (visible |> Set.fromList) )


{-| Get a line between two points, with cost for passing through tiles
The cost function defines how much it costs to pass throug a tile, a Nothing value means the tile can't be passed through
-}
rayTraceWithCost : Point -> Point -> Int -> (Point -> Maybe Int) -> Set Point
rayTraceWithCost from to cap cost =
    let
        ray =
            line from to

        visibleAcum point ( remaining, fs ) =
            if remaining > 0 then
                case cost point of
                    Just c ->
                        ( remaining - (1 + c), point :: fs )

                    Nothing ->
                        ( 0, point :: fs )

            else
                ( 0, fs )

        ( _, visible ) =
            List.foldl visibleAcum ( cap, [] ) ray
    in
    Set.union Set.empty (visible |> Set.fromList)


{-| Returns a set of unobstructed points within radius
-}
fieldOfVision : Int -> Point -> Set Point -> Set Point
fieldOfVision radius point obstacles =
    let
        ringPoints =
            ring radius point
                |> Set.toList
                |> List.map (\p -> rayTrace point p obstacles |> Tuple.second)
    in
    List.foldl Set.union Set.empty ringPoints


{-| Returns a set of visible points within radius, given a function that determines the cost of passing through a tile
-}
fieldOfVisionWithCost : Int -> Point -> (Point -> Maybe Int) -> Set Point
fieldOfVisionWithCost radius point cost =
    let
        ringPoints =
            ring radius point
                |> Set.toList
                |> List.map (\p -> rayTraceWithCost point p radius cost)
    in
    List.foldl Set.union Set.empty ringPoints


{-| Returns a ring around given point with given radius
This is more of a hex for now. Will fix later
-}
ring : Int -> Point -> Set Point
ring radius center =
    let
        getDirection s =
            case s of
                0 ->
                    4

                1 ->
                    5

                2 ->
                    0

                3 ->
                    1

                4 ->
                    2

                _ ->
                    3

        start s =
            Point.add center (Point.scale (toFloat radius) (direction (getDirection s)))

        side s =
            List.map (\i -> Point.add (start s) (Point.scale (toFloat i) (direction s))) (List.range 0 radius)
    in
    List.concatMap side (List.range 0 6) |> Set.fromList
