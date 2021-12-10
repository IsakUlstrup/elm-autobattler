module HexEngine.Path exposing (path)

import HexEngine.Grid as Grid
import HexEngine.Point exposing (Point)
import Set exposing (Set)


type PathTree
    = Empty
    | Predecessor PathNode


type alias PathNode =
    { point : Point
    , cameFrom : PathTree
    , cost : Float
    }


{-| Given a pathnode and initial list, this function wil recursively find the points that lead back to the start of the path tree
-}
tracePathBack : PathNode -> List Point -> List Point
tracePathBack targetNode currentPath =
    case targetNode.cameFrom of
        Empty ->
            currentPath

        Predecessor pred ->
            tracePathBack pred (pred.point :: currentPath)


{-| Find a path from point to point given a function that calculates the cost of moving through a given point
The movementCost function should return a Float representing how hard a certain point is to traverse, Nothing is the Point is not traversable
Returns explored points and path to point
-}
path : Point -> Point -> (Point -> Maybe Float) -> ( Set Point, Set Point )
path from to movementCost =
    case movementCost to of
        Nothing ->
            -- ( Set.empty, Set.empty ) |> Debug.log ("invalid target" ++ HexEngine.Point.toString to)
            ( Set.empty, Set.empty )

        Just _ ->
            let
                closedSet : List PathNode
                closedSet =
                    []

                openSet : List PathNode
                openSet =
                    [ PathNode from Empty 0 ]

                exploredNodes =
                    exploreNodes from to openSet closedSet movementCost

                targetNode =
                    exploredNodes
                        |> List.filter (\n -> n.point == to)
                        |> List.head
            in
            case targetNode of
                Just node ->
                    ( List.map (\n -> n.point) exploredNodes |> Set.fromList
                    , tracePathBack node [ node.point ] |> Set.fromList
                    )

                Nothing ->
                    ( Set.empty, Set.empty )


lowerCostNode : List PathNode -> PathNode -> Maybe PathNode
lowerCostNode closedSet node =
    let
        matchingNode =
            closedSet
                |> List.filter (\n -> n.point == node.point)
                |> List.head
    in
    case matchingNode of
        Nothing ->
            Just node

        Just n ->
            if n.cost > node.cost then
                Just node

            else
                Nothing


{-| Explore neighbouring nodes, excluding those that can't be traversed
-}
exploreNodes : Point -> Point -> List PathNode -> List PathNode -> (Point -> Maybe Float) -> List PathNode
exploreNodes start target openSet closedSet movementCost =
    let
        origin : Maybe PathNode
        origin =
            List.sortBy .cost openSet
                |> List.head

        neighbors : PathNode -> List PathNode
        neighbors p =
            passableNeighbours start target p movementCost
                |> List.filterMap (lowerCostNode closedSet)

        newOpenSet node =
            List.filter (\n -> n.point /= node.point) openSet ++ neighbors node

        newClosedSet node =
            node :: closedSet
    in
    case origin of
        Just o ->
            if o.point == target then
                newClosedSet o

            else
                exploreNodes start target (newOpenSet o) (newClosedSet o) movementCost

        Nothing ->
            closedSet


{-| Given a point and a movement cost function, returns a list of PathNode to all neighbours with a movement cost
-}
passableNeighbours : Point -> Point -> PathNode -> (Point -> Maybe Float) -> List PathNode
passableNeighbours start target node movementCost =
    let
        neighbourNode point =
            movementCost point
                |> Maybe.andThen
                    (\cost ->
                        let
                            h =
                                Grid.distanceFloat node.point start

                            g =
                                Grid.distanceFloat node.point target

                            f =
                                cost + h + g
                        in
                        Just (PathNode point (Predecessor node) f)
                    )
    in
    Grid.neighbors node.point
        |> Set.toList
        |> List.filterMap neighbourNode
