module HexEngine.Point exposing
    ( Point
    , add
    , fromAxial
    , fromFloat
    , getX
    , getY
    , getZ
    , lerp
    , scale
    , subtract
    , toAxial
    , toString
    , valid
    )

{-| A 3d point
-}


type alias Point =
    ( Int, Int, Int )


{-| Create a new point from float values, round to nearest valid point
-}
fromFloat : ( Float, Float, Float ) -> Point
fromFloat ( x, y, z ) =
    let
        -- rounded point
        ( rx, ry, rz ) =
            ( round x, round y, round z )

        -- diierence between input point and rounded point
        ( dx, dy, dz ) =
            ( abs (toFloat rx - x), abs (toFloat ry - y), abs (toFloat rz - z) )

        -- final adjusted point
        ( fx, fy, fz ) =
            if dx > dy && dx > dz then
                ( -ry - rz, ry, rz )

            else if dy > dz then
                ( rx, -rx - rz, rz )

            else
                ( rx, ry, -rx - ry )
    in
    ( fx, fy, fz )


{-| Check if a point is valid
A valid point is one where x + y + z == 0
-}
valid : Point -> Bool
valid ( x, y, z ) =
    x + y + z == 0


{-| Convert cube point to axial point
Note: returns (0, 0) if point is invalid
-}
toAxial : Point -> ( Int, Int )
toAxial ( x, y, z ) =
    if valid ( x, y, z ) then
        ( x, z )

    else
        ( 0, 0 )


fromAxial : ( Int, Int ) -> Point
fromAxial ( q, r ) =
    ( q, r, -q - r )


{-| get x component of a point
-}
getX : Point -> Int
getX ( x, _, _ ) =
    x


{-| get y component of a point
-}
getY : Point -> Int
getY ( _, y, _ ) =
    y


{-| get z component of a point
-}
getZ : Point -> Int
getZ ( _, _, z ) =
    z


{-| Add two points together
-}
add : Point -> Point -> Point
add ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 + x2, y1 + y2, z1 + z2 )


{-| Subtract point from point
-}
subtract : Point -> Point -> Point
subtract ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 - x2, y1 - y2, z1 - z2 )


{-| Scale point
-}
scale : Float -> Point -> Point
scale i ( x1, y1, z1 ) =
    ( toFloat x1 * i, toFloat y1 * i, toFloat z1 * i ) |> fromFloat


{-| Point linear interpolation
-}
lerp : Float -> Point -> Point -> Point
lerp t ( x1, y1, z1 ) ( x2, y2, z2 ) =
    let
        lrp : Float -> Float -> Float -> Float
        lrp a b tt =
            a + (b - a) * tt
    in
    -- add p1 (subtract p2 p1) |> multiply t
    ( lrp (x1 |> toFloat) (x2 |> toFloat) t
    , lrp (y1 |> toFloat) (toFloat y2) t
    , lrp (toFloat z1) (toFloat z2) t
    )
        |> fromFloat


toString : Point -> String
toString ( x, y, z ) =
    [ "("
    , String.fromInt x
    , ", "
    , String.fromInt y
    , ", "
    , String.fromInt z
    , ")"
    ]
        |> String.concat



-- function lerp(a, b, t): # for floats
--     return a + (b - a) * t
-- function cube_lerp(a, b, t): # for hexes
--     return Cube(lerp(a.x, b.x, t),
--                 lerp(a.y, b.y, t),
--                 lerp(a.z, b.z, t))
