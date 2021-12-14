module HexEngine.Render exposing
    ( HexAppearance
    , HexOrientation(..)
    , Layer
    , RenderConfig
    , circle
    , cornersToString
    , fancyHexCorners
    , hex
    , hexStroke
    , initAppearance
    , initRenderConfig
    , render1
    , render2
    , render3
    , render4
    , renderLayers
    , text
    , trimDashArrayStart
    , withCameraMovementX
    , withCameraMovementY
    , withCameraPositionX
    , withCameraPositionY
    , withCameraZoom
    , withDebug
    , withOrientation
    , withScale
    )

import Dict
import HexEngine.Grid exposing (HexGrid)
import HexEngine.Point as Point exposing (Point)
import Set exposing (Set)
import Svg exposing (Svg, svg)
import Svg.Attributes as SvgAttr exposing (height, width)
import Svg.Lazy


{-| Hex size constant, use RenderConfig.zoom if you need to change hex display size
-}
hexSize : Float
hexSize =
    50



---- HEX APPEARANCE BUILDER ----


{-| Hex orientation, flat or pointy top, used in RenderConfig
-}
type HexOrientation
    = FlatTop
    | PointyTop


type alias HexAppearance =
    { orientation : HexOrientation
    , scale : Float
    }


initAppearance : HexAppearance
initAppearance =
    HexAppearance FlatTop 1


withOrientation : HexOrientation -> HexAppearance -> HexAppearance
withOrientation orientation app =
    { app | orientation = orientation }


withScale : Float -> HexAppearance -> HexAppearance
withScale scale app =
    { app | scale = scale }



---- RENDER CONFIG BUILDER ----


{-| All render settings, use initRenderConfig to build default settings, and with\* to change settings
-}
type alias RenderConfig =
    { width : Int
    , height : Int
    , cameraX : Float
    , cameraY : Float
    , cameraZoom : Float
    , debug : Bool
    }


{-| Init default RenderConfig
-}
initRenderConfig : Int -> Int -> RenderConfig
initRenderConfig width height =
    RenderConfig width height 0 0 1 False


{-| Set camera x position
-}
withCameraPositionX : Float -> RenderConfig -> RenderConfig
withCameraPositionX x config =
    { config | cameraX = x }


{-| Set camera y position
-}
withCameraPositionY : Float -> RenderConfig -> RenderConfig
withCameraPositionY y config =
    { config | cameraY = y }


{-| Set camera zoom
-}
withCameraZoom : Float -> RenderConfig -> RenderConfig
withCameraZoom zoomMultiplier config =
    { config | cameraZoom = zoomMultiplier }


withCameraMovementX : Float -> RenderConfig -> RenderConfig
withCameraMovementX amount config =
    { config | cameraX = config.cameraX + amount }


withCameraMovementY : Float -> RenderConfig -> RenderConfig
withCameraMovementY amount config =
    { config | cameraY = config.cameraY + amount }



-- {-| Set hex margin, this will create a gap between hexes
-- -}
-- withHexMargin : Float -> RenderConfig -> RenderConfig
-- withHexMargin margin config =
--     { config | hexMargin = margin }


{-| Set debug mode
-}
withDebug : Bool -> RenderConfig -> RenderConfig
withDebug debug config =
    { config | debug = debug }



---- GENERAL RENDER STUFF ----


{-| Get the center of a given point in screen coordinates
-}
pointToPixel : HexOrientation -> Point -> ( Float, Float )
pointToPixel orientation point =
    let
        ( q, r ) =
            Point.toAxial point

        -- ( screenCenterX, screenCenterY ) =
        --     ( toFloat config.width / 2
        --     , toFloat config.height / 2
        --     )
    in
    case orientation of
        FlatTop ->
            ( hexSize * (3 / 2 * toFloat q)
            , hexSize * (sqrt 3 / 2 * toFloat q + sqrt 3 * toFloat r)
            )

        PointyTop ->
            ( hexSize * (sqrt 3 * toFloat q + sqrt 3 / 2 * toFloat r)
            , hexSize * (3 / 2 * toFloat r)
            )


cornersToString : HexCorners -> String
cornersToString { c0, c1, c2, c3, c4, c5 } =
    let
        tupleToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    tupleToString c0
        ++ " "
        ++ tupleToString c1
        ++ " "
        ++ tupleToString c2
        ++ " "
        ++ tupleToString c3
        ++ " "
        ++ tupleToString c4
        ++ " "
        ++ tupleToString c5


{-| The six corners of a hex in screen coordinates
-}
type alias HexCorners =
    { c0 : ( Float, Float )
    , c1 : ( Float, Float )
    , c2 : ( Float, Float )
    , c3 : ( Float, Float )
    , c4 : ( Float, Float )
    , c5 : ( Float, Float )
    }



---- EXPERIMENTAL ----


{-| Calculate hex corners in screen coordinates
-}
fancyHexCorners : HexOrientation -> HexCorners
fancyHexCorners orientation =
    let
        angleRad cornerNumber =
            case orientation of
                FlatTop ->
                    degrees (60 * cornerNumber |> toFloat)

                PointyTop ->
                    degrees (60 * cornerNumber - 30 |> toFloat)

        corner cornerNumber =
            case orientation of
                FlatTop ->
                    ( (hexSize / 2) + hexSize * cos (angleRad cornerNumber)
                    , (hexSize / 2) + hexSize * sin (angleRad cornerNumber)
                    )

                PointyTop ->
                    ( (hexSize / 2) + hexSize * cos (angleRad cornerNumber)
                    , (hexSize / 2) + hexSize * sin (angleRad cornerNumber)
                    )
    in
    HexCorners
        (corner 0)
        (corner 1)
        (corner 2)
        (corner 3)
        (corner 4)
        (corner 5)



-- {-| debug function, renders a colored dot at each hex corner
-- -}
-- renderHexCorners : HexCorners -> List (Svg msg)
-- renderHexCorners corners =
--     let
--         cornerToCoords c =
--             [ SvgAttr.cx (Tuple.first c |> String.fromFloat), SvgAttr.cy (Tuple.second c |> String.fromFloat) ]
--     in
--     [ Svg.circle (cornerToCoords corners.c0 ++ [ SvgAttr.fill "red", SvgAttr.r "3" ]) []
--     , Svg.circle (cornerToCoords corners.c1 ++ [ SvgAttr.fill "orange", SvgAttr.r "3" ]) []
--     , Svg.circle (cornerToCoords corners.c2 ++ [ SvgAttr.fill "yellow", SvgAttr.r "3" ]) []
--     , Svg.circle (cornerToCoords corners.c3 ++ [ SvgAttr.fill "lime", SvgAttr.r "3" ]) []
--     , Svg.circle (cornerToCoords corners.c4 ++ [ SvgAttr.fill "cyan", SvgAttr.r "3" ]) []
--     , Svg.circle (cornerToCoords corners.c5 ++ [ SvgAttr.fill "purple", SvgAttr.r "3" ]) []
--     ]


maybeToBool : Maybe a -> Bool
maybeToBool m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


{-| transform list of bools representing neighbours to list of (Float, Float) representing dashes and gaps
-}
dashArray : List Bool -> Float -> List ( Float, Float ) -> Bool -> List ( Float, Float )
dashArray adjacent sum listSum flag =
    let
        addSum s f ls =
            if f then
                ( 0, s ) :: ls

            else
                ( s, 0 ) :: ls
    in
    case adjacent of
        [] ->
            addSum sum flag listSum

        n :: ns ->
            if n == flag then
                dashArray ns (sum + hexSize) listSum flag

            else
                dashArray adjacent 0 (addSum sum flag listSum) (not flag)


{-| move list head to tail until head is false or we have made it through the entire list
returns float meant for strokeDashOffset and new list
-}
trimDashArrayStart : List Bool -> Float -> Int -> ( Float, List Bool )
trimDashArrayStart adjacent sum count =
    if count < 6 then
        case adjacent of
            [] ->
                ( sum, [] )

            x :: xs ->
                if x then
                    trimDashArrayStart (xs ++ [ x ]) (sum + hexSize) (count + 1)

                else
                    ( sum, adjacent )

    else
        ( sum, adjacent )


{-| Transform list of dashes and gaps to string to be used with strokeDashArray
-}
dashArrayString : List ( Float, Float ) -> String
dashArrayString xs =
    xs
        |> List.map
            (\( a, b ) ->
                String.fromFloat a ++ " " ++ String.fromFloat b
            )
        |> List.intersperse " "
        |> String.concat


{-| Set stroke based on neighbouring hexes
If hex has no neighbours it gets normal stroke
if it's completly surronded it gets no stroke
if it's partially surronded it gets stroke on empty sides
-}
hexStroke : List (Maybe d) -> List (Svg.Attribute msg)
hexStroke neighbours =
    let
        boolNeighbours =
            List.map maybeToBool neighbours

        ( strokeOffset, adjacent ) =
            trimDashArrayStart boolNeighbours 0 0

        defaultAttrs =
            [ SvgAttr.stroke "white"
            , SvgAttr.strokeWidth "8"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeDashoffset (strokeOffset + -hexSize |> String.fromFloat)
            ]
    in
    if List.all (\a -> a == True) boolNeighbours then
        []

    else if List.all (\a -> a == False) boolNeighbours then
        defaultAttrs

    else
        SvgAttr.strokeDasharray (dashArray adjacent 0 [] False |> dashArrayString) :: defaultAttrs


{-| A layer is a grid and a function that renders svg based on position/data
-}
renderLayer : HexAppearance -> Layer msg -> Svg msg
renderLayer app { points, render } =
    Svg.g []
        (points
            -- |> Debug.log "render layer"
            |> List.map
                (\point ->
                    let
                        ( hexCenterX, hexCenterY ) =
                            pointToPixel app.orientation point

                        transformAttr =
                            SvgAttr.transform
                                ("translate("
                                    ++ String.fromFloat hexCenterX
                                    ++ ", "
                                    ++ String.fromFloat hexCenterY
                                    ++ ") scale("
                                    ++ String.fromFloat app.scale
                                    ++ ")"
                                )

                        -- debugText d =
                        --     if d then
                        --         [ text (Point.toString point) [ SvgAttr.pointerEvents "none" ] ]
                        --     else
                        --         []
                    in
                    Svg.g [ transformAttr ] (render app.orientation point)
                )
        )


renderLayer2 : HexAppearance -> HexGrid d -> Maybe (Set Point) -> (HexGrid d -> HexOrientation -> ( Point, d ) -> Svg msg) -> Svg msg
renderLayer2 app grid whitelist render =
    Svg.g []
        (Dict.toList grid
            -- |> Debug.log "render layer"
            |> List.filterMap
                (\( point, tile ) ->
                    case whitelist of
                        Nothing ->
                            let
                                ( hexCenterX, hexCenterY ) =
                                    pointToPixel app.orientation point

                                transformAttr =
                                    SvgAttr.transform
                                        ("translate("
                                            ++ String.fromFloat hexCenterX
                                            ++ ", "
                                            ++ String.fromFloat hexCenterY
                                            ++ ") scale("
                                            ++ String.fromFloat app.scale
                                            ++ ")"
                                        )
                            in
                            Just (Svg.g [ transformAttr ] [ render grid app.orientation ( point, tile ) ])

                        Just wl ->
                            if Set.member point wl then
                                let
                                    ( hexCenterX, hexCenterY ) =
                                        pointToPixel app.orientation point

                                    transformAttr =
                                        SvgAttr.transform
                                            ("translate("
                                                ++ String.fromFloat hexCenterX
                                                ++ ", "
                                                ++ String.fromFloat hexCenterY
                                                ++ ") scale("
                                                ++ String.fromFloat app.scale
                                                ++ ")"
                                            )
                                in
                                Just (Svg.g [ transformAttr ] [ render grid app.orientation ( point, tile ) ])

                            else
                                Nothing
                )
        )


type alias Layer msg =
    { points : List Point
    , render : HexOrientation -> Point -> List (Svg msg)
    }


type alias GridRenderer a msg =
    ( HexGrid a, Maybe (Set Point), HexGrid a -> HexOrientation -> ( Point, a ) -> Svg msg )


renderLayers : List (Layer msg) -> RenderConfig -> HexAppearance -> Svg msg
renderLayers layers config app =
    let
        svgAttrs : RenderConfig -> List (Svg.Attribute msg)
        svgAttrs c =
            let
                ( w, h ) =
                    ( toFloat c.width / c.cameraZoom
                    , toFloat c.height / c.cameraZoom
                    )

                ( x, y ) =
                    ( c.cameraX - ((w / 2) - hexSize / 2)
                    , c.cameraY - ((h / 2) - hexSize / 2)
                    )
            in
            [ SvgAttr.viewBox
                ([ x, y, w, h ]
                    |> List.map String.fromFloat
                    |> List.intersperse " "
                    |> String.concat
                )
            , SvgAttr.preserveAspectRatio "xMidYMid slice"
            ]
    in
    svg
        (svgAttrs config)
        (List.map
            (\layer ->
                Svg.Lazy.lazy2 renderLayer app layer
            )
            layers
        )


renderSvg : RenderConfig -> List (Svg msg) -> Svg msg
renderSvg config children =
    let
        svgAttrs : RenderConfig -> List (Svg.Attribute msg)
        svgAttrs c =
            let
                ( w, h ) =
                    ( toFloat c.width / c.cameraZoom
                    , toFloat c.height / c.cameraZoom
                    )

                ( x, y ) =
                    ( c.cameraX - ((w / 2) - hexSize / 2)
                    , c.cameraY - ((h / 2) - hexSize / 2)
                    )
            in
            [ SvgAttr.viewBox
                ([ x, y, w, h ]
                    |> List.map String.fromFloat
                    |> List.intersperse " "
                    |> String.concat
                )
            , SvgAttr.preserveAspectRatio "xMidYMid slice"
            ]
    in
    svg
        (svgAttrs config)
        children


{-| render two hexgrids of varying types to a single svg element
-}
render1 :
    RenderConfig
    -> HexAppearance
    -> GridRenderer a msg
    -> Svg msg
render1 config appearance ( gridA, whitelist, renderA ) =
    renderSvg config
        [ Svg.Lazy.lazy4 renderLayer2 appearance gridA whitelist renderA
        ]


{-| render two hexgrids of varying types to a single svg element
-}
render2 :
    RenderConfig
    -> HexAppearance
    -> GridRenderer a msg
    -> GridRenderer b msg
    -> Svg msg
render2 config appearance ( gridA, whitelistA, renderA ) ( gridB, whitelistB, renderB ) =
    renderSvg config
        [ Svg.Lazy.lazy4 renderLayer2 appearance gridA whitelistA renderA
        , Svg.Lazy.lazy4 renderLayer2 appearance gridB whitelistB renderB
        ]


{-| render three hexgrids of varying types to a single svg element
-}
render3 :
    RenderConfig
    -> HexAppearance
    -> GridRenderer a msg
    -> GridRenderer b msg
    -> GridRenderer c msg
    -> Svg msg
render3 config appearance ( gridA, whitelistA, renderA ) ( gridB, whitelistB, renderB ) ( gridC, whitelistC, renderC ) =
    renderSvg config
        [ Svg.Lazy.lazy4 renderLayer2 appearance gridA whitelistA renderA
        , Svg.Lazy.lazy4 renderLayer2 appearance gridB whitelistB renderB
        , Svg.Lazy.lazy4 renderLayer2 appearance gridC whitelistC renderC
        ]


{-| render three hexgrids of varying types to a single svg element
-}
render4 :
    RenderConfig
    -> HexAppearance
    -> GridRenderer a msg
    -> GridRenderer b msg
    -> GridRenderer c msg
    -> GridRenderer d msg
    -> Svg msg
render4 config appearance ( gridA, whitelistA, renderA ) ( gridB, whitelistB, renderB ) ( gridC, whitelistC, renderC ) ( gridD, whitelistD, renderD ) =
    renderSvg config
        [ Svg.Lazy.lazy4 renderLayer2 appearance gridA whitelistA renderA
        , Svg.Lazy.lazy4 renderLayer2 appearance gridB whitelistB renderB
        , Svg.Lazy.lazy4 renderLayer2 appearance gridC whitelistC renderC
        , Svg.Lazy.lazy4 renderLayer2 appearance gridD whitelistD renderD
        ]



---- SHAPE PRESETS ----


hex : HexOrientation -> List (Svg.Attribute msg) -> Svg msg
hex orientation attrs =
    let
        corners =
            fancyHexCorners
                orientation

        pointsAttr =
            SvgAttr.points (corners |> cornersToString)
    in
    Svg.polygon
        (pointsAttr :: attrs)
        []


circle : Float -> List (Svg.Attribute msg) -> Svg msg
circle radius attrs =
    Svg.circle
        ([ SvgAttr.cx (hexSize / 2 |> String.fromFloat)
         , SvgAttr.cy (hexSize / 2 |> String.fromFloat)
         , SvgAttr.r (String.fromFloat radius)
         ]
            ++ attrs
        )
        []


text : String -> List (Svg.Attribute msg) -> Svg msg
text string attrs =
    Svg.text_
        ([ SvgAttr.textAnchor "middle"
         , SvgAttr.dominantBaseline "central"
         , SvgAttr.x (hexSize / 2 |> String.fromFloat)
         , SvgAttr.y (hexSize / 2 |> String.fromFloat)
         ]
            ++ attrs
        )
        [ Svg.text string ]
