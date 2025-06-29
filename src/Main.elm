module Main exposing (main)

import Browser
import Html as H exposing (Html, button, div, text)
import Html.Attributes as A exposing (style)
import Html.Events as E exposing (onClick)
import Url



-- MODEL


type alias Model =
    { radius : String
    , radiusValid : Int
    , width : String
    , widthValid : Int
    , height : String
    , heightValid : Int
    }


init : Model
init =
    { radius = "100"
    , radiusValid = 100
    , width = "1000"
    , widthValid = 1000
    , height = "500"
    , heightValid = 500
    }



-- UPDATE


type Msg
    = RadiusChanged String
    | WidthChanged String
    | HeightChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        RadiusChanged value ->
            { model
                | radius = value
                , radiusValid = String.toInt value |> Maybe.withDefault model.radiusValid
            }

        WidthChanged value ->
            { model
                | width = value
                , widthValid = String.toInt value |> Maybe.withDefault model.widthValid
            }

        HeightChanged value ->
            { model
                | height = value
                , heightValid = String.toInt value |> Maybe.withDefault model.heightValid
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "gap" "8px"
            ]
            [ myInput { label = "Radius" } RadiusChanged model.radius
            , myInput { label = "Width" } WidthChanged model.width
            , myInput { label = "Height" } HeightChanged model.height
            ]
        , div
            [ style "position" "relative"
            , style "width" (String.fromInt model.widthValid ++ "px")
            , style "height" (String.fromInt model.heightValid ++ "px")
            , style "margin-top" "16px"
            ]
            [ div
                [ style "position" "absolute"
                , style "width" (String.fromInt model.widthValid ++ "px")
                , style "height" (String.fromInt model.heightValid ++ "px")
                , style "background" "black"
                , style "border-radius" (String.fromInt model.radiusValid ++ "px")
                ]
                []
            , div
                [ style "position" "absolute"
                , style "width" (String.fromInt model.widthValid ++ "px")
                , style "height" (String.fromInt model.heightValid ++ "px")
                , style "background" "yellow"
                , continuousBorderRadius (toFloat model.radiusValid)
                ]
                []
            ]
        ]


myInput { label } onInput value =
    H.label []
        [ H.div [] [ H.text label ]
        , H.div
            [ continuousBorderRadius 12
            , style "padding" "1px"
            , style "background" "#aaa"
            , style "display" "inline-block"
            , style "margin-top" "4px"
            ]
            [ H.input
                [ E.onInput onInput
                , A.value value
                , continuousBorderRadius 11
                , style "background" "#ddd"
                , style "padding" "8px"
                , style "border" "none"
                ]
                []
            ]
        ]


continuousBorderRadius : Float -> H.Attribute msg
continuousBorderRadius r =
    style "mask"
        (("url(\"data:image/svg+xml;charset=US-ASCII,"
            ++ Url.percentEncode (elemToStr (cornerPathSvgXX2 1 1 r))
            ++ "\") intersect no-repeat,"
         )
            ++ ("url(\"data:image/svg+xml;charset=US-ASCII,"
                    ++ Url.percentEncode (elemToStr (cornerPathSvgXX2 1 -1 r))
                    ++ "\") intersect no-repeat,"
               )
            ++ ("url(\"data:image/svg+xml;charset=US-ASCII,"
                    ++ Url.percentEncode (elemToStr (cornerPathSvgXX2 -1 1 r))
                    ++ "\") intersect no-repeat,"
               )
            ++ ("url(\"data:image/svg+xml;charset=US-ASCII,"
                    ++ Url.percentEncode (elemToStr (cornerPathSvgXX2 -1 -1 r))
                    ++ "\") intersect no-repeat"
               )
        )


cornerPathSvgXX2 : Int -> Int -> Float -> Elem
cornerPathSvgXX2 s1 s2 r =
    let
        w =
            r * 1.528665

        h =
            r * 1.528665
    in
    node "svg"
        [ attr "version" "1.1"
        , attr "xmlns" "http://www.w3.org/2000/svg"
        , attr "xmlns:xlink" "http://www.w3.org/1999/xlink"
        , attr "width" "100%"
        , attr "height" "100%"
        , attr "xml:space" "preserve"
        ]
        [ node "g"
            [ attr "transform-origin" "center"
            , attr "transform"
                ("scale(" ++ String.fromInt s1 ++ "," ++ String.fromInt s2 ++ ")")
            ]
            [ node "path"
                [ attr "fill" "black"
                , attr "d"
                    ([ m (w + 1) 0
                     , c
                        { x1 = 1.08849296 * r
                        , y1 = 0
                        , x2 = 0.86840694 * r
                        , y2 = 0
                        , x = 0.63149379 * r
                        , y = 0.07491139 * r
                        }
                     , c
                        { x1 = 0.37282383 * r
                        , y1 = 0.16905956 * r
                        , x2 = 0.16905956 * r
                        , y2 = 0.37282383 * r
                        , x = 0.07491139 * r
                        , y = 0.63149379 * r
                        }
                     , c
                        { x1 = 0
                        , y1 = 0.86840694 * r
                        , x2 = 0
                        , y2 = 1.08849296 * r
                        , x = 0
                        , y = 1.52866698 * r
                        }
                     , l 0 (h + 1)
                     , l (w + 1) (h + 1)
                     , z
                     ]
                        |> String.join " "
                    )
                ]
                []
            , node "rect"
                [ attr "x" (String.fromFloat w)
                , attr "y" "0"
                , attr "width" "100%"
                , attr "height" "100%"
                , attr "fill" "black"
                ]
                []
            , node "rect"
                [ attr "x" "0"
                , attr "y" (String.fromFloat h)
                , attr "width" "100%"
                , attr "height" "100%"
                , attr "fill" "black"
                ]
                []
            ]
        ]


{-| Move to
-}
m x y =
    "M" ++ String.fromFloat x ++ " " ++ String.fromFloat y


{-| Cubic Bézier
-}
c { x1, y1, x2, y2, x, y } =
    "C"
        ++ String.join " "
            [ String.fromFloat x1
            , String.fromFloat y1
            , String.fromFloat x2
            , String.fromFloat y2
            , String.fromFloat x
            , String.fromFloat y
            ]


{-| Line to
-}
l x y =
    "L" ++ String.fromFloat x ++ " " ++ String.fromFloat y


{-| Close path
-}
z =
    "Z"


type Attr
    = Attr String String


type Elem
    = Elem String (List Attr) (List Elem)


node : String -> List Attr -> List Elem -> Elem
node key attrs children =
    Elem key attrs children


attr : String -> String -> Attr
attr k v =
    Attr k v


elemToStr : Elem -> String
elemToStr (Elem tag attrs children) =
    let
        attrStr (Attr k v) =
            k ++ "=\"" ++ v ++ "\""

        attrsStr =
            String.join " " (List.map attrStr attrs)

        childrenStr =
            String.join "" (List.map elemToStr children)
    in
    if String.isEmpty childrenStr then
        "<"
            ++ tag
            ++ (if String.isEmpty attrsStr then
                    ""

                else
                    " " ++ attrsStr
               )
            ++ "/>"

    else
        "<"
            ++ tag
            ++ (if String.isEmpty attrsStr then
                    ""

                else
                    " " ++ attrsStr
               )
            ++ ">"
            ++ childrenStr
            ++ "</"
            ++ tag
            ++ ">"



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
