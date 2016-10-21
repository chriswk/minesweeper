module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)


type CssClasses
    = WaterMark
    | Toolbar
    | StatusLost
    | StatusWon
    | Time
    | Board
    | Tile
    | Threat2
    | Threat3
    | Threat4
    | Threat5
    | Threat6
    | Threat7
    | Threat8
    | Lid
    | Mine


type CssIds
    = Main


borderColor =
    hex "A49E96"


css =
    (stylesheet << namespace "minesweeper")
        [ body
            [ fontFamilies
                [ "Averia Sans Libre"
                , "Helvetica"
                , "Sans"
                , .value sansSerif
                ]
            ]
        , (.) WaterMark
            [ position absolute
            , width (pct 100)
            , property "z-index" "-1"
            ]
        , (.) Toolbar
            [ border3 (px 10) solid borderColor
            , borderRadius (px 10)
            , margin (px 20)
            , backgroundColor (hex "938D85")
            , display inlineBlock
            ]
        , (.) StatusLost []
        , (.) Time []
        , (.) Board []
        , (.) Tile []
        , (.) Threat2 []
        , (.) Threat3 []
        , (.) Threat4 []
        , (.) Threat5 []
        , (.) Threat6 []
        , (.) Threat7 []
        , (.) Threat8 []
        , (.) Lid []
        , (.) Mine []
        , (#) Main
            [ margin4 zero (px 20) zero (px 20)
            ]
        ]
