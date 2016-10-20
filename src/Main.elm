module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Utils
import Game exposing (..)
import Random


type Msg
    = NewGame
    | RevealTile Int
    | NewBoard (List Bool)


type alias Model =
    Game


init : ( Model, Cmd Msg )
init =
    { status = NOT_STARTED, rows = 15, cols = 15, tiles = [] } ! []


statusView : Model -> Html Msg
statusView game =
    let
        ( status, c ) =
            case game.status of
                SAFE ->
                    ( " - You won", "status-won" )

                DEAD ->
                    ( " - You lost", "status-lost" )

                IN_PROGRESS ->
                    ( "", "" )

                NOT_STARTED ->
                    ( "Click new game to start", "" )
    in
        span [ class c ] [ text status ]


threatCount : Maybe Int -> List (Html Msg)
threatCount count =
    case count of
        Nothing ->
            []

        Just t ->
            [ text
                (if t > 0 then
                    toString t
                 else
                    ""
                )
            ]


rowView : List Game.Tile -> Html Msg
rowView tiles =
    let
        children =
            tiles
                |> List.map tileView
    in
        div [ class "row" ] children


tileView : Game.Tile -> Html Msg
tileView tile =
    if tile.isRevealed then
        let
            mClass =
                if tile.isMine then
                    " mine"
                else
                    ""
        in
            div [ class ("tile" ++ mClass) ] <| threatCount tile.threatCount
    else
        div [ class "tile", onClick (RevealTile tile.id) ]
            [ div [ class "lid" ] []
            ]


statsFooter : Model -> Html Msg
statsFooter game =
    let
        mCount =
            allMines game

        yetToBeRevealed =
            nonRevealedSafeTiles game
    in
        footer []
            [ div [ class "info" ]
                [ text ("Mines: " ++ (toString mCount))
                , text ("Yet to be revealed: " ++ (toString yetToBeRevealed))
                ]
            ]


view : Model -> Html Msg
view game =
    let
        rows =
            Utils.partitionByN game.cols game.tiles

        boardRows =
            rows |> List.map rowView
    in
        div [ id "main" ]
            [ h1 [] [ text "Minesweeper", statusView game ]
            , div [ class "board" ] boardRows
            , div []
                [ button [ class "button", type' "button", onClick NewGame ] [ text "New Game" ]
                ]
            , statsFooter game
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RevealTile tileId ->
            revealTile model tileId ! []

        NewGame ->
            let
                generator =
                    Utils.listBoolGenerator model.cols model.rows
            in
                model ! [ Random.generate NewBoard generator ]

        NewBoard tiles ->
            let
                newBoard =
                    generateBoard tiles
            in
                { model | tiles = newBoard, status = IN_PROGRESS } ! []


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
