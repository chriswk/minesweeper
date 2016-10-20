module Game exposing (..)

import Array
import Random
import Utils exposing (listBoolGenerator)


type alias Tile =
    { id : Int
    , threatCount : Maybe Int
    , isRevealed : Bool
    , isMine : Bool
    }


type GameStatus
    = IN_PROGRESS
    | SAFE
    | DEAD
    | NOT_STARTED


type alias Game =
    { status : GameStatus
    , rows : Int
    , cols : Int
    , tiles : List Tile
    }


type Direction
    = W
    | NW
    | N
    | NE
    | E
    | SE
    | S
    | SW


tileByIdx : Game -> Int -> Maybe Tile
tileByIdx game idx =
    if idx >= 0 then
        List.drop idx game.tiles |> List.head
    else
        Nothing


onWestEdge : Game -> Tile -> Bool
onWestEdge game tile =
    (tile.id % game.cols) == 0


onEastEdge : Game -> Tile -> Bool
onEastEdge game tile =
    (tile.id % game.cols) == game.cols - 1


neighbourByDirection : Game -> Maybe Tile -> Direction -> Maybe Tile
neighbourByDirection game tile dir =
    let
        tIdx =
            tileByIdx game

        isWOk t =
            not <| onWestEdge game t

        isEOk t =
            not <| onEastEdge game t
    in
        case ( tile, dir ) of
            ( Nothing, _ ) ->
                Nothing

            ( Just t, N ) ->
                tIdx <| t.id - game.cols

            ( Just t, S ) ->
                tIdx <| t.id + game.cols

            ( Just t, W ) ->
                if isWOk t then
                    tIdx <| t.id - 1
                else
                    Nothing

            ( Just t, NW ) ->
                if isWOk t then
                    tIdx <| t.id - game.cols - 1
                else
                    Nothing

            ( Just t, SW ) ->
                if isWOk t then
                    tIdx <| t.id + game.cols - 1
                else
                    Nothing

            ( Just t, E ) ->
                if isEOk t then
                    tIdx <| t.id + 1
                else
                    Nothing

            ( Just t, NE ) ->
                if isEOk t then
                    tIdx <| t.id - game.cols + 1
                else
                    Nothing

            ( Just t, SE ) ->
                if isEOk t then
                    tIdx <| t.id + game.cols + 1
                else
                    Nothing


neighbours : Game -> Maybe Tile -> List Tile
neighbours game tile =
    let
        n =
            neighbourByDirection game tile
    in
        List.filterMap identity <| List.map n [ W, NW, N, NE, E, SE, S, SW ]


mineCount : Game -> Maybe Tile -> Int
mineCount game tile =
    neighbours game tile
        |> List.filter .isMine
        |> List.length


updateIn : Int -> (a -> a) -> List a -> List a
updateIn idx f items =
    let
        ts =
            Array.fromList items

        t =
            Array.get idx ts
    in
        case t of
            Just v ->
                Array.toList <| Array.set idx (f v) ts

            Nothing ->
                items


revealMine : Tile -> Tile
revealMine tile =
    { tile | isRevealed = tile.isRevealed || tile.isMine }


revealMines : Game -> Game
revealMines game =
    { game
        | tiles = List.map revealMine game.tiles
        , status = DEAD
    }


revealThreatCount : Game -> Tile -> Tile
revealThreatCount game tile =
    { tile
        | threatCount = Just (mineCount game <| Just tile)
        , isRevealed = True
    }


revealAdjacentSafeTiles : Game -> Int -> Game
revealAdjacentSafeTiles game tileId =
    case tileByIdx game tileId of
        Nothing ->
            game

        Just t ->
            if t.isMine then
                game
            else
                let
                    updT =
                        revealThreatCount game t

                    updG =
                        { game | tiles = updateIn tileId (\_ -> updT) game.tiles }

                    fn t g =
                        if not t.isRevealed then
                            revealAdjacentSafeTiles g t.id
                        else
                            g
                in
                    if not (updT.threatCount == Just 0) then
                        updG
                    else
                        List.foldl fn updG <| neighbours updG <| Just updT


isSafe : Game -> Bool
isSafe game =
    let
        noMinesRevealed =
            game.tiles
                |> List.filter (\t -> t.isMine)
                |> List.filter (\t -> t.isRevealed)
                |> List.length

        allTilesRevealed =
            game.tiles
                |> List.filter (\t -> not t.isMine)
                |> List.filter (\t -> not t.isRevealed)
                |> List.length
    in
        noMinesRevealed == 0 && allTilesRevealed == 0


attemptWinning : Game -> Game
attemptWinning game =
    let
        s =
            if isSafe game then
                SAFE
            else
                IN_PROGRESS
    in
        { game | status = s }


revealTile : Game -> Int -> Game
revealTile game tileId =
    let
        t =
            tileByIdx game tileId
    in
        case t of
            Nothing ->
                game

            Just v ->
                if v.isMine then
                    revealMines game
                else
                    revealAdjacentSafeTiles game tileId
                        |> attemptWinning


createTile : Int -> Bool -> Tile
createTile id isMine =
    Tile id Nothing False isMine


generateBoard : List Bool -> List Tile
generateBoard tiles =
    tiles
        |> List.indexedMap createTile


allMines : Game -> Int
allMines game =
    game.tiles
        |> List.filter .isMine
        |> List.length


nonRevealedSafeTiles : Game -> Int
nonRevealedSafeTiles game =
    game.tiles
        |> List.filter (\t -> (not t.isMine))
        |> List.filter (\t -> (not t.isRevealed))
        |> List.length
