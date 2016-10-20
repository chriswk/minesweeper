module Utils exposing (partitionByN, listBoolGenerator)

import List
import Random exposing (Generator, list, bool)


partitionByN : Int -> List a -> List (List a)
partitionByN n list =
    if List.isEmpty list then
        []
    else
        let
            catch =
                (List.take n list)
        in
            if n == (List.length catch) then
                [ catch ] ++ (partitionByN n (List.drop n list))
            else
                [ catch ]


listBoolGenerator : Int -> Int -> Generator (List Bool)
listBoolGenerator rows cols =
    let
        listSize =
            rows * cols
    in
        list listSize bool
