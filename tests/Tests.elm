module Tests exposing (..)

import Expect exposing (equal, notEqual)
import Fuzz exposing (intRange)
import Main exposing (Rarity(..), gacha, getDisplayRarity, getRarity, lottery)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "The Main module"
        [ test "Hello" <|
            \_ ->
                let
                    act =
                        "Hello"
                in
                equal act "Hello"
        , fuzz (intRange 0 32) "lottery fuzzy test" <|
            \weight ->
                notEqual (lottery gacha weight) Maybe.Nothing
        ]


getRarityTest : Test
getRarityTest =
    describe "getRarityTest"
        (List.map
            (\( item, expected ) ->
                test ("Testing getRarity with " ++ item.name) <|
                    \_ -> getRarity item |> Expect.equal expected
            )
            [ ( { rarity = N, name = "Normal Item", weight = 10 }, "N" )
            , ( { rarity = R, name = "Rea Item", weight = 10 }, "R" )
            , ( { rarity = SR, name = "SuperRea Item", weight = 10 }, "SR" )
            ]
        )
