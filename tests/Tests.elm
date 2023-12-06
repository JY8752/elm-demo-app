module Tests exposing (..)

import Expect exposing (equal, notEqual)
import Fuzz exposing (intRange)
import Main exposing (gacha, lottery)
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
