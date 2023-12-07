module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List exposing (foldl)
import Maybe exposing (map, withDefault)
import Platform.Cmd as Cmd
import Random


type Rarity
    = N
    | R
    | SR


type alias Item =
    { rarity : Rarity
    , name : String
    , weight : Int
    }



-- Model


type alias Model =
    { result : Maybe Item
    }



-- Msg


type Msg
    = Draw
    | GenerateRandomValue Int



-- ガチャに含まれるアイテム一覧


gacha : List Item
gacha =
    [ { rarity = N, name = "item1", weight = 10 }
    , { rarity = N, name = "item2", weight = 10 }
    , { rarity = R, name = "item3", weight = 5 }
    , { rarity = R, name = "item4", weight = 5 }
    , { rarity = SR, name = "item5", weight = 2 }
    ]


getRarity : Item -> String
getRarity item =
    case item.rarity of
        N ->
            "N"

        R ->
            "R"

        SR ->
            "SR"


getDisplayRarity : Model -> String
getDisplayRarity model =
    model.result
        |> map getRarity
        |> withDefault ""


getItemName : Model -> String
getItemName model =
    map (\item -> item.name) model.result
        |> withDefault ""



-- トータルの重みを算出


getTotalWeight : List Item -> Int
getTotalWeight items =
    foldl (\item acc -> acc + item.weight) 0 items



-- 乱数生成


generateRandomValue : Int -> Cmd Msg
generateRandomValue total =
    Random.generate GenerateRandomValue (Random.int 0 total)



-- アイテム抽選


lottery : List Item -> Int -> Maybe Item
lottery items randomValue =
    lotteryHelper items randomValue 0


lotteryHelper : List Item -> Int -> Int -> Maybe Item
lotteryHelper items randomValue acc =
    case items of
        [] ->
            Nothing

        item :: rest ->
            let
                newAcc =
                    acc + item.weight
            in
            if newAcc >= randomValue then
                Just item

            else
                lotteryHelper rest randomValue newAcc



-- init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { result = Nothing
      }
    , Cmd.none
    )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw ->
            ( model, getTotalWeight gacha |> generateRandomValue )

        GenerateRandomValue randomValue ->
            ( { model | result = lottery gacha randomValue }, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- view


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen flex justify-center items-center flex-col bg-slate-800" ]
        [ img [ src "/public/gachagacha.png", class "h-70 w-50" ] []
        , button [ onClick Draw, class "mt-10 p-5 rounded bg-indigo-500 hover:bg-indigo-300" ] [ text "ガチャを引く" ]
        , div [ class "mt-10 grid grid-cols-1" ]
            [ div [ class "font-bold text-lg text-pink-400 col-span-1 text-center mb-2" ] [ text "Result" ]
            , div [ class "col-span-1" ]
                [ div [ class "grid grid-cols-6 text-center text-white" ]
                    [ div [ class "col-span-3 -white" ] [ text "rarity" ]
                    , div [ class "col-span-3 text-center -white" ] [ text (getDisplayRarity model) ]
                    ]
                ]
            , div [ class "col-span-1" ]
                [ div [ class "grid grid-cols-6 text-center text-white" ]
                    [ div [ class "col-span-3" ] [ text "item name" ]
                    , div [ class "col-span-3 text-center" ] [ text (getItemName model) ]
                    ]
                ]
            ]
        ]



-- main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
