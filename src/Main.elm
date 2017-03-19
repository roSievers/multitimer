module Main exposing (..)

import Types exposing (..)
import View
import Update
import Time
import Html
import Return exposing (Return, singleton)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = Update.update
        , subscriptions = subscriptions
        , view = View.page
        }


init : Return a Model
init =
    singleton (Setup { player_names = [], name_input = "" })


old_init : ( Model, Cmd Msg )
old_init =
    ( Ingame
        { active_player = Player "Jojo" 600 Nothing
        , buffer_time = 120
        , num_passed = 0
        , players = dummyPlayers
        , paused = True
        }
    , Cmd.none
    )


dummyPlayers : List Player
dummyPlayers =
    let
        time =
            10 * 60
    in
        [ Player "Theo" time Nothing
        , Player "Rolf" time Nothing
        , Player "ReRe" time Nothing
        , Player "Sara" time Nothing
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Ingame gameModel ->
            if gameModel.paused then
                Sub.none
            else
                Time.every Time.second (\_ -> TickDown)

        _ ->
            Sub.none
