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
    singleton
        (Setup
            { name_input = "Ada, Bert, Calvin, Dora"
            , time_left = 600
            , config = initialConfig
            }
        )


initialConfig : Config
initialConfig =
    { buffer_time_initial = 120
    , keep_buffer = False
    , passing_allowed = True
    , passed_playing = False
    , passed_playing_time = 120
    , rearrangement = StartPlayer
    }

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
