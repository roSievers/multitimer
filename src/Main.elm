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
            { name_input = ""
            , time_left = 600
            , config = initialConfig
            }
        )


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
