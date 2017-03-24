module View exposing (page)

import Types exposing (..)
import Parser
import Html exposing (Html, div, text, input, button, li, br, ul, span, h1, p, label)
import Html.Attributes exposing (style, value, disabled, type_, checked)
import Html.Events exposing (onInput, onClick)
import Update


page : Model -> Html Msg
page model =
    case model of
        Ingame gameModel ->
            game gameModel

        Setup setupModel ->
            setup setupModel


setup : SetupModel -> Html Msg
setup model =
    div []
        [ h1 [] [ text "Timer Setup" ]
        , input [ value model.name_input, onInput NameInput ] []
        , playerList model.name_input
        , br [] []
        , text "Zeit pro Zug"
        , input [ value (toString model.config.buffer_time_initial), onInput BufferTimeInput ] []
        , checkbox "Übrige Zeit gutschreiben" model.config.keep_buffer KeepBufferInput
        , br [] []
        , text "Bedenkzeit"
        , input [ value (toString model.time_left), onInput TimeLeftInput ] []
        , br [] []
        , checkbox "Passen möglich" model.config.passing_allowed PassInput
        , checkbox "Nach Passen reagieren" model.config.passed_playing PassedPlayInput
        , text "Verminderte Zeit"
        , input [ value (toString model.config.passed_playing_time), disabled (not (model.config.passed_playing && model.config.passing_allowed)), onInput PassedPlayTime ] []
        , br [] []
        , br [] []
        , startGameButton model
        ]


checkbox : String -> Bool -> (Bool -> Msg) -> Html Msg
checkbox caption current message =
    label []
        [ input [ type_ "checkbox", onClick (message (not current)), checked current ] []
        , text caption
        ]


playerList : String -> Html a
playerList name_input =
    case Parser.playerList name_input of
        Ok names ->
            ul [] (List.map (\name -> li [] [ text name ]) names)

        Err errorText ->
            p [ style [ ( "color", "red" ) ] ] [ text errorText ]


startGameButton : SetupModel -> Html Msg
startGameButton setupModel =
    case Update.setupGame setupModel of
        Ok gameModel ->
            button [ onClick (StartGame gameModel) ] [ text "Timer starten" ]

        Err errorText ->
            div []
                [ button [ disabled True ] [ text "Timer starten" ]
                , p [ style [ ( "color", "red" ) ] ] [ text errorText ]
                ]



-- Game View


game : GameModel -> Html Msg
game model =
    div []
        [ pause model
        , activePlayer model
        , ul [] (List.map waitingPlayer model.players)
        ]


pause : GameModel -> Html Msg
pause model =
    if model.paused then
        button [ onClick (Pause False) ] [ text "Fortfahren" ]
    else
        button [ onClick (Pause True) ] [ text "Pausieren" ]


activePlayer : GameModel -> Html Msg
activePlayer model =
    div []
        [ text model.active_player.name
        , br [] []
        , text (timeFormat model.buffer_time)
        , text "#"
        , text (timeFormat model.active_player.time_left)
        , endTurnButton model
        ]


endTurnButton : GameModel -> Html Msg
endTurnButton model =
    if model.paused then
        div []
            [ button [ disabled True ] [ text "Passen" ]
            , button [ disabled True ] [ text "Zug beenden" ]
            ]
    else if model.active_player.passed == Nothing then
        div []
            [ button [ onClick Pass, disabled <| not model.config.passing_allowed ] [ text "Passen" ]
            , button [ onClick EndTurn ] [ text "Zug beenden" ]
            ]
    else
        div []
            [ button [ disabled True ] [ text "Passen" ]
            , button [ onClick EndTurn ] [ text "Zug beenden" ]
            ]


waitingPlayer : Player -> Html a
waitingPlayer player =
    let
        color =
            if player.passed == Nothing then
                "green"
            else
                "red"

        attribute =
            style [ ( "color", color ) ]
    in
        li [ attribute ]
            [ text player.name
            , text " -> "
            , text (timeFormat player.time_left)
            ]


timeFormat : Int -> String
timeFormat seconds =
    let
        minutes =
            seconds // 60

        rest =
            seconds % 60
    in
        (toString minutes) ++ ":" ++ (String.padLeft 2 '0' (toString rest))
