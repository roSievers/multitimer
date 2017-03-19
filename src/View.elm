module View exposing (page)

import Types exposing (..)
import Html exposing (Html, div, text, input, button, li, br, ul, span, h1)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput, onClick)


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
        , ul [] (List.map addedPlayer model.player_names)
        , input [ value model.name_input, onInput NameInput ] []
        , button [ onClick SubmitName ] [ text "Hinzufügen" ]
        , br [] []
        , button [ onClick StartGame ] [ text "Timer starten" ]
        ]


addedPlayer : String -> Html Msg
addedPlayer name =
    li [] [ text name ]


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
            [ button [ Html.Attributes.disabled True ] [ text "Passen" ]
            , button [ Html.Attributes.disabled True ] [ text "Zug beenden" ]
            ]
    else if model.active_player.passed == Nothing then
        div []
            [ button [ onClick Pass ] [ text "Passen" ]
            , button [ onClick EndTurn ] [ text "Zug beenden" ]
            ]
    else
        div []
            [ button [ Html.Attributes.disabled True ] [ text "Passen" ]
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
