module View exposing (page)

import Types exposing (..)
import Html exposing (Html, div, text, input, button, li, br, h1, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput, onClick)


page : Model -> Html Msg
page model =
    div []
        [ viewPause model
        , viewActive model
        , Html.ul [] (List.map viewPlayer model.players)
        ]


viewPause : Model -> Html Msg
viewPause model =
    if model.paused then
        button [ onClick (Pause False) ] [ text "Fortfahren" ]
    else
        button [ onClick (Pause True) ] [ text "Pausieren" ]


viewActive : Model -> Html Msg
viewActive model =
    div []
        [ text model.active_player.name
        , br [] []
        , text (viewTime model.buffer_time)
        , text "#"
        , text (viewTime model.active_player.time_left)
        , viewEndTurnButton model
        ]


viewEndTurnButton : Model -> Html Msg
viewEndTurnButton model =
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


viewPlayer : Player -> Html a
viewPlayer player =
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
            , text (viewTime player.time_left)
            ]


viewTime : Int -> String
viewTime seconds =
    let
        minutes =
            seconds // 60

        rest =
            seconds % 60
    in
        (toString minutes) ++ ":" ++ (String.padLeft 2 '0' (toString rest))
