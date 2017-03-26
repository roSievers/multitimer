module View exposing (page)

import Types exposing (..)
import Parser
import Html exposing (Html, div, text, input, button, li, br, ul, span, h1, h2, p, label, hr)
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
    div [ style [ ( "margin", "auto" ), ( "max-width", "30em" ) ] ]
        [ h1 [] [ text "Timer Setup" ]
        , input [ value model.name_input, onInput NameInput, style [ ( "width", "100%" ) ] ] []
        , playerList model.name_input
        , br [] []
        , text "Zeit pro Zug: "
        , input
            [ value (toString model.config.buffer_time_initial)
            , onInput BufferTimeInput
            , style [ ( "width", "3em" ), ( "margin-right", "1em" ) ]
            ]
            []
        , checkbox "Übrige Zeit gutschreiben" model.config.keep_buffer KeepBufferInput
        , br [] []
        , text "Bedenkzeit: "
        , input
            [ value (toString model.time_left)
            , onInput TimeLeftInput
            , style [ ( "width", "3em" ), ( "margin-right", "1em" ) ]
            ]
            []
        , checkbox "Passen möglich" model.config.passing_allowed PassInput
        , passingConfig model.config
        , br [] []
        , startGameButton model
        ]


passingConfig : Config -> Html Msg
passingConfig config =
    div [ style [ displayIf config.passing_allowed, ( "margin-left", "1em" ) ] ]
        [ checkbox "Nach Passen reagieren" config.passed_playing PassedPlayInput
        , div [ style [ ( "margin-left", "1em" ), ( "margin-top", "0.5em" ) ] ]
            [ text "Verminderte Zeit: "
            , input
                [ value (toString config.passed_playing_time)
                , disabled (not (config.passed_playing && config.passing_allowed))
                , onInput PassedPlayTime
                , style [ ( "width", "3em" ), ( "margin-right", "1em" ) ]
                ]
                []
            ]
        , rearrangementChoice config.passing_allowed config.rearrangement
        ]


displayIf : Bool -> ( String, String )
displayIf bool =
    ( "display"
    , if bool then
        "block"
      else
        "none"
    )


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


rearrangementChoice : Bool -> Rearrangement -> Html Msg
rearrangementChoice passing_allowed current =
    div [ style [ ( "margin-top", "0.5em" ) ] ]
        [ rearrangementButton passing_allowed current Static "Feste Spielreihenfolge"
        , br [] []
        , rearrangementButton passing_allowed current StartPlayer "Startspieler nach Passen"
        , br [] []
        , rearrangementButton passing_allowed current PassOrder "Komplett nach Passen"
        ]


rearrangementButton : Bool -> Rearrangement -> Rearrangement -> String -> Html Msg
rearrangementButton passing_allowed current new caption =
    let
        cssStyles =
            style
                [ ( "width", "14em" )
                , ( "text-align", "left" )
                ]
    in
        if current == new then
            button [ disabled (not passing_allowed), cssStyles ]
                [ text ("X " ++ caption) ]
        else
            button [ onClick (RearrangementInput new), disabled (not passing_allowed), cssStyles ]
                [ text ("O " ++ caption) ]



-- Game View


game : GameModel -> Html Msg
game model =
    div [ style [ ( "margin", "auto" ), ( "max-width", "30em" ) ] ]
        [ activePlayer model
        , ul [ style [ ( "clear", "both" ), ( "margin-top", "3em" ) ] ]
            (List.map waitingPlayer model.players)
        , passedPlayers model
        ]


pause : GameModel -> Html Msg
pause model =
    div [ style [ ( "display", "block" ), ( "float", "right" ) ] ]
        [ if model.paused then
            button [ onClick (Pause False) ] [ text "Fortfahren" ]
          else
            button [ onClick (Pause True) ] [ text "Pausieren" ]
        ]


activePlayer : GameModel -> Html Msg
activePlayer model =
    div []
        [ pause model
        , h1 [] [ text model.active_player.name ]
        , countdown model
        , endTurnButton model
        ]


countdown : GameModel -> Html Msg
countdown model =
    if model.buffer_time > 0 then
        div []
            [ span
                [ style
                    [ ( "color", gradient (model.config.buffer_time_initial // 4) model.buffer_time )
                    , ( "font-size", "300%" )
                    , ( "font-weight", "bold" )
                    ]
                ]
                [ text (timeFormat model.buffer_time) ]
            , span
                [ style
                    [ ( "font-size", "200%" )
                    ]
                ]
                [ text "#", text (timeFormat model.active_player.time_left) ]
            ]
    else
        div []
            [ span
                [ style
                    [ ( "color", gradient 1 0 )
                    , ( "font-size", "200%" )
                    ]
                ]
                [ text (timeFormat model.buffer_time), text "#" ]
            , span
                [ style
                    [ ( "color", gradient 1 0 )
                    , ( "font-size", "300%" )
                    , ( "font-weight", "bold" )
                    ]
                ]
                [ text (timeFormat model.active_player.time_left) ]
            ]


gradient : Int -> Int -> String
gradient gradient_cutoff time_left =
    if gradient_cutoff < time_left then
        "hsl(120, 60%, 40%)"
    else
        "hsl(" ++ toString (120 * (toFloat time_left) / (toFloat gradient_cutoff)) ++ ", 60%, 40%)"


endTurnButton : GameModel -> Html Msg
endTurnButton model =
    let
        passing_disabled =
            disabled
                (model.paused
                    || (model.active_player.passed /= Nothing)
                    || not model.config.passing_allowed
                )

        end_turn_disabled =
            disabled model.paused
    in
        div []
            [ div [ style [ ( "float", "left" ), ( "display", "block" ) ] ]
                [ button [ passing_disabled, onClick Pass ] [ text "Passen" ] ]
            , div [ style [ ( "float", "right" ), ( "display", "block" ) ] ]
                [ button [ end_turn_disabled, onClick EndTurn ] [ text "Zug beenden" ] ]
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


passedPlayers model =
    div [ style [ displayIf (List.length model.disabled_players > 0) ] ]
        [ hr [] []
        , ul [] (List.map waitingPlayer model.disabled_players)
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
