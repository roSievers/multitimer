module View exposing (page)

import Types exposing (..)
import Parser
import Html exposing (Html, div, text, input, button, li, br, ul, span, h1, h2, p, label, hr)
import Html.Attributes exposing (style, value, disabled, type_, checked)
import Html.Events exposing (onInput, onClick)
import Update
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Radio as Radio
import Bootstrap.Button as Button
import Bootstrap.Alert as Alert
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Badge as Badge


page : Model -> Html Msg
page model =
    case model of
        Ingame gameModel ->
            game gameModel |> wrapBootstrap

        Setup setupModel ->
            setup setupModel |> wrapBootstrap


wrapBootstrap : Html Msg -> Html Msg
wrapBootstrap content =
    Grid.container []
        -- Responsive fixed width container
        [ CDN.stylesheet
          -- Inlined Bootstrap CSS for use with reactor
        , content
        ]


setup : SetupModel -> Html Msg
setup model =
    Form.form []
        [ h1 [] [ text "Einstellungen" ]
        , Input.text
            [ Input.id "nameInput"
            , Input.value model.name_input
            , Input.onInput NameInput
            , Input.placeholder "Spielernamen mit Komma trennen"
            ]
        , br [] []
        , playerList model.name_input
        , br [] []
        , Form.group []
            [ Form.label [] [ text "Zeit pro Zug in Sekunden: " ]
            , Input.number
                [ Input.value (toString model.config.buffer_time_initial)
                , Input.onInput BufferTimeInput
                ]
            , Checkbox.checkbox
                [ Checkbox.checked model.config.keep_buffer
                , Checkbox.onCheck KeepBufferInput
                , Checkbox.inline
                ]
                "Übrige Zeit am Ende vom Zug gutschreiben"
            ]
        , Form.group []
            [ Form.label [] [ text "Bedenkzeit (Insgesammt nach Ablauf der Zugzeit):" ]
            , Input.number
                [ Input.value (toString model.time_left)
                , Input.onInput TimeLeftInput
                ]
            ]
        , passingConfig model.config
        , br [] []
        , startGameButton model
        ]


passingConfig : Config -> Html Msg
passingConfig config =
    if not config.passing_allowed then
        enable_passing config
    else
        div []
            [ enable_passing config
            , passingBody config
            ]


passingBody : Config -> Html Msg
passingBody config =
    card
        [ Checkbox.checkbox
            [ Checkbox.checked config.passed_playing
            , Checkbox.onCheck PassedPlayInput
            , Checkbox.inline
            ]
            "Nach Passen reagieren"
        , Form.group []
            [ Form.label [] [ text "Verminderte Zeit, nach dem Passen:" ]
            , Input.number
                [ Input.value (toString config.passed_playing_time)
                , Input.onInput PassedPlayTime
                , Input.disabled (not (config.passed_playing && config.passing_allowed))
                ]
            ]
        , rearrangementChoice config.rearrangement
        ]


card : List (Html Msg) -> Html Msg
card msgHtmlList =
    Card.block []
        [ Card.custom <| div [] msgHtmlList
        ]
        (Card.config [])
        |> Card.view


enable_passing : Config -> Html Msg
enable_passing config =
    Checkbox.checkbox
        [ Checkbox.checked config.passing_allowed
        , Checkbox.onCheck PassInput
        , Checkbox.inline
        ]
        "Passen möglich"


displayIf : Bool -> ( String, String )
displayIf bool =
    ( "display"
    , if bool then
        "block"
      else
        "none"
    )


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
            Button.button
                [ Button.onClick (StartGame gameModel)
                , Button.primary
                ]
                [ text "Timer starten" ]

        Err errorText ->
            div []
                [ Button.button
                    [ Button.disabled True
                    ]
                    [ text "Timer starten" ]
                , Alert.danger [ text errorText ]
                ]


rearrangementChoice : Rearrangement -> Html Msg
rearrangementChoice current =
    let
        choices =
            [ ( Static, "Feste Spielreihenfolge" )
            , ( StartPlayer, "Startspieler nach Passen" )
            , ( PassOrder, "Komplett nach Passen" )
            ]

        description =
            case current of
                Static ->
                    "Immer der gleiche Spieler darf die neue Runde beginen."

                StartPlayer ->
                    "Wer als erstes passt darf die neue Runde beginnen. Die Reihenfolge wird beibehalten."

                PassOrder ->
                    "In der neuen Runde sind die Spieler nach Passreihenfolge sortiert."

        singleChoice ( new, caption ) =
            Radio.radio
                [ Radio.onClick (RearrangementInput new)
                , Radio.checked (current == new)
                ]
                caption
    in
        div []
            [ Form.group [] (List.map singleChoice choices)
            , p [] [ text description ]
            ]



-- Game View


game : GameModel -> Html Msg
game model =
    div [ style [ ( "margin", "auto" ), ( "max-width", "30em" ) ] ]
        [ activePlayer model
        , ListGroup.ul (List.map (waitingPlayer model.config.rearrangement) model.players)
        , passedPlayers model
        ]


pause : GameModel -> Html Msg
pause model =
    div [ style [ ( "display", "block" ), ( "float", "right" ) ] ]
        [ if model.paused then
            Button.button [ Button.onClick (Pause False) ] [ text "Fortfahren" ]
          else
            Button.button [ Button.onClick (Pause True) ] [ text "Pausieren" ]
        ]


activePlayer : GameModel -> Html Msg
activePlayer model =
    card
        [ pause model
        , playerName model
        , countdown model
        , endTurnButton model
        ]


playerName : GameModel -> Html a
playerName model =
    case rearrangementBadge model.config.rearrangement model.active_player of
        Just badge ->
            h1 []
                [ text model.active_player.name
                , text " "
                , span
                    [ style [ ( "font-size", "0.5em" ), ( "vertical-align", "middle" ) ] ]
                    [ badge ]
                ]

        Nothing ->
            h1 [] [ text model.active_player.name ]


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
            Button.disabled
                (model.paused
                    || (model.active_player.passed /= Nothing)
                    || not model.config.passing_allowed
                )

        passing_caption =
            if (model.active_player.passed /= Nothing) then
                "Bereits gepasst"
            else if model.num_passed == List.length model.players + List.length model.disabled_players then
                "Runde schließen"
            else
                "Passen"

        end_turn_disabled =
            Button.disabled model.paused
    in
        div []
            [ div [ style [ ( "float", "left" ), ( "display", "block" ) ] ]
                [ Button.button [ passing_disabled, Button.onClick Pass ] [ text passing_caption ] ]
            , div [ style [ ( "float", "right" ), ( "display", "block" ) ] ]
                [ Button.button [ end_turn_disabled, Button.onClick EndTurn, Button.primary ] [ text "Zug beenden" ] ]
            ]


waitingPlayer : Rearrangement -> Player -> ListGroup.Item a
waitingPlayer rearrangement player =
    ListGroup.li
        [ playerColor player
        , ListGroup.attrs [ Html.Attributes.class "justify-content-between" ]
        ]
        [ text player.name
        , playerBadge rearrangement player
        ]


playerColor : Player -> ListGroup.ItemOption a
playerColor player =
    case player.passed of
        Just _ ->
            ListGroup.danger

        Nothing ->
            ListGroup.success


playerBadge : Rearrangement -> Player -> Html a
playerBadge rearrangement player =
    case rearrangementBadge rearrangement player of
        Just badge ->
            span []
                [ badge, text " ", timeBadge player ]

        Nothing ->
            timeBadge player


timeBadge : Player -> Html a
timeBadge player =
    Badge.badge []
        [ text (timeFormat player.time_left) ]


rearrangementBadge : Rearrangement -> Player -> Maybe (Html a)
rearrangementBadge rearrangement player =
    case rearrangement of
        Static ->
            if player.position == 1 then
                Just <| Badge.pill [] [ text ("start") ]
            else
                Nothing

        StartPlayer ->
            if player.passed == Just 1 then
                Just <| Badge.pill [] [ text ("1.") ]
            else
                Nothing

        PassOrder ->
            player.passed
                |> Maybe.map (\position -> Badge.pill [] [ text (toString position ++ ".") ])


passedPlayers model =
    div [ style [ displayIf (List.length model.disabled_players > 0) ] ]
        [ hr [] []
        , ListGroup.ul (List.map (waitingPlayer model.config.rearrangement) model.disabled_players)
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
