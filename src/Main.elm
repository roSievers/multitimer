module Main exposing (..)

{-|

https://en.wiktionary.org/wiki/Appendix:Russian_verbs
-}

import Return
import Html exposing (Html, div, text, input, button, li, br, h1, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput, onClick)
import Time


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { active_player : Player
    , buffer_time : Int
    , num_passed : Int
    , players : List Player
    , paused : Bool
    }


type alias Player =
    { name : String
    , time_left : Int
    , passed : Maybe Int
    }


type Msg
    = EndTurn
    | Pass
    | TickDown
    | Pause Bool


init : ( Model, Cmd Msg )
init =
    ( { active_player = Player "Jojo" 600 Nothing
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EndTurn ->
            ( updateEndTurn model, Cmd.none )

        Pass ->
            ( updatePass model, Cmd.none )

        TickDown ->
            ( updateTickDown model, Cmd.none )

        Pause pauseState ->
            ( updatePause pauseState model, Cmd.none )


updatePause : Bool -> Model -> Model
updatePause pauseState model =
    { model | paused = pauseState }


updateTickDown : Model -> Model
updateTickDown model =
    let
        active_player =
            model.active_player
    in
        if model.buffer_time > 0 then
            { model | buffer_time = model.buffer_time - 1 }
        else if active_player.time_left > 0 then
            { model | active_player = { active_player | time_left = model.active_player.time_left - 1 } }
        else if active_player.passed == Nothing then
            updatePass model
        else
            updateEndTurn model


updateEndTurn : Model -> Model
updateEndTurn model =
    let
        players =
            model.players
                ++ [ model.active_player ]
                |> List.tail
                |> Maybe.withDefault []

        active_player =
            List.head model.players
                |> Maybe.withDefault model.active_player
    in
        { model | buffer_time = 120, players = players, active_player = active_player }


updatePass : Model -> Model
updatePass model =
    let
        old_active_player =
            model.active_player

        new_active_player =
            { old_active_player | passed = Just (model.num_passed + 1) }

        new_model =
            { model | active_player = new_active_player, num_passed = model.num_passed + 1 }
    in
        if model.num_passed < List.length model.players then
            updateEndTurn new_model
        else
            updateReset new_model


updateReset : Model -> Model
updateReset model =
    let
        all_players =
            model.active_player
                :: model.players
                |> List.sortBy (.passed >> unsaveUnwrap)
                |> List.map (\p -> { p | passed = Nothing })

        active_player =
            all_players |> List.head |> unsaveUnwrap

        players =
            all_players |> List.tail |> unsaveUnwrap
    in
        { model | players = players, active_player = active_player, buffer_time = 120, num_passed = 0 }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none
    else
        Time.every Time.second (\_ -> TickDown)


view : Model -> Html Msg
view model =
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


unsaveUnwrap : Maybe a -> a
unsaveUnwrap aMaybe =
    case aMaybe of
        Just x ->
            x

        Nothing ->
            Debug.crash "This Maybe was empty."
