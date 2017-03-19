module Update exposing (update)

import Types exposing (..)
import Return exposing (Return, singleton)


update : Msg -> Model -> Return Msg Model
update msg model =
    case model of
        Ingame gameModel ->
            updateGame msg gameModel |> Return.map Ingame

        Setup setupModel ->
            updateSetup msg setupModel


updateSetup : Msg -> SetupModel -> Return Msg Model
updateSetup msg model =
    case msg of
        NameInput new_name ->
            Setup { model | name_input = new_name }
                |> singleton

        SubmitName ->
            Setup { model | name_input = "", player_names = model.player_names ++ [ model.name_input ] }
                |> singleton

        StartGame ->
            let
                make_player name =
                    Player name 600 Nothing

                all_players =
                    List.map make_player model.player_names

                active_player =
                    List.head all_players |> unsaveUnwrap

                players =
                    List.tail all_players |> unsaveUnwrap
            in
                Ingame
                    { active_player = active_player
                    , buffer_time = 120
                    , num_passed = 0
                    , players = players
                    , paused = True
                    }
                    |> singleton

        _ ->
            singleton (Setup model)


updateGame : Msg -> GameModel -> Return Msg GameModel
updateGame msg model =
    case msg of
        EndTurn ->
            singleton (endTurn model)

        Pass ->
            singleton (updatePass model)

        TickDown ->
            singleton (tickDown model)

        Pause pauseState ->
            singleton (setPaused pauseState model)

        _ ->
            singleton model


setPaused : Bool -> GameModel -> GameModel
setPaused pauseState model =
    { model | paused = pauseState }


tickDown : GameModel -> GameModel
tickDown model =
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
            endTurn model


endTurn : GameModel -> GameModel
endTurn model =
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


updatePass : GameModel -> GameModel
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
            endTurn new_model
        else
            endSuperturn new_model


endSuperturn : GameModel -> GameModel
endSuperturn model =
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


unsaveUnwrap : Maybe a -> a
unsaveUnwrap aMaybe =
    case aMaybe of
        Just x ->
            x

        Nothing ->
            Debug.crash "This Maybe was empty."
