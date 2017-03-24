module Update exposing (update, setupGame)

import Types exposing (..)
import Return exposing (Return, singleton)
import Parser


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

        BufferTimeInput buffer_time ->
            case String.toInt buffer_time of
                Ok seconds ->
                    let
                        config =
                            model.config

                        new_config =
                            { config | buffer_time_initial = seconds }
                    in
                        Setup { model | config = new_config }
                            |> singleton

                Err _ ->
                    singleton <| Setup model

        TimeLeftInput time_left ->
            Setup { model | time_left = time_left }
                |> singleton

        KeepBufferInput value ->
            let
                config =
                    model.config

                new_config =
                    { config | keep_buffer = value }
            in
                Setup { model | config = new_config }
                    |> singleton

        PassInput value ->
            let
                config =
                    model.config

                new_config =
                    { config | passing_allowed = value }
            in
                Setup { model | config = new_config }
                    |> singleton

        PassedPlayInput value ->
            let
                config =
                    model.config

                new_config =
                    { config | passed_playing = value }
            in
                Setup { model | config = new_config }
                    |> singleton

        PassedPlayTime play_time ->
            case String.toInt play_time of
                Ok seconds ->
                    let
                        config =
                            model.config

                        new_config =
                            { config | passed_playing_time = seconds }
                    in
                        Setup { model | config = new_config }
                            |> singleton

                Err _ ->
                    singleton <| Setup model

        StartGame gameModel ->
            Ingame gameModel
                |> singleton

        _ ->
            singleton (Setup model)


setupGame : SetupModel -> Result String GameModel
setupGame setupModel =
    let
        all_players =
            Parser.playerList setupModel.name_input

        active_player =
            Result.andThen (List.head >> Result.fromMaybe "Es muss mindestens einen Spieler geben") all_players

        players =
            Result.andThen (List.tail >> Result.fromMaybe "Es muss mindestens einen Spieler geben") all_players

        buffer_time =
            setupModel.config.buffer_time_initial

        time_left =
            String.toInt setupModel.time_left
    in
        Result.map3
            (\active_player players time_left ->
                { active_player = Player active_player time_left Nothing
                , buffer_time = buffer_time
                , num_passed = 0
                , players = List.map (\name -> Player name time_left Nothing) players
                , paused = True
                , config = setupModel.config
                }
            )
            active_player
            players
            time_left


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
        else if active_player.passed == Nothing && model.config.passing_allowed then
            updatePass model
        else
            endTurn model


endTurn : GameModel -> GameModel
endTurn model =
    model |> transferBuffer |> rotatePlayers


endSuperturn : GameModel -> GameModel
endSuperturn model =
    model |> transferBuffer |> rearrangePlayers


transferBuffer : GameModel -> GameModel
transferBuffer gameModel =
    if gameModel.config.keep_buffer then
        let
            active_player =
                gameModel.active_player

            new_active_player =
                { active_player | time_left = active_player.time_left + gameModel.buffer_time }
        in
            { gameModel | active_player = new_active_player, buffer_time = 0 }
    else
        gameModel


rearrangePlayers : GameModel -> GameModel
rearrangePlayers model =
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
        { model | players = players, active_player = active_player, buffer_time = model.config.buffer_time_initial, num_passed = 0 }


rotatePlayers : GameModel -> GameModel
rotatePlayers model =
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
        { model | buffer_time = model.config.buffer_time_initial, players = players, active_player = active_player }


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


unsaveUnwrap : Maybe a -> a
unsaveUnwrap aMaybe =
    case aMaybe of
        Just x ->
            x

        Nothing ->
            Debug.crash "This Maybe was empty."
