module Update exposing (update, setupGame)

import Types exposing (..)
import Return exposing (Return, singleton)
import Parser


update : Msg -> Model -> Return Msg Model
update msg model =
    case model of
        Ingame gameModel ->
            updateGame msg gameModel |> Ingame |> singleton

        Setup setupModel ->
            updateSetup msg setupModel |> singleton


updateSetup : Msg -> SetupModel -> Model
updateSetup msg model =
    case msg of
        NameInput new_name ->
            Setup { model | name_input = new_name }

        TimeLeftInput time_left ->
            case String.toInt time_left of
                Ok seconds ->
                    Setup { model | time_left = seconds }

                Err _ ->
                    Setup model

        StartGame gameModel ->
            Ingame gameModel

        message ->
            Setup { model | config = updateConfig message model.config }


updateConfig : Msg -> Config -> Config
updateConfig msg config =
    case msg of
        BufferTimeInput buffer_time ->
            case String.toInt buffer_time of
                Ok seconds ->
                    { config | buffer_time_initial = seconds }

                Err _ ->
                    config

        KeepBufferInput value ->
            { config | keep_buffer = value }

        PassInput value ->
            { config | passing_allowed = value }

        PassedPlayInput value ->
            { config | passed_playing = value }

        PassedPlayTime play_time ->
            case String.toInt play_time of
                Ok seconds ->
                    { config | passed_playing_time = seconds }

                Err _ ->
                    config

        _ ->
            config


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
            setupModel.time_left
    in
        Result.map2
            (\active_player players ->
                { active_player = Player active_player time_left Nothing 1
                , buffer_time = buffer_time
                , num_passed = 0
                , players = List.indexedMap (\i name -> Player name time_left Nothing (i+2)) players
                , disabled_players =[]
                , paused = True
                , config = setupModel.config
                }
            )
            active_player
            players


updateGame : Msg -> GameModel -> GameModel
updateGame msg model =
    case msg of
        EndTurn ->
            endTurn model

        Pass ->
            updatePass model

        TickDown ->
            tickDown model

        Pause pauseState ->
            setPaused pauseState model

        _ ->
            model


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
        playstyle= model.config.rearrangement

        firstPlayerWillStart list =
            (List.head list |> Maybe.andThen .passed)
            ==Just 1

        all_players =
            case playstyle of
            PassOrder ->
                model.active_player
                    :: model.players ++ model.disabled_players
                    |> List.sortBy (.passed >> unsaveUnwrap)
                    |> List.map (\p -> { p | passed = Nothing })
            
            Static  ->
                model.active_player
                    :: model.players ++ model.disabled_players
                    |> List.sortBy (.position)
                    |> List.map (\p -> { p | passed = Nothing })
            
            StartPlayer ->
                model.active_player
                    :: model.players ++ model.disabled_players
                    |> List.sortBy (.position )
                    |> repeatUntil rotateList firstPlayerWillStart
                    |> List.map (\p -> { p | passed = Nothing })
            
        active_player =
            all_players |> List.head |> unsaveUnwrap

        players =
            all_players |> List.tail |> unsaveUnwrap
    in
        { model | players = players, active_player = active_player, buffer_time = model.config.buffer_time_initial, disabled_players=[], num_passed = 0 }


rotateList: List a -> List a
rotateList list =
    case list of
        head::tail -> tail ++[head]
        _ -> []
    

repeatUntil: (a -> a) -> (a -> Bool) -> a -> a
repeatUntil  f  oracle value =
    if oracle value
    then 
        value
    else
        repeatUntil f oracle (f value)
    

rotatePlayers : GameModel -> GameModel
rotatePlayers model =
    let
        keep_playing = model.active_player.passed == Nothing || model.config.passed_playing
    
        removeActivePlayer = if keep_playing then queueActivePlayer else disableActivePlayer


    in
        model |> removeActivePlayer |> activateFromQueue |> resetBufferTime

resetBufferTime : GameModel -> GameModel
resetBufferTime model =
    let 
        buffer_time= 
            if model.active_player.passed == Nothing
            then
                model.config.buffer_time_initial
            else
                model.config.passed_playing_time
    in 
        {model | buffer_time = buffer_time}

queueActivePlayer : GameModel -> GameModel
queueActivePlayer model =
    {model | players= model.players ++[model.active_player]}

disableActivePlayer : GameModel -> GameModel
disableActivePlayer model =
    {model | disabled_players = model.disabled_players ++ [model.active_player]}

activateFromQueue : GameModel -> GameModel
activateFromQueue model =
    let
        active_player = model.players |> List.head |> Maybe.withDefault model.active_player
        players = model.players |> List.tail |> Maybe.withDefault []
    in 
        {model | active_player= active_player, players=players}



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
        if model.num_passed < List.length model.players + List.length model.disabled_players then
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
