module Update exposing (update)

import Types exposing (..)


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


unsaveUnwrap : Maybe a -> a
unsaveUnwrap aMaybe =
    case aMaybe of
        Just x ->
            x

        Nothing ->
            Debug.crash "This Maybe was empty."
