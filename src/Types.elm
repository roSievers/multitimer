module Types exposing (..)


type Model
    = Ingame GameModel
    | Setup SetupModel


type alias SetupModel =
    { player_names : List String
    , name_input : String
    }


type alias GameModel =
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
    | NameInput String
    | SubmitName
    | StartGame
