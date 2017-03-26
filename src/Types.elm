module Types exposing (..)


type Model
    = Ingame GameModel
    | Setup SetupModel


type alias SetupModel =
    { name_input : String
    , time_left : Int
    , config : Config
    }


type alias GameModel =
    { active_player : Player
    , buffer_time : Int
    , num_passed : Int
    , players : List Player
    , paused : Bool
    , config : Config
    , disabled_players : List Player
    }


type alias Config =
    { buffer_time_initial : Int
    , keep_buffer : Bool
    , passing_allowed : Bool
    , passed_playing : Bool
    , passed_playing_time : Int
    , rearrangement : Rearrangement
    }


initialConfig : Config
initialConfig =
    { buffer_time_initial = 120
    , keep_buffer = False
    , passing_allowed = True
    , passed_playing = False
    , passed_playing_time = 120
    , rearrangement = StartPlayer
    }


type Rearrangement
    = Static
    | StartPlayer
    | PassOrder


type alias Player =
    { name : String
    , time_left : Int
    , passed : Maybe Int
    , position : Int
    }


type Msg
    = EndTurn
    | Pass
    | TickDown
    | Pause Bool
    | NameInput String
    | BufferTimeInput String
    | TimeLeftInput String
    | StartGame GameModel
    | PassInput Bool
    | PassedPlayInput Bool
    | PassedPlayTime String
    | RearrangementInput Rearrangement
    | KeepBufferInput Bool
