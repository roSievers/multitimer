module Types exposing (..)


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
