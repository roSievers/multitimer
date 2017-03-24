module Parser exposing (playerList)

import Combine exposing (Parser, whitespace, (*>), (<*))


name : Parser a String
name =
    whitespace *> Combine.regex "[^ ,]([^,]*[^ ,])?" <* whitespace


names : Parser a (List String)
names =
    Combine.sepBy (Combine.string ",") name


playerList : String -> Result String (List String)
playerList inputString =
    case Combine.parse names inputString of
        Err _ ->
            Err "Die Liste von Spielernamen wird nicht verstanden."

        Ok ( _, _, result ) ->
            Ok result
