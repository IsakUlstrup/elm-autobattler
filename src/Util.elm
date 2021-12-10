module Util exposing (boolToString)


boolToString : Bool -> String
boolToString flag =
    if flag then
        "True"

    else
        "False"
