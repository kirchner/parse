module Parse.LiveQuery exposing (Msg(..))

{-|

@docs Msg

-}


{-| -}
type Msg a
    = Open
    | Close
    | Create a
    | Update a
    | Enter a
    | Leave a
    | Delete a
