module Parse.Encode
    exposing
        ( objectId
        , sessionToken
        )

{-|

@docs sessionToken

@docs objectId

-}

import Internal.ObjectId exposing (..)
import Internal.SessionToken exposing (..)
import Json.Encode as Encode exposing (Value)


{-| -}
sessionToken : SessionToken -> Value
sessionToken (SessionToken token) =
    Encode.string token


{-| -}
objectId : ObjectId -> Value
objectId (ObjectId id) =
    Encode.string id
