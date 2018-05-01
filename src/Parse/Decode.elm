module Parse.Decode
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
import Json.Decode as Decode exposing (Decoder)


{-| -}
sessionToken : Decoder SessionToken
sessionToken =
    Decode.map SessionToken Decode.string


{-| -}
objectId : Decoder ObjectId
objectId =
    Decode.map ObjectId Decode.string
