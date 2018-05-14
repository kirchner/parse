module Internal.SessionToken
    exposing
        ( SessionToken(..)
        , toString
        , fromString
        , decode
        , encode
        )

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type SessionToken
    = SessionToken String


toString : SessionToken -> String
toString sessionToken =
    case sessionToken of
        SessionToken sessionToken ->
            sessionToken


fromString : String -> SessionToken
fromString sessionToken =
    SessionToken sessionToken


decode : Decoder SessionToken
decode =
    Decode.map fromString Decode.string


encode : SessionToken -> Value
encode sessionToken =
    Encode.string (toString sessionToken)
