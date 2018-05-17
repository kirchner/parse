module Internal.Analytics exposing (Event, post, postAt)

import Date exposing (Date)
import Internal.Request exposing (Request, request)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Parse.Decode as Decode
import Parse.Encode as Encode


type alias Event a =
    { a
        | eventName : String
    }


{-|
-}
post : (Event a -> List ( String, Value )) -> Event a -> Request {}
post encode event =
    request
        { method = "POST"
        , endpoint = "/events/" ++ event.eventName
        , body = Just (Encode.object (encode event))
        , decoder = Decode.succeed {}
        }


{-|
@todo(aforemny) Encoders *should* be a -> List (String, Value)
-}
postAt : (Event a -> List ( String, Value )) -> Date -> Event a -> Request {}
postAt encode date event =
    request
        { method = "POST"
        , endpoint = "/events/" ++ event.eventName
        , body = Just (Encode.object (( "at", Encode.date date ) :: encode event))
        , decoder = Decode.succeed {}
        }
