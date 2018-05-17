module Internal.CloudCode exposing (function, job)

import Internal.Request exposing (Request, request)
import Json.Decode as Decode exposing (Decoder, Value)


function : String -> Decoder a -> Value -> Request a
function name decoder value =
    request
        { method = "POST"
        , endpoint = "/functions/" ++ name
        , body = Just value
        , decoder = Decode.at [ "result" ] decoder
        }


job : String -> Value -> Request {}
job name value =
    request
        { method = "POST"
        , endpoint = "/jobs/" ++ name
        , body = Just value
        , decoder = Decode.succeed {}
        }
