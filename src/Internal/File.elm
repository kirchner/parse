module Internal.File exposing (..)

import Http
import Internal.Request as Request exposing (Request, request, requestWithAdditionalHeaders)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type File
    = File
        { name : String
        , url : String
        }


name : File -> String
name file =
    case file of
        File { name } ->
            name


url : File -> String
url file =
    case file of
        File { url } ->
            url


encodeFile : File -> Value
encodeFile file =
    case file of
        File file ->
            Encode.object
                [ ( "name", Encode.string file.name )
                , ( "url", Encode.string file.url )
                ]


fileDecoder : Decoder File
fileDecoder =
    Decode.decode (\name url -> File { name = name, url = url })
        |> Decode.required "name" Decode.string
        |> Decode.required "url" Decode.string


type alias ContentType =
    String


uploadFile : String -> ContentType -> Value -> Request File
uploadFile fileName contentType file =
    requestWithAdditionalHeaders
        { method = "POST"
        , additionalHeaders = [ Http.header "Content-Type" contentType ]
        , endpoint = "/files/" ++ Http.encodeUri fileName
        , body = Just file
        , decoder = fileDecoder
        }


deleteFile : File -> Request {}
deleteFile file =
    request
        { method = "DELETE"
        , endpoint = "/files/" ++ name file
        , body = Nothing
        , decoder = Decode.succeed {}
        }
