module Internal.Request exposing (Request, request, toTask, send, postDecoder, putDecoder, requestWithAdditionalHeaders)

import Date exposing (Date)
import Http
import Internal.Config as Config exposing (Config)
import Internal.Error as Error exposing (Error)
import Internal.ObjectId as ObjectId exposing (ObjectId)
import Internal.SessionToken as SessionToken
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Parse.Decode as Decode
import Task exposing (Task)


type Request a
    = Request { runRequest : Config -> Http.Request a }


headers : Config -> List Http.Header
headers config =
    List.filterMap identity
        [ Just (Http.header "X-Parse-Application-Id" config.applicationId)
        , config.restAPIKey
            |> Maybe.map (Http.header "X-Parse-REST-API-Key")
        , config.javascriptKey
            |> Maybe.map (Http.header "X-Parse-JavaScript-Key")
        , config.clientKey
            |> Maybe.map (Http.header "X-Parse-Client-Key")
        , config.windowsKey
            |> Maybe.map (Http.header "X-Parse-Windows-Key")
        , config.masterKey
            |> Maybe.map (Http.header "X-Parse-Master-Key")
        , config.sessionToken
            |> Maybe.map SessionToken.toString
            |> Maybe.map (Http.header "X-Parse-Session-Token")
        ]


request :
    { method : String
    , endpoint : String
    , body : Maybe Value
    , decoder : Decoder a
    }
    -> Request a
request { method, endpoint, body, decoder } =
    requestWithAdditionalHeaders
        { method = method
        , additionalHeaders = []
        , endpoint = endpoint
        , body = body
        , decoder = decoder
        }


requestWithAdditionalHeaders :
    { method : String
    , additionalHeaders : List Http.Header
    , endpoint : String
    , body : Maybe Value
    , decoder : Decoder a
    }
    -> Request a
requestWithAdditionalHeaders { method, additionalHeaders, endpoint, body, decoder } =
    Request <|
        { runRequest =
            \config ->
                Http.request
                    { method = method
                    , headers = headers config ++ additionalHeaders
                    , url = config.serverUrl ++ endpoint
                    , body =
                        Maybe.map Http.jsonBody body
                            |> Maybe.withDefault Http.emptyBody
                    , expect = Http.expectJson decoder
                    , timeout = Nothing
                    , withCredentials = False
                    }
        }



-- TODO:?
--        |> Http.toTask
--        |> Task.mapError
--            (\httpError ->
--                case httpError of
--                    Http.BadStatus { status, body } ->
--                        case Decode.decodeString errorDecoder body of
--                            Ok parseError ->
--                                ParseError parseError
--
--                            Err decodeError ->
--                                DecodeError decodeError
--
--                    _ ->
--                        HttpError httpError
--            )


toTask : Config -> Request a -> Task Error a
toTask config request =
    case request of
        Request { runRequest } ->
            Http.toTask (runRequest config)
                |> Task.mapError Error.HttpError


send : Config -> (Result Error a -> m) -> Request a -> Cmd m
send config handle request =
    case request of
        Request { runRequest } ->
            Http.send (handle << Result.mapError Error.HttpError) (runRequest config)


postDecoder : Decoder { objectId : ObjectId a, createdAt : Date }
postDecoder =
    Decode.map2 (\createdAt objectId -> { createdAt = createdAt, objectId = objectId })
        (Decode.field "createdAt" Decode.date)
        (Decode.field "objectId" Decode.objectId)


putDecoder : Decoder { updatedAt : Date }
putDecoder =
    Decode.map (\updatedAt -> { updatedAt = updatedAt })
        (Decode.field "updatedAt" Decode.date)
