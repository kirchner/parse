module Parse
    exposing
        ( Code
        , Config
        , Error
        , Object
        , ObjectId
        , create
        , delete
        , encodeObjectId
        , get
        , objectIdDecoder
        , query
        , update
        )

{-|


# Configuration

@docs Config


# Objects

@docs Object, ObjectId, objectIdDecoder, objectIdEncoder


# REST Actions

@docs create, get, update, delete


# Errors

@docs Error, Code

-}

import Date exposing (Date)
import Http exposing (Request)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)


---- CONFIGURATION


{-| -}
type alias Config =
    { applicationId : String
    , restApiKey : String
    , serverUrl : String
    }



---- OBJECTS


{-| TODO: do we need this?
-}
type alias Object fields =
    { fields
        | objectId : ObjectId
        , createdAt : Date
        , updatedAt : Date
    }


{-| -}
type ObjectId
    = ObjectId String


{-| -}
objectIdDecoder : Decoder ObjectId
objectIdDecoder =
    Decode.map ObjectId Decode.string


{-| -}
encodeObjectId : ObjectId -> Value
encodeObjectId (ObjectId id) =
    Encode.string id



---- ACTIONS


{-| -}
create :
    String
    -> (fields -> Value)
    -> Config
    -> fields
    -> Task Error { createdAt : Date, objectId : ObjectId }
create className encodeFields config fields =
    request config
        { method = "POST"
        , urlSuffix = className
        , body = Http.jsonBody (encodeFields fields)
        , responseDecoder =
            Decode.map2 (\createdAt objectId -> { createdAt = createdAt, objectId = objectId })
                (Decode.field "createdAt" dateDecoder)
                (Decode.field "objectId" objectIdDecoder)
        }


{-| -}
get :
    String
    -> Decoder fields
    -> Config
    -> ObjectId
    -> Task Error fields
get className fieldsDecoder config (ObjectId id) =
    request config
        { method = "GET"
        , urlSuffix = "/classes/" ++ className ++ "/" ++ id
        , body = Http.emptyBody
        , responseDecoder = fieldsDecoder
        }


{-| -}
update :
    String
    -> (fields -> Value)
    -> Config
    -> ObjectId
    -> fields
    -> Task Error { updatedAt : Date }
update className encodeFields config (ObjectId id) fields =
    request config
        { method = "PUT"
        , urlSuffix = className ++ "/" ++ id
        , body = Http.jsonBody (encodeFields fields)
        , responseDecoder =
            Decode.map (\updatedAt -> { updatedAt = updatedAt })
                (Decode.field "updatedAt" dateDecoder)
        }


{-| -}
delete :
    String
    -> Config
    -> ObjectId
    -> Task Error ()
delete className config (ObjectId id) =
    request config
        { method = "DELETE"
        , urlSuffix = className ++ "/" ++ id
        , body = Http.emptyBody
        , responseDecoder = Decode.succeed ()
        }


{-| -}
query :
    String
    -> Decoder fields
    -> Config
    -> Task Error (List fields)
query className fieldsDecoder config =
    request config
        { method = "GET"
        , urlSuffix = className
        , body = Http.emptyBody
        , responseDecoder = Decode.field "results" (Decode.list fieldsDecoder)
        }



-- INTERNAL HELPER


request :
    Config
    ->
        { method : String
        , urlSuffix : String
        , body : Http.Body
        , responseDecoder : Decoder a
        }
    -> Task Error a
request config { method, urlSuffix, body, responseDecoder } =
    Http.request
        { method = method
        , headers = defaultHeaders config
        , url = config.serverUrl ++ "/classes/" ++ urlSuffix
        , body = body
        , expect =
            Http.expectJson <|
                responseDecoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.toTask
        |> Task.mapError
            (\httpError ->
                case httpError of
                    Http.BadStatus { status, body } ->
                        case Decode.decodeString errorDecoder body of
                            Ok parseError ->
                                ParseError parseError

                            Err decodeError ->
                                BadError decodeError

                    _ ->
                        HttpError httpError
            )


defaultHeaders : Config -> List Http.Header
defaultHeaders config =
    [ Http.header "X-Parse-Application-Id" config.applicationId
    , Http.header "X-Parse-REST-API-Key" config.restApiKey
    ]



---- ERRORS


{-| -}
type Error
    = ParseError
        { code : Code
        , error : String
        }
    | HttpError Http.Error
    | BadError String


{-| -}
type Code
    = UserInvalidLoginParams
    | InvalidQuery


errorDecoder : Decoder { code : Code, error : String }
errorDecoder =
    Decode.map2 (\code error -> { code = code, error = error })
        (Decode.field "code" codeDecoder)
        (Decode.field "error" Decode.string)


codeDecoder : Decoder Code
codeDecoder =
    Decode.int
        |> Decode.andThen
            (\rawCode ->
                case rawCode of
                    101 ->
                        Decode.succeed UserInvalidLoginParams

                    102 ->
                        Decode.succeed InvalidQuery

                    _ ->
                        [ "'"
                        , toString rawCode
                        , "' is not a valid error code"
                        ]
                            |> String.concat
                            |> Decode.fail
            )



---- HELPER


dateDecoder : Decoder Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\rawDate ->
                case Date.fromString rawDate of
                    Ok date ->
                        Decode.succeed date

                    Err error ->
                        [ "could not parse date: "
                        , error
                        ]
                            |> String.concat
                            |> Decode.fail
            )
