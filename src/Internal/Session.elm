module Internal.Session exposing (..)

import Date exposing (Date)
import Internal.Object as Object exposing (Object)
import Internal.ObjectId as ObjectId exposing (ObjectId)
import Internal.Pointer as Pointer exposing (Pointer)
import Internal.Request as Request exposing (Request, request)
import Internal.SessionToken as SessionToken exposing (SessionToken)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse.Decode as Decode
import Parse.Encode as Encode


type alias Session user =
    { sessionToken : SessionToken
    , user : Pointer user
    , createdWith : CreatedWith
    , restricted : Bool
    , expiresAt : Date
    , installationId : String
    }


encodeSession : Session user -> Value
encodeSession session =
    Encode.object
        [ ( "sessionToken", Encode.sessionToken session.sessionToken )
        , ( "user", Encode.pointer "_User" session.user )
        , ( "createdWith", encodeCreatedWith session.createdWith )
        , ( "restricted", Encode.bool session.restricted )
        , ( "expiresAt", Encode.date session.expiresAt )
        , ( "installationId", Encode.string session.installationId )
        ]


sessionDecoder : Decoder (Object (Session user))
sessionDecoder =
    Decode.decode
        (\objectId createdAt updatedAt sessionToken user createdWith restricted expiresAt installationId ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , sessionToken = sessionToken
            , user = user
            , createdWith = createdWith
            , restricted = restricted
            , expiresAt = expiresAt
            , installationId = installationId
            }
        )
        |> Decode.required "objectId" Decode.objectId
        |> Decode.required "createdAt" Decode.date
        |> Decode.required "updatedAt" Decode.date
        |> Decode.required "sessionToken" Decode.sessionToken
        |> Decode.required "user" (Decode.pointer "_User")
        |> Decode.required "createdWith" createdWithDecoder
        |> Decode.required "restricted" Decode.bool
        |> Decode.required "expiresAt" Decode.date
        |> Decode.required "installationid" Decode.string


createSession :
    Session user
    -> Request
        { createdAt : Date
        , createdWith : CreatedWith
        , objectId : ObjectId (Session user)
        , restricted : Bool
        , sessionToken : SessionToken
        }
createSession session =
    request
        { method = "POST"
        , endpoint = "/sessions"
        , body = Just (encodeSession session)
        , decoder = createDecoder
        }


getSession : ObjectId (Session user) -> Request (Object (Session user))
getSession objectId =
    request
        { method = "GET"
        , endpoint = "/sessions/" ++ ObjectId.toString objectId
        , body = Nothing
        , decoder = sessionDecoder
        }


updateSession : (b -> Value) -> ObjectId a -> b -> Request { updatedAt : Date }
updateSession encodeObject objectId object =
    request
        { method = "PUT"
        , endpoint = "/sessions/" ++ ObjectId.toString objectId
        , body = Just (encodeObject object)
        , decoder = Request.putDecoder
        }


getSessions : Request (List (Object (Session user)))
getSessions =
    request
        { method = "GET"
        , endpoint = "/sessions"
        , body = Nothing
        , decoder = Decode.list sessionDecoder
        }


deleteSession : ObjectId (Session user) -> Request {}
deleteSession objectId =
    request
        { method = "DELETE"
        , endpoint = "/sessions/" ++ ObjectId.toString objectId
        , body = Nothing
        , decoder = Decode.succeed {}
        }


createDecoder :
    Decoder
        { createdAt : Date
        , createdWith : CreatedWith
        , objectId : ObjectId (Session user)
        , restricted : Bool
        , sessionToken : SessionToken
        }
createDecoder =
    Decode.decode
        (\createdAt createdWith objectId restricted sessionToken ->
            { createdAt = createdAt
            , createdWith = createdWith
            , objectId = objectId
            , restricted = restricted
            , sessionToken = sessionToken
            }
        )
        |> Decode.required "createdAt" Decode.date
        |> Decode.required "createdWith" createdWithDecoder
        |> Decode.required "objectId" Decode.objectId
        |> Decode.required "restricted" Decode.bool
        |> Decode.required "sessionToken" Decode.sessionToken


type CreatedWith
    = CreatedWith
        { action : Action
        , authProvider : AuthProvider
        }


encodeCreatedWith : CreatedWith -> Value
encodeCreatedWith createdWith =
    case createdWith of
        CreatedWith createdWith ->
            Encode.object
                [ ( "action", encodeAction createdWith.action )
                , ( "authProvider", encodeAuthProvider createdWith.authProvider )
                ]


createdWithDecoder : Decoder CreatedWith
createdWithDecoder =
    Decode.decode
        (\action authProvider ->
            CreatedWith { action = action, authProvider = authProvider }
        )
        |> Decode.required "action" actionDecoder
        |> Decode.required "authProvider" authProviderDecoder


type Action
    = Signup
    | Login
    | Create
    | Upgrade


encodeAction : Action -> Value
encodeAction action =
    Encode.string <|
        case action of
            Signup ->
                "signup"

            Login ->
                "login"

            Create ->
                "create"

            Upgrade ->
                "upgrade"


actionDecoder : Decoder Action
actionDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "signup" ->
                        Decode.succeed Signup

                    "login" ->
                        Decode.succeed Login

                    "create" ->
                        Decode.succeed Create

                    "upgrade" ->
                        Decode.succeed Upgrade

                    _ ->
                        [ "we expected a string of "
                        , "password', 'anonymous', 'facebook', 'twitter'"
                        , " but the string is '"
                        , string
                        , "'"
                        ]
                            |> String.concat
                            |> Decode.fail
            )


type AuthProvider
    = Password
    | Anonymous
    | Facebook
    | Twitter


encodeAuthProvider : AuthProvider -> Value
encodeAuthProvider authProvider =
    Encode.string <|
        case authProvider of
            Password ->
                "password"

            Anonymous ->
                "anonymous"

            Facebook ->
                "facebook"

            Twitter ->
                "twitter"


authProviderDecoder : Decoder AuthProvider
authProviderDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "password" ->
                        Decode.succeed Password

                    "anonymous" ->
                        Decode.succeed Anonymous

                    "facebook" ->
                        Decode.succeed Facebook

                    "twitter" ->
                        Decode.succeed Twitter

                    _ ->
                        [ "we expected a string of '"
                        , "password', 'anonymous', 'facebook', 'twitter'"
                        , "' but the string is '"
                        , string
                        , "'"
                        ]
                            |> String.concat
                            |> Decode.fail
            )
