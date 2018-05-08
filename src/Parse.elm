module Parse
    exposing
        ( Cause
            ( AccountAlreadyLinked
            , AggregateError
            , CacheMiss
            , CommandUnavailable
            , ConnectionFailed
            , DuplicateValue
            , EmailMissing
            , EmailNotFound
            , EmailTaken
            , ExceededQuota
            , FileDeleteError
            , FileReadError
            , FileSaveError
            , FileTooLarge
            , IncorrectType
            , InternalServerError
            , InvalidACL
            , InvalidChannelName
            , InvalidClassName
            , InvalidContentType
            , InvalidEmailAddress
            , InvalidEventName
            , InvalidFileName
            , InvalidImageData
            , InvalidJSON
            , InvalidKeyName
            , InvalidLinkedSession
            , InvalidNestedKey
            , InvalidPointer
            , InvalidPushTimeError
            , InvalidQuery
            , InvalidRoleName
            , InvalidSchemaOperation
            , InvalidSessionToken
            , LinkedIdMissing
            , MissingContentLength
            , MissingContentType
            , MissingObjectId
            , MustCreateUserThroughSignup
            , NotInitialized
            , ObjectNotFound
            , ObjectTooLarge
            , OperationForbidden
            , OtherCause
            , Passwordmissing
            , PushMisconfigured
            , RequestLimitExceeded
            , ScriptFailed
            , SessionMissing
            , Timeout
            , UnsavedFileError
            , UnsupportedService
            , UsernameMissing
            , UsernameTaken
            , ValidationError
            , XDomainRequest
            )
        , Config
        , Constraint
        , Error
            ( DecodeError
            , HttpError
            , ParseError
            )
        , Object
        , ObjectId
        , Query
        , SessionToken
        , and
        , code
        , create
        , delete
        , deleteUser
        , emailVerificationRequest
        , emptyQuery
        , encodeQuery
        , equalTo
        , exists
        , get
        , getCurrentUser
        , getUser
        , greaterThan
        , greaterThanOrEqualTo
        , lessThan
        , lessThanOrEqualTo
        , logIn
        , notEqualTo
        , or
        , passwordResetRequest
        , query
        , regex
        , signUp
        , simpleConfig
        , update
        , updateUser
        )

{-|


# Configuration

@docs Config, simpleConfig

@docs SessionToken


# REST Actions

@docs create, get, update, delete

@docs Object
@docs ObjectId


# Queries

@docs query, Query, emptyQuery, encodeQuery


## Constraints

@docs Constraint

@docs and, or, exists

@docs equalTo, notEqualTo, regex

@docs lessThan, lessThanOrEqualTo, greaterThan, greaterThanOrEqualTo


# Users

@docs signUp

@docs logIn

@docs emailVerificationRequest, passwordResetRequest

@docs getUser, getCurrentUser, updateUser, deleteUser


# Errors

@docs Error, Cause, code

-}

import Date exposing (Date)
import Dict
import Http exposing (Request)
import Internal.ObjectId as Internal
import Internal.SessionToken as Internal
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Parse.Decode as Decode
import Task exposing (Task)


---- CONFIGURATION


{-| -}
type alias Config =
    { serverUrl : String
    , applicationId : String
    , restAPIKey : Maybe String
    , javascriptKey : Maybe String
    , clientKey : Maybe String
    , windowsKey : Maybe String
    , masterKey : Maybe String
    , sessionToken : Maybe SessionToken
    }


{-| -}
simpleConfig : String -> String -> Config
simpleConfig serverUrl applicationId =
    Config
        serverUrl
        applicationId
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing


{-| -}
type alias SessionToken =
    Internal.SessionToken



---- OBJECTS


{-| -}
type alias ObjectId =
    Internal.ObjectId


type alias Object a =
    { a | objectId : ObjectId
        , createdAt : Date
        , updatedAt : Date
    }



---- ACTIONS


{-| -}
create :
    String
    -> (object -> Value)
    -> Config
    -> object
    -> Task Error { createdAt : Date, objectId : ObjectId }
create className encodeObject config object =
    request config
        { method = "POST"
        , urlSuffix = className
        , body = Http.jsonBody (encodeObject object)
        , responseDecoder =
            Decode.map2 (\createdAt objectId -> { createdAt = createdAt, objectId = objectId })
                (Decode.field "createdAt" Decode.date)
                (Decode.field "objectId" Decode.objectId)
        }


{-| -}
get :
    String
    -> Decoder object
    -> Config
    -> ObjectId
    -> Task Error object
get className objectDecoder config (Internal.ObjectId id) =
    request config
        { method = "GET"
        , urlSuffix = className ++ "/" ++ id
        , body = Http.emptyBody
        , responseDecoder = objectDecoder
        }


{-| -}
update :
    String
    -> (object -> Value)
    -> Config
    -> ObjectId
    -> object
    -> Task Error { updatedAt : Date }
update className encodeObject config (Internal.ObjectId id) object =
    request config
        { method = "PUT"
        , urlSuffix = className ++ "/" ++ id
        , body = Http.jsonBody (encodeObject object)
        , responseDecoder =
            Decode.map (\updatedAt -> { updatedAt = updatedAt })
                (Decode.field "updatedAt" Decode.date)
        }


{-| -}
delete :
    String
    -> Config
    -> ObjectId
    -> Task Error ()
delete className config (Internal.ObjectId id) =
    request config
        { method = "DELETE"
        , urlSuffix = className ++ "/" ++ id
        , body = Http.emptyBody
        , responseDecoder = Decode.succeed ()
        }



---- USERS


{-| -}
signUp :
    (user -> List ( String, Value ))
    -> Config
    -> String
    -> String
    -> user
    ->
        Task Error
            { createdAt : Date
            , objectId : ObjectId
            , sessionToken : SessionToken
            , location : String
            }
signUp encodeUser config username password user =
    let
        body =
            [ ( "username", Encode.string username )
            , ( "password", Encode.string password )
            ]
                ++ encodeUser user
                |> Encode.object

        bodyDecoder location =
            Decode.map3
                (\createdAt objectId sessionToken ->
                    { createdAt = createdAt
                    , objectId = objectId
                    , sessionToken = sessionToken
                    , location = location
                    }
                )
                (Decode.field "createdAt" Decode.date)
                (Decode.field "objectId" Decode.objectId)
                (Decode.field "sessionToken" Decode.sessionToken)
    in
    Http.request
        { method = "POST"
        , headers =
            Http.header "X-Parse-Revocable-Session" (toString 1)
                :: defaultHeaders config
        , url = config.serverUrl ++ "/users"
        , body = Http.jsonBody body
        , expect =
            Http.expectStringResponse <|
                \{ headers, body } ->
                    case Dict.get "Location" (Debug.log "headers" headers) of
                        Just location ->
                            Decode.decodeString (bodyDecoder location) body

                        Nothing ->
                            Decode.decodeString (bodyDecoder "TODO missing location header") body
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
                                DecodeError decodeError

                    _ ->
                        HttpError httpError
            )


{-| -}
logIn :
    Decoder user
    -> Config
    -> String
    -> String
    ->
        Task Error
            { user : user
            , sessionToken : SessionToken
            }
logIn userDecoder config username password =
    let
        responseDecoder =
            Decode.map2
                (\user sessionToken ->
                    { user = user
                    , sessionToken = sessionToken
                    }
                )
                userDecoder
                (Decode.field "sessionToken" Decode.sessionToken)
    in
    Http.request
        { method = "GET"
        , headers =
            Http.header "X-Parse-Revocable-Session" (toString 1)
                :: defaultHeaders config
        , url =
            [ config.serverUrl
            , "/login?username="
            , Http.encodeUri username
            , "&password="
            , Http.encodeUri password
            ]
                |> String.concat
        , body = Http.emptyBody
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
                                DecodeError decodeError

                    _ ->
                        HttpError httpError
            )


{-| -}
emailVerificationRequest : Config -> String -> Task Error ()
emailVerificationRequest config email =
    Http.request
        { method = "POST"
        , headers = defaultHeaders config
        , url = config.serverUrl ++ "/verificationEmailRequest"
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.succeed ())
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
                                DecodeError decodeError

                    _ ->
                        HttpError httpError
            )


{-| -}
passwordResetRequest : Config -> String -> Task Error ()
passwordResetRequest config email =
    Http.request
        { method = "POST"
        , headers = defaultHeaders config
        , url = config.serverUrl ++ "/requestPasswordReset"
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.succeed ())
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
                                DecodeError decodeError

                    _ ->
                        HttpError httpError
            )


{-| -}
getUser : Decoder user -> Config -> ObjectId -> Task Error user
getUser userDecoder config (Internal.ObjectId id) =
    request config
        { method = "GET"
        , urlSuffix = "/users/" ++ id
        , body = Http.emptyBody
        , responseDecoder = userDecoder
        }


{-| -}
getCurrentUser : Decoder user -> Config -> Task Error user
getCurrentUser userDecoder config =
    request config
        { method = "GET"
        , urlSuffix = "/users/me"
        , body = Http.emptyBody
        , responseDecoder = userDecoder
        }


{-| -}
updateUser :
    (user -> Value)
    -> Config
    -> ObjectId
    -> user
    -> Task Error { updatedAt : Date }
updateUser encodeUser config (Internal.ObjectId id) user =
    request config
        { method = "PUT"
        , urlSuffix = "/users/" ++ id
        , body = Http.jsonBody (encodeUser user)
        , responseDecoder =
            Decode.map (\updatedAt -> { updatedAt = updatedAt })
                (Decode.field "updatedAt" Decode.date)
        }


{-| -}
deleteUser :
    Config
    -> ObjectId
    -> Task Error ()
deleteUser config (Internal.ObjectId id) =
    request config
        { method = "DELETE"
        , urlSuffix = "/users/" ++ id
        , body = Http.emptyBody
        , responseDecoder = Decode.succeed ()
        }



---- QUERIES


{-| -}
query :
    Decoder object
    -> Config
    -> Query
    -> Task Error (List object)
query objectDecoder config query =
    request config
        { method = "GET"
        , urlSuffix = query.className ++ "?" ++ serializeQuery query
        , body = Http.emptyBody
        , responseDecoder = Decode.field "results" (Decode.list objectDecoder)
        }


{-| -}
type alias Query =
    { className : String
    , whereClause : Constraint

    -- RESPONSE
    , order : List String
    , keys : List String
    , include : List String
    , count : Bool

    -- PAGINATION
    , limit : Maybe Int
    , skip : Maybe Int
    }


{-| -}
emptyQuery : String -> Query
emptyQuery className =
    { className = className
    , whereClause = And []
    , order = []
    , keys = []
    , include = []
    , count = False
    , limit = Nothing
    , skip = Nothing
    }


{-| -}
encodeQuery : Query -> Value
encodeQuery query =
    let
        required key value =
            Just ( key, value )

        optional key encode value =
            Maybe.map (\value -> ( key, encode value )) value
    in
    [ required "className" (Encode.string query.className)
    , required "where" (encodeConstraint query.whereClause)
    , if query.include == [] then
        Nothing
      else
        required "include" (Encode.string (String.join "," query.include))
    , if query.count then
        required "count" (Encode.int 1)
      else
        Nothing
    , if query.keys == [] then
        Nothing
      else
        required "keys" (Encode.string (String.join "," query.keys))
    , optional "limit" Encode.int query.limit
    , query.skip
        |> Maybe.andThen
            (\skip ->
                if skip <= 0 then
                    Nothing
                else
                    required "skip" (Encode.int skip)
            )
    , if query.order == [] then
        Nothing
      else
        required "order" (Encode.string (String.join "," query.order))
    ]
        |> List.filterMap identity
        |> Encode.object


{-| -}
type Constraint
    = And (List Constraint)
    | Or (List Constraint)
    | Field String (List FieldConstraint)


type FieldConstraint
    = Exists
    | EqualTo String
    | NotEqualTo String
    | LessThan Float
    | LessThanOrEqualTo Float
    | GreaterThan Float
    | GreaterThanOrEqualTo Float
    | Regex String



{- TODO: missing constraints:

   $in          Contained In
   $nin         Not Contained in
   $select      This matches a value for a key in the result of a different query
   $dontSelect  Requires that a key’s value not match a value for a key in the result of a different query
   $all         Contains all of the given values
   $text        Performs a full text search on indexed fields
-}


{-| -}
and : List Constraint -> Constraint
and constraints =
    let
        flattenNestedAnds constraint flatConstraints =
            case constraint of
                And nestedConstraints ->
                    nestedConstraints ++ flatConstraints

                _ ->
                    constraint :: flatConstraints

        mergeFieldConstraints constraint ( fieldConstraints, otherConstraints ) =
            case constraint of
                Field fieldName actualFieldConstraints ->
                    ( Dict.update fieldName
                        (\maybeActualFieldConstraints ->
                            case maybeActualFieldConstraints of
                                Just otherActualFieldConstraints ->
                                    Just <|
                                        actualFieldConstraints
                                            ++ otherActualFieldConstraints

                                Nothing ->
                                    Just actualFieldConstraints
                        )
                        fieldConstraints
                    , otherConstraints
                    )

                _ ->
                    ( fieldConstraints
                    , constraint :: otherConstraints
                    )
    in
    constraints
        |> List.foldr flattenNestedAnds []
        |> List.foldr mergeFieldConstraints ( Dict.empty, [] )
        |> Tuple.mapFirst
            (\fieldConstraints ->
                fieldConstraints
                    |> Dict.foldl
                        (\fieldName actualFieldConstraints result ->
                            Field fieldName actualFieldConstraints
                                :: result
                        )
                        []
            )
        |> (\( fieldConstraints, otherConstraints ) ->
                fieldConstraints ++ otherConstraints
           )
        |> And


{-| -}
or : List Constraint -> Constraint
or =
    Or


{-| -}
exists : String -> Constraint
exists fieldName =
    Field fieldName [ Exists ]


{-| -}
equalTo : String -> String -> Constraint
equalTo fieldName =
    Field fieldName << List.singleton << EqualTo


{-| -}
notEqualTo : String -> String -> Constraint
notEqualTo fieldName =
    Field fieldName << List.singleton << NotEqualTo


{-| -}
lessThan : String -> Float -> Constraint
lessThan fieldName =
    Field fieldName << List.singleton << LessThan


{-| -}
lessThanOrEqualTo : String -> Float -> Constraint
lessThanOrEqualTo fieldName =
    Field fieldName << List.singleton << LessThanOrEqualTo


{-| -}
greaterThan : String -> Float -> Constraint
greaterThan fieldName =
    Field fieldName << List.singleton << GreaterThan


{-| -}
greaterThanOrEqualTo : String -> Float -> Constraint
greaterThanOrEqualTo fieldName =
    Field fieldName << List.singleton << GreaterThanOrEqualTo


{-| -}
regex : String -> String -> Constraint
regex fieldName =
    Field fieldName << List.singleton << Regex



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
                                DecodeError decodeError

                    _ ->
                        HttpError httpError
            )


defaultHeaders : Config -> List Http.Header
defaultHeaders config =
    List.filterMap identity
        [ Just (Http.header "X-Parse-Application-Id" config.applicationId)
        , config.restAPIKey
            |> Maybe.map (Http.header "X-Parse-REST-API-Key")
        , config.sessionToken
            |> Maybe.map
                (\(Internal.SessionToken token) ->
                    Http.header "X-Parse-Session-Token" token
                )
        ]


serializeQuery : Query -> String
serializeQuery query =
    [ [ "where="
      , encodeConstraint query.whereClause
            |> Encode.encode 0
            |> Http.encodeUri
      ]
        |> String.concat
        |> Just
    , if List.isEmpty query.order then
        Nothing
      else
        Just (String.join "," query.order)
    , if List.isEmpty query.keys then
        Nothing
      else
        Just (String.join "," query.keys)
    , if List.isEmpty query.include then
        Nothing
      else
        Just ("include=" ++ String.join "," query.include)
    , if query.count then
        Just "count=1"
      else
        Nothing
    , query.limit
        |> Maybe.map (\limit -> "limit=" ++ toString limit)
    , query.skip
        |> Maybe.map (\skip -> "skip=" ++ toString skip)
    ]
        |> List.filterMap identity
        |> String.join "&"


encodeConstraint : Constraint -> Value
encodeConstraint constraint =
    constraint
        |> encodeConstraintHelp
        |> Encode.object


encodeConstraintHelp : Constraint -> List ( String, Value )
encodeConstraintHelp constraint =
    case constraint of
        And constraints ->
            constraints
                |> List.map encodeConstraintHelp
                |> List.concat

        Or constraints ->
            [ ( "$or"
              , constraints
                    |> List.map (encodeConstraintHelp >> Encode.object)
                    |> Encode.list
              )
            ]

        Field fieldName fieldConstraints ->
            let
                fieldEqualTo =
                    fieldConstraints
                        |> List.filterMap
                            (\constraint ->
                                case constraint of
                                    EqualTo value ->
                                        Just value

                                    _ ->
                                        Nothing
                            )
                        |> List.head
            in
            [ ( fieldName
              , case fieldEqualTo of
                    Just value ->
                        Encode.string value

                    Nothing ->
                        fieldConstraints
                            |> List.filterMap encodeFieldConstraint
                            |> Encode.object
              )
            ]


encodeFieldConstraint : FieldConstraint -> Maybe ( String, Value )
encodeFieldConstraint fieldConstraint =
    case fieldConstraint of
        Exists ->
            Just
                ( "$exists"
                , Encode.bool True
                )

        NotEqualTo string ->
            Just
                ( "$ne"
                , Encode.string string
                )

        Regex regex ->
            Just
                ( "$regex"
                , Encode.string regex
                )

        LessThan float ->
            Just
                ( "$lt"
                , Encode.float float
                )

        LessThanOrEqualTo float ->
            Just
                ( "$lte"
                , Encode.float float
                )

        GreaterThan float ->
            Just
                ( "$gt"
                , Encode.float float
                )

        GreaterThanOrEqualTo float ->
            Just
                ( "$gte"
                , Encode.float float
                )

        EqualTo _ ->
            Nothing



---- ERRORS


{-| -}
type Error
    = ParseError
        { cause : Cause
        , error : String
        }
    | HttpError Http.Error
    | DecodeError String


{-| -}
type Cause
    = OtherCause
    | InternalServerError
    | ConnectionFailed
    | ObjectNotFound
    | InvalidQuery
    | InvalidClassName
    | MissingObjectId
    | InvalidKeyName
    | InvalidPointer
    | InvalidJSON
    | CommandUnavailable
    | NotInitialized
    | IncorrectType
    | InvalidChannelName
    | PushMisconfigured
    | ObjectTooLarge
    | OperationForbidden
    | CacheMiss
    | InvalidNestedKey
    | InvalidFileName
    | InvalidACL
    | Timeout
    | InvalidEmailAddress
    | MissingContentType
    | MissingContentLength
    | InvalidContentType
    | FileTooLarge
    | FileSaveError
    | DuplicateValue
    | InvalidRoleName
    | ExceededQuota
    | ScriptFailed
    | ValidationError
    | InvalidImageData
    | UnsavedFileError
    | InvalidPushTimeError
    | FileDeleteError
    | RequestLimitExceeded
    | InvalidEventName
    | UsernameMissing
    | Passwordmissing
    | UsernameTaken
    | EmailTaken
    | EmailMissing
    | EmailNotFound
    | SessionMissing
    | MustCreateUserThroughSignup
    | AccountAlreadyLinked
    | InvalidSessionToken
    | LinkedIdMissing
    | InvalidLinkedSession
    | UnsupportedService
    | InvalidSchemaOperation
    | AggregateError
    | FileReadError
    | XDomainRequest


errorDecoder : Decoder { cause : Cause, error : String }
errorDecoder =
    Decode.map2 (\cause error -> { cause = cause, error = error })
        (Decode.field "code" causeDecoder)
        (Decode.field "error" Decode.string)


{-| -}
code : Cause -> Int
code cause =
    case cause of
        OtherCause ->
            -1

        InternalServerError ->
            1

        ConnectionFailed ->
            100

        ObjectNotFound ->
            101

        InvalidQuery ->
            102

        InvalidClassName ->
            103

        MissingObjectId ->
            104

        InvalidKeyName ->
            105

        InvalidPointer ->
            106

        InvalidJSON ->
            107

        CommandUnavailable ->
            108

        NotInitialized ->
            109

        IncorrectType ->
            111

        InvalidChannelName ->
            112

        PushMisconfigured ->
            115

        ObjectTooLarge ->
            116

        OperationForbidden ->
            119

        CacheMiss ->
            120

        InvalidNestedKey ->
            121

        InvalidFileName ->
            122

        InvalidACL ->
            123

        Timeout ->
            124

        InvalidEmailAddress ->
            125

        MissingContentType ->
            126

        MissingContentLength ->
            127

        InvalidContentType ->
            128

        FileTooLarge ->
            129

        FileSaveError ->
            130

        DuplicateValue ->
            137

        InvalidRoleName ->
            139

        ExceededQuota ->
            140

        ScriptFailed ->
            141

        ValidationError ->
            142

        InvalidImageData ->
            143

        UnsavedFileError ->
            151

        InvalidPushTimeError ->
            152

        FileDeleteError ->
            153

        RequestLimitExceeded ->
            155

        InvalidEventName ->
            160

        UsernameMissing ->
            200

        Passwordmissing ->
            201

        UsernameTaken ->
            202

        EmailTaken ->
            203

        EmailMissing ->
            204

        EmailNotFound ->
            205

        SessionMissing ->
            206

        MustCreateUserThroughSignup ->
            207

        AccountAlreadyLinked ->
            208

        InvalidSessionToken ->
            209

        LinkedIdMissing ->
            250

        InvalidLinkedSession ->
            251

        UnsupportedService ->
            252

        InvalidSchemaOperation ->
            255

        AggregateError ->
            600

        FileReadError ->
            601

        XDomainRequest ->
            602


causeDecoder : Decoder Cause
causeDecoder =
    Decode.int
        |> Decode.andThen
            (\code ->
                case code of
                    (-1) ->
                        Decode.succeed OtherCause

                    1 ->
                        Decode.succeed InternalServerError

                    100 ->
                        Decode.succeed ConnectionFailed

                    101 ->
                        Decode.succeed ObjectNotFound

                    102 ->
                        Decode.succeed InvalidQuery

                    103 ->
                        Decode.succeed InvalidClassName

                    104 ->
                        Decode.succeed MissingObjectId

                    105 ->
                        Decode.succeed InvalidKeyName

                    106 ->
                        Decode.succeed InvalidPointer

                    107 ->
                        Decode.succeed InvalidJSON

                    108 ->
                        Decode.succeed CommandUnavailable

                    109 ->
                        Decode.succeed NotInitialized

                    111 ->
                        Decode.succeed IncorrectType

                    112 ->
                        Decode.succeed InvalidChannelName

                    115 ->
                        Decode.succeed PushMisconfigured

                    116 ->
                        Decode.succeed ObjectTooLarge

                    119 ->
                        Decode.succeed OperationForbidden

                    120 ->
                        Decode.succeed CacheMiss

                    121 ->
                        Decode.succeed InvalidNestedKey

                    122 ->
                        Decode.succeed InvalidFileName

                    123 ->
                        Decode.succeed InvalidACL

                    124 ->
                        Decode.succeed Timeout

                    125 ->
                        Decode.succeed InvalidEmailAddress

                    126 ->
                        Decode.succeed MissingContentType

                    127 ->
                        Decode.succeed MissingContentLength

                    128 ->
                        Decode.succeed InvalidContentType

                    129 ->
                        Decode.succeed FileTooLarge

                    130 ->
                        Decode.succeed FileSaveError

                    137 ->
                        Decode.succeed DuplicateValue

                    139 ->
                        Decode.succeed InvalidRoleName

                    140 ->
                        Decode.succeed ExceededQuota

                    141 ->
                        Decode.succeed ScriptFailed

                    142 ->
                        Decode.succeed ValidationError

                    143 ->
                        Decode.succeed InvalidImageData

                    151 ->
                        Decode.succeed UnsavedFileError

                    152 ->
                        Decode.succeed InvalidPushTimeError

                    153 ->
                        Decode.succeed FileDeleteError

                    155 ->
                        Decode.succeed RequestLimitExceeded

                    160 ->
                        Decode.succeed InvalidEventName

                    200 ->
                        Decode.succeed UsernameMissing

                    201 ->
                        Decode.succeed Passwordmissing

                    202 ->
                        Decode.succeed UsernameTaken

                    203 ->
                        Decode.succeed EmailTaken

                    204 ->
                        Decode.succeed EmailMissing

                    205 ->
                        Decode.succeed EmailNotFound

                    206 ->
                        Decode.succeed SessionMissing

                    207 ->
                        Decode.succeed MustCreateUserThroughSignup

                    208 ->
                        Decode.succeed AccountAlreadyLinked

                    209 ->
                        Decode.succeed InvalidSessionToken

                    250 ->
                        Decode.succeed LinkedIdMissing

                    251 ->
                        Decode.succeed InvalidLinkedSession

                    252 ->
                        Decode.succeed UnsupportedService

                    255 ->
                        Decode.succeed InvalidSchemaOperation

                    600 ->
                        Decode.succeed AggregateError

                    601 ->
                        Decode.succeed FileReadError

                    602 ->
                        Decode.succeed XDomainRequest

                    _ ->
                        [ "'"
                        , toString code
                        , "' is not a valid Parse error code"
                        ]
                            |> String.concat
                            |> Decode.fail
            )
