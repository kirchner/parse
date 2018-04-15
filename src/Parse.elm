module Parse
    exposing
        ( Code
            ( InvalidQuery
            , UserInvalidLoginParams
            )
        , Config
        , Constraint
        , Error
            ( BadError
            , HttpError
            , ParseError
            )
        , Object
        , ObjectId
        , Param
        , and
        , constraint
        , count
        , create
        , delete
        , distinct
        , encodeObjectId
        , equalTo
        , exists
        , get
        , greaterThan
        , greaterThanOrEqualTo
        , lessThan
        , lessThanOrEqualTo
        , limit
        , notEqualTo
        , objectIdDecoder
        , or
        , query
        , regex
        , update
        )

{-|


# Configuration

@docs Config


# Objects

@docs Object, ObjectId, objectIdDecoder, encodeObjectId


# REST Actions

@docs create, get, update, delete


# Queries

@docs query

@docs Param, count, limit, distinct


## Constraints

@docs Constraint, constraint

@docs and, or, exists

@docs equalTo, notEqualTo, regex

@docs lessThan, lessThanOrEqualTo, greaterThan, greaterThanOrEqualTo


# Errors

@docs Error, Code

-}

import Date exposing (Date)
import Dict
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
        , urlSuffix = className ++ "/" ++ id
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



---- QUERIES


{-| -}
query :
    String
    -> Decoder fields
    -> Config
    -> List Param
    -> Task Error (List fields)
query className fieldsDecoder config params =
    request config
        { method = "GET"
        , urlSuffix = className ++ "?" ++ serializeParams params
        , body = Http.emptyBody
        , responseDecoder = Decode.field "results" (Decode.list fieldsDecoder)
        }


{-| -}
type Param
    = Where Constraint
    | Count
    | Limit Int
    | Distinct String


{-| -}
count : Param
count =
    Count


{-| -}
limit : Int -> Param
limit =
    Limit


{-| -}
distinct : String -> Param
distinct =
    Distinct


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
constraint : Constraint -> Param
constraint =
    Where


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
                                BadError decodeError

                    _ ->
                        HttpError httpError
            )


defaultHeaders : Config -> List Http.Header
defaultHeaders config =
    [ Http.header "X-Parse-Application-Id" config.applicationId
    , Http.header "X-Parse-REST-API-Key" config.restApiKey
    ]


serializeParams : List Param -> String
serializeParams params =
    params
        |> List.map serializeParam
        |> String.join "&"


serializeParam : Param -> String
serializeParam param =
    case param of
        Count ->
            "count=1"

        Limit limit ->
            "limit=" ++ toString limit

        Distinct fieldName ->
            "distinct=" ++ fieldName

        Where constraint ->
            [ "where="
            , encodeConstraint constraint
                |> Encode.object
                |> Encode.encode 0
                |> Http.encodeUri
            ]
                |> String.concat


encodeConstraint : Constraint -> List ( String, Value )
encodeConstraint constraint =
    case constraint of
        And constraints ->
            constraints
                |> List.map encodeConstraint
                |> List.concat

        Or constraints ->
            [ ( "$or"
              , constraints
                    |> List.map (encodeConstraint >> Encode.object)
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
