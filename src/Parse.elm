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
        , ObjectId
        , Query
        , and
        , create
        , delete
        , emptyQuery
        , encodeObjectId
        , encodeQuery
        , equalTo
        , exists
        , get
        , greaterThan
        , greaterThanOrEqualTo
        , lessThan
        , lessThanOrEqualTo
        , notEqualTo
        , objectIdDecoder
        , or
        , query
        , regex
        , simpleConfig
        , update
        )

{-|


# Configuration

@docs Config, simpleConfig


# REST Actions

@docs create, get, update, delete

@docs ObjectId, objectIdDecoder, encodeObjectId


# Queries

@docs query, Query, emptyQuery, encodeQuery


## Constraints

@docs Constraint

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
    { serverUrl : String
    , applicationId : String
    , restAPIKey : Maybe String
    , javascriptKey : Maybe String
    , clientKey : Maybe String
    , windowsKey : Maybe String
    , masterKey : Maybe String
    , sessionToken : Maybe String
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



---- OBJECTS


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
    Decoder fields
    -> Config
    -> Query
    -> Task Error (List fields)
query fieldsDecoder config query =
    request config
        { method = "GET"
        , urlSuffix = query.className ++ "?" ++ serializeQuery query
        , body = Http.emptyBody
        , responseDecoder = Decode.field "results" (Decode.list fieldsDecoder)
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
                                BadError decodeError

                    _ ->
                        HttpError httpError
            )


defaultHeaders : Config -> List Http.Header
defaultHeaders config =
    List.filterMap identity
        [ Just (Http.header "X-Parse-Application-Id" config.applicationId)
        , config.restAPIKey
            |> Maybe.map (Http.header "X-Parse-REST-API-Key")
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
        Just (String.join "," query.include)
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
