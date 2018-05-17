module Internal.Query exposing (..)


import Dict exposing (Dict)
import Http
import Internal.Config exposing (Config)
import Internal.Error exposing (Error)
import Internal.Object exposing (Object)
import Internal.Request as Requet exposing (Request, request)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Task exposing (Task)


query :
    Decoder (Object a)
    -> Query
    -> Request (List (Object a))
query objectDecoder query =
    request
        { method = "GET"
        , endpoint = "/classes/" ++ query.className ++ "?" ++ serializeQuery query
        , body = Nothing
        , decoder = Decode.field "results" (Decode.list objectDecoder)
        }


type alias Query =
    { className : String
    , whereClause :
        Constraint
        -- RESPONSE
    , order : List String
    , keys : List String
    , include : List String
    , count :
        Bool
        -- PAGINATION
    , limit : Maybe Int
    , skip : Maybe Int
    }


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


or : List Constraint -> Constraint
or =
    Or


exists : String -> Constraint
exists fieldName =
    Field fieldName [ Exists ]


equalTo : String -> String -> Constraint
equalTo fieldName =
    Field fieldName << List.singleton << EqualTo


notEqualTo : String -> String -> Constraint
notEqualTo fieldName =
    Field fieldName << List.singleton << NotEqualTo


lessThan : String -> Float -> Constraint
lessThan fieldName =
    Field fieldName << List.singleton << LessThan


lessThanOrEqualTo : String -> Float -> Constraint
lessThanOrEqualTo fieldName =
    Field fieldName << List.singleton << LessThanOrEqualTo


greaterThan : String -> Float -> Constraint
greaterThan fieldName =
    Field fieldName << List.singleton << GreaterThan


greaterThanOrEqualTo : String -> Float -> Constraint
greaterThanOrEqualTo fieldName =
    Field fieldName << List.singleton << GreaterThanOrEqualTo


regex : String -> String -> Constraint
regex fieldName =
    Field fieldName << List.singleton << Regex



-- INTERNAL HELPER


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
