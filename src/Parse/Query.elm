module Parse.Query
    exposing
        ( Query
        , defaultQuery
        , encodeQuery
        )

{-|

@docs Query, defaultQuery, encodeQuery

-}

import Json.Decode as Json exposing (Decoder, Value)
import Json.Encode as Encode


{-| -}
type alias Query =
    { whereClause : List String
    , include : List String
    , keys : List String
    , limit : Maybe Int
    , skip : Int
    , order : List String
    , className : Maybe String
    , count : Maybe Int
    }


{-| -}
defaultQuery : Query
defaultQuery =
    { whereClause = []
    , include = []
    , keys = []
    , limit = Nothing
    , skip = 0
    , order = []
    , className = Nothing
    , count = Nothing
    }


{-| -}
encodeQuery : Query -> Value
encodeQuery query =
    Encode.object <|
        List.filterMap identity
            [ required "where" (Encode.object [])
            , if query.include == [] then
                Nothing
              else
                required "include" (Encode.string (String.join "," query.include))
            , if query.keys == [] then
                Nothing
              else
                required "keys" (Encode.string (String.join "," query.keys))
            , optional "limit" Encode.int query.limit
            , if query.skip <= 0 then
                Nothing
              else
                required "skip" (Encode.int query.skip)
            , if query.order == [] then
                Nothing
              else
                required "order" (Encode.string (String.join "," query.order))
            , optional "className" Encode.string query.className
            , optional "count" Encode.int query.count
            ]


required key value =
    Just ( key, value )


optional key encode value =
    Maybe.map (\value -> ( key, encode value )) value
