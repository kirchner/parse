module Internal.GeoPoint
    exposing
        ( GeoPoint
        , geoPoint
        , latitude
        , longitude
        , decode
        , encode
        )

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse.Decode as Decode


type GeoPoint
    = GeoPoint
        { latitude : Float
        , longitude : Float
        }


geoPoint : { latitude : Float, longitude : Float } -> GeoPoint
geoPoint =
    GeoPoint


latitude : GeoPoint -> Float
latitude geoPoint =
    case geoPoint of
        GeoPoint { latitude } ->
            latitude


longitude : GeoPoint -> Float
longitude geoPoint =
    case geoPoint of
        GeoPoint { longitude } ->
            longitude


decode : Decoder GeoPoint
decode =
    Decode.parseTypeDecoder "GeoPoint"
        (Decode.decode
            (\latitude longitude ->
                geoPoint { latitude = latitude, longitude = longitude }
            )
            |> Decode.required "latitude" Decode.float
            |> Decode.required "longitude" Decode.float
        )


encode : GeoPoint -> Value
encode geoPoint =
    case geoPoint of
        GeoPoint { latitude, longitude } ->
            Encode.object
                [ ( "__type", Encode.string "GeoPoint" )
                , ( "latitude", Encode.float latitude )
                , ( "longitude", Encode.float longitude )
                ]
