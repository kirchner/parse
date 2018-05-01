module Parse.Decode
    exposing
        ( date
        , objectId
        , pointer
        , sessionToken
        )

{-|

@docs sessionToken

@docs objectId

@docs date

@docs pointer

-}

import Date exposing (Date)
import Internal.ObjectId exposing (..)
import Internal.SessionToken exposing (..)
import Json.Decode as Decode exposing (Decoder)


{-| -}
sessionToken : Decoder SessionToken
sessionToken =
    Decode.map SessionToken Decode.string


{-| -}
objectId : Decoder ObjectId
objectId =
    Decode.map ObjectId Decode.string


{-| -}
date : Decoder Date
date =
    Decode.oneOf
        [ Decode.string
        , parseTypeDecoder "Date" <|
            Decode.field "iso" Decode.string
        ]
        |> Decode.andThen
            (\rawDate ->
                case Date.fromString rawDate of
                    Ok date ->
                        Decode.succeed date

                    Err err ->
                        [ "could not parse date: "
                        , err
                        ]
                            |> String.concat
                            |> Decode.fail
            )


{-| -}
pointer : String -> Decoder ObjectId
pointer className =
    parseTypeDecoder "Pointer"
        (Decode.field "className" Decode.string
            |> Decode.andThen
                (\actualClassName ->
                    if actualClassName /= className then
                        [ "we expected a pointer for the class '"
                        , className
                        , "' but got a pointer for the class '"
                        , actualClassName
                        , "'"
                        ]
                            |> String.concat
                            |> Decode.fail
                    else
                        Decode.field "objectId" objectId
                )
        )



---- HELPER


parseTypeDecoder : String -> Decoder a -> Decoder a
parseTypeDecoder expectedType decoder =
    Decode.field "__type" Decode.string
        |> Decode.andThen
            (\actualType ->
                if actualType /= expectedType then
                    [ "we expected a field of the Parse type '"
                    , expectedType
                    , "' but the Parse type is '"
                    , actualType
                    , "'"
                    ]
                        |> String.concat
                        |> Decode.fail
                else
                    decoder
            )
