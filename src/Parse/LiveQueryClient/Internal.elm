module Parse.LiveQueryClient.Internal
    exposing
        ( Msg(..)
        , decodeMsg
        , subscribe
        , unsubscribe
        , connect
        )

import Json.Decode as Json exposing (Decoder, Value)
import Json.Encode as Encode
import Parse exposing (Config, Query)


type Msg
    = DecodeError String
    | Connected { clientId : String }
    | Subscribed { clientId : String, requestId : Int }
    | Unsubscribed { requestId : Int }
    | Error { code : Int, error : String, reconnect : Bool }
    | Open { requestId : Int }
    | Close { requestId : Int }
    | Create { requestId : Int, object : Value }
    | Update { requestId : Int, object : Value }
    | Enter { requestId : Int, object : Value }
    | Leave { requestId : Int, object : Value }
    | Delete { requestId : Int, object : Value }


decodeMsg : Decoder Msg
decodeMsg =
    Json.at [ "op" ] Json.string
        |> Json.andThen
            (\op ->
                case op of
                    "connected" ->
                        Json.map
                            (\clientId ->
                                Connected { clientId = clientId }
                            )
                            (Json.at [ "clientId" ] Json.string)

                    "subscribed" ->
                        Json.map2
                            (\clientId requestId ->
                                Subscribed { clientId = clientId, requestId = requestId }
                            )
                            (Json.at [ "clientId" ] Json.string)
                            (Json.at [ "requestId" ] Json.int)

                    "unsubscribed" ->
                        Json.map
                            (\requestId ->
                                Unsubscribed { requestId = requestId }
                            )
                            (Json.at [ "requestId" ] Json.int)

                    "error" ->
                        Json.map3
                            (\code error reconnect ->
                                Error { code = code, error = error, reconnect = reconnect }
                            )
                            (Json.at [ "code" ] Json.int)
                            (Json.at [ "error" ] Json.string)
                            (Json.at [ "reconnect" ] Json.bool)

                    "open" ->
                        Json.map
                            (\requestId ->
                                Open { requestId = requestId }
                            )
                            (Json.at [ "requestId" ] Json.int)

                    "close" ->
                        Json.map
                            (\requestId ->
                                Close { requestId = requestId }
                            )
                            (Json.at [ "requestId" ] Json.int)

                    "create" ->
                        Json.map2
                            (\requestId object ->
                                Create { requestId = requestId, object = object }
                            )
                            (Json.at [ "requestId" ] Json.int)
                            (Json.at [ "object" ] Json.value)

                    "update" ->
                        Json.map2
                            (\requestId object ->
                                Update { requestId = requestId, object = object }
                            )
                            (Json.at [ "requestId" ] Json.int)
                            (Json.at [ "object" ] Json.value)

                    "enter" ->
                        Json.map2
                            (\requestId object ->
                                Enter { requestId = requestId, object = object }
                            )
                            (Json.at [ "requestId" ] Json.int)
                            (Json.at [ "object" ] Json.value)

                    "leave" ->
                        Json.map2
                            (\requestId object ->
                                Leave { requestId = requestId, object = object }
                            )
                            (Json.at [ "requestId" ] Json.int)
                            (Json.at [ "object" ] Json.value)

                    "delete" ->
                        Json.map2
                            (\requestId object ->
                                Delete { requestId = requestId, object = object }
                            )
                            (Json.at [ "requestId" ] Json.int)
                            (Json.at [ "object" ] Json.value)

                    _ ->
                        Json.fail ("unkown op `" ++ op ++ "'")
            )


connect : Config -> Value
connect config =
    Encode.object <|
        List.filterMap identity <|
            [ Just ( "op", Encode.string "connect" )
            , Just ( "applicationId", Encode.string config.applicationId )
            , config.restAPIKey
                |> Maybe.map ((,) "restAPIKey" << Encode.string)
            , config.javascriptKey
                |> Maybe.map ((,) "javascriptKey" << Encode.string)
            , config.clientKey
                |> Maybe.map ((,) "clientKey" << Encode.string)
            , config.windowsKey
                |> Maybe.map ((,) "windowsKey" << Encode.string)
            , config.masterKey
                |> Maybe.map ((,) "masterKey" << Encode.string)
            , config.sessionToken
                |> Maybe.map ((,) "sessionToken" << Encode.string)
            ]


subscribe : Config -> Query -> Int -> Value
subscribe config query requestId =
    Encode.object <|
        List.filterMap identity
            [ Just ( "op", Encode.string "subscribe" )
            , Just ( "requestId", Encode.int requestId )
            , Just ( "query", Parse.encodeQuery query )
            , config.sessionToken
                |> Maybe.map ((,) "sessionToken" << Encode.string)
            ]


unsubscribe : Config -> Int -> Value
unsubscribe config requestId =
    Encode.object <|
        [ ( "op", Encode.string "unsubscribe" )
        , ( "requestId", Encode.int requestId )
        ]
