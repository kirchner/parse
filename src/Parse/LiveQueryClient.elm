module Parse.LiveQueryClient
    exposing
        ( defaultModel
        , init
        , Model
        , subscribe
        , subscription
        , unsubscribe
        , update
        )

{-|

@docs Model, defaultModel, init

@docs subscribe, subscription, unsubscribe

@docs update

-}

import Json.Decode as Json exposing (Decoder, Value)
import Json.Encode as Encode
import Parse exposing (Config, Query)
import Parse.LiveQuery as LiveQuery
import Parse.LiveQueryClient.Internal as Internal exposing (Msg(..), Lift, connect, decodeMsg)
import Task exposing (Task)
import WebSocket


{-| -}
type alias Model m =
    { subscriptions : List (Subscription m)
    , clientId : Maybe String
    , nextRequestId : Int
    , queue : List (Msg m)
    }


type alias Subscription m =
    { requestId : Int
    , open : Bool
    , query : Query
    , lift : Lift m
    }


{-| -}
defaultModel : Model m
defaultModel =
    { subscriptions = []
    , nextRequestId = 1
    , clientId = Nothing
    , queue = []
    }


{-| -}
init :
    Config
    -> List (Msg m)
    -> ( Model m, Cmd m )
init config queue =
    ( { defaultModel | queue = queue }
    , WebSocket.send config.serverUrl (Encode.encode 0 (connect config))
    )


{-| -}
update : Config -> Msg m -> Model m -> ( Model m, Cmd m )
update config msg model =
    let
        ( updatedModel, internalCmds ) =
            updateInternal config msg model

        userMsgs =
            List.concat <|
                List.map (updateSubscription config msg) model.subscriptions

        userCmds =
            List.map (Task.perform identity << Task.succeed) userMsgs
    in
        ( updatedModel, Cmd.batch (internalCmds :: userCmds) )


{-| -}
updateSubscription : Config -> Msg m -> Subscription m -> List m
updateSubscription config msg subscription =
    let
        when requestId x =
            if requestId == subscription.requestId then
                x
            else
                []

        objectWhen requestId x =
            when requestId <|
                case x of
                    Ok m ->
                        [ m ]

                    Err err ->
                        [ subscription.lift.liftErr err ]
    in
        case msg of
            DecodeError err ->
                []

            Subscribe _ _ ->
                []

            Unsubscribe _ ->
                []

            Connected { clientId } ->
                []

            Subscribed { clientId, requestId } ->
                when requestId [ subscription.lift.onOpen ]

            Unsubscribed { requestId } ->
                when requestId [ subscription.lift.onClose ]

            Error { code, error, reconnect } ->
                []

            Create { requestId, object } ->
                objectWhen requestId (Json.decodeValue subscription.lift.onCreate object)

            Update { requestId, object } ->
                objectWhen requestId (Json.decodeValue subscription.lift.onUpdate object)

            Enter { requestId, object } ->
                objectWhen requestId (Json.decodeValue subscription.lift.onEnter object)

            Leave { requestId, object } ->
                objectWhen requestId (Json.decodeValue subscription.lift.onLeave object)

            Delete { requestId, object } ->
                objectWhen requestId (Json.decodeValue subscription.lift.onDelete object)


updateInternal : Config -> Msg m -> Model m -> ( Model m, Cmd m )
updateInternal config msg model =
    case msg of
        DecodeError err ->
            let
                _ =
                    Debug.log "ProtocolError" err
            in
                ( model, Cmd.none )

        Connected { clientId } ->
            let
                ( updatedModel, cmds ) =
                    List.foldl
                        (\msg ( model, cmds ) ->
                            let
                                ( updatedModel, newCmds ) =
                                    update config msg model
                            in
                                ( updatedModel, Cmd.batch [ newCmds, cmds ] )
                        )
                        ( { model | clientId = Just clientId, queue = [] }, Cmd.none )
                        model.queue
            in
                ( updatedModel, cmds )

        Error { code, error, reconnect } ->
            ( { model | clientId = Nothing }
            , if reconnect then
                WebSocket.send config.serverUrl <|
                    Encode.encode 0 (connect config)
              else
                Cmd.none
            )

        Subscribed { clientId, requestId } ->
            let
                subscriptions =
                    List.map
                        (\subscription ->
                            if subscription.requestId == requestId then
                                { subscription | open = True }
                            else
                                subscription
                        )
                        model.subscriptions
            in
                ( { model | subscriptions = subscriptions }, Cmd.none )

        Unsubscribed { requestId } ->
            let
                subscriptions =
                    model.subscriptions
                        |> List.filter ((/=) requestId << .requestId)
            in
                ( { model | subscriptions = subscriptions }, Cmd.none )

        Subscribe query lift ->
            let
                requestId =
                    model.nextRequestId

                subscription =
                    { requestId = requestId
                    , open = False
                    , lift = lift
                    , query = query
                    }
            in
                ( { model
                    | nextRequestId = requestId + 1
                    , subscriptions = subscription :: model.subscriptions
                  }
                , WebSocket.send config.serverUrl <|
                    Encode.encode 0 (Internal.subscribe config query requestId)
                )

        Unsubscribe query ->
            let
                requestId =
                    model.subscriptions
                        |> List.filter
                            (\subscription ->
                                subscription.query == query
                            )
                        |> List.head
                        |> Maybe.map .requestId
            in
                ( model
                , case requestId of
                    Just requestId ->
                        WebSocket.send config.serverUrl <|
                            Encode.encode 0 (Internal.unsubscribe config requestId)

                    Nothing ->
                        Cmd.none
                )

        _ ->
            ( model, Cmd.none )


{-| -}
subscribe : Query -> Decoder a -> (Result String (LiveQuery.Msg a) -> m) -> Msg m
subscribe query decodeObject lift =
    let
        liftOk =
            lift << Ok

        liftErr =
            lift << Err
    in
        Subscribe query
            { onOpen = liftOk LiveQuery.Open
            , onClose = liftOk LiveQuery.Close
            , onCreate = Json.map (liftOk << LiveQuery.Create) decodeObject
            , onUpdate = Json.map (liftOk << LiveQuery.Update) decodeObject
            , onEnter = Json.map (liftOk << LiveQuery.Enter) decodeObject
            , onLeave = Json.map (liftOk << LiveQuery.Leave) decodeObject
            , onDelete = Json.map (liftOk << LiveQuery.Delete) decodeObject
            , liftErr = liftErr
            }


{-| -}
unsubscribe : Query -> Msg m
unsubscribe query =
    Unsubscribe query


{-| -}
subscription : (Msg m -> m) -> Config -> Model m -> Sub m
subscription lift config model =
    WebSocket.listen config.serverUrl <|
        \string ->
            case Json.decodeString decodeMsg string of
                Ok internalMsg ->
                    lift internalMsg

                Err err ->
                    lift (DecodeError err)
