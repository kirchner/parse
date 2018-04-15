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

import Dict exposing (Dict)
import Json.Decode as Json exposing (Decoder, Value)
import Json.Encode as Encode
import Parse exposing (Config)
import Parse.LiveQuery as LiveQuery
import Parse.LiveQueryClient.Internal as Internal
import Parse.Query as Query exposing (Query)
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


type alias Lift m =
    { onOpen : m
    , onClose : m
    , onCreate : Decoder m
    , onUpdate : Decoder m
    , onEnter : Decoder m
    , onLeave : Decoder m
    , onDelete : Decoder m
    , liftErr : String -> m
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
    , WebSocket.send config.serverUrl (Encode.encode 0 (Internal.connect config))
    )


{-| -}
update : Config -> Internal.Msg -> Model m -> ( Model m, Cmd m )
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
updateSubscription : Config -> Internal.Msg -> Subscription m -> List m
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
            Internal.DecodeError err ->
                []

            Internal.Connected { clientId } ->
                []

            Internal.Subscribed { clientId, requestId } ->
                []

            Internal.Unsubscribed { requestId } ->
                []

            Internal.Error { code, error, reconnect } ->
                []

            Internal.Open { requestId } ->
                when requestId [ subscription.lift.onOpen ]

            Internal.Close { requestId } ->
                when requestId [ subscription.lift.onClose ]

            Internal.Create { requestId, object } ->
                objectWhen requestId (Json.decodeValue subscription.lift.onCreate object)

            Internal.Update { requestId, object } ->
                objectWhen requestId (Json.decodeValue subscription.lift.onUpdate object)

            Internal.Enter { requestId, object } ->
                objectWhen requestId (Json.decodeValue subscription.lift.onEnter object)

            Internal.Leave { requestId, object } ->
                objectWhen requestId (Json.decodeValue subscription.lift.onLeave object)

            Internal.Delete { requestId, object } ->
                objectWhen requestId (Json.decodeValue subscription.lift.onDelete object)


updateInternal : Config -> Internal.Msg -> Model m -> ( Model m, Cmd m )
updateInternal config msg model =
    case msg of
        Internal.DecodeError err ->
            let
                _ =
                    Debug.log "ProtocolError" err
            in
                ( model, Cmd.none )

        Internal.Connected { clientId } ->
            let
                ( updatedModel, cmds ) =
                    List.foldl
                        (\operation ( model, cmds ) ->
                            case operation of
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
                                        ( { model | nextRequestId = requestId + 1, subscriptions = subscription :: model.subscriptions }
                                        , runMsg (Subscribe query lift) config requestId :: cmds
                                        )

                                _ ->
                                    ( model, cmds )
                         -- TODO:
                         --                        Unsubscribe query ->
                         --                          let
                         --                              requestId =
                         --                                model.subscriptions
                         --                                |> List.filter (\ subscription ->
                         --                                  subscription.query == query
                         --                                  )
                         --                                |> List.head
                         --                                |> Maybe.map .requestId
                         --                          in
                         --                          ( model
                         --                          , (::)
                         --                          (case requestId of
                         --                                Just requestId ->
                         --                                    runMsg (Subscribe query) config requestId
                         --                                Nothing ->
                         --                                  Cmd.none
                         --                                  ) cmds
                         --                          )
                        )
                        ( { model | clientId = Just clientId, queue = [] }, [] )
                        model.queue
            in
                ( updatedModel, Cmd.batch cmds )

        Internal.Error { code, error, reconnect } ->
            ( { model | clientId = Nothing }
            , if reconnect then
                WebSocket.send config.serverUrl <|
                    Encode.encode 0 (Internal.connect config)
              else
                Cmd.none
            )

        Internal.Subscribed { clientId, requestId } ->
            ( model, Cmd.none )

        Internal.Unsubscribed { requestId } ->
            let
                subscriptions =
                    model.subscriptions
                        |> List.filter ((/=) requestId << .requestId)
            in
                ( { model | subscriptions = subscriptions }, Cmd.none )

        Internal.Open { requestId } ->
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

        Internal.Close { requestId } ->
            let
                subscriptions =
                    List.map
                        (\subscription ->
                            if subscription.requestId == requestId then
                                { subscription | open = False }
                            else
                                subscription
                        )
                        model.subscriptions
            in
                ( { model | subscriptions = subscriptions }, Cmd.none )

        _ ->
            ( model, Cmd.none )


type Msg m
    = Subscribe Query (Lift m)
    | Unsubscribe Query


runMsg : Msg m -> (Config -> Int -> Cmd m)
runMsg operation =
    case operation of
        Subscribe query lift ->
            \config requestId ->
                WebSocket.send config.serverUrl <|
                    Encode.encode 0 (Internal.subscribe config query requestId)

        Unsubscribe query ->
            \config requestId ->
                WebSocket.send config.serverUrl <|
                    Encode.encode 0 (Internal.unsubscribe config requestId)


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
subscription : (Internal.Msg -> m) -> Config -> Model m -> Sub m
subscription lift config model =
    WebSocket.listen config.serverUrl <|
        \string ->
            case Json.decodeString Internal.decodeMsg string of
                Ok internalMsg ->
                    lift internalMsg

                Err err ->
                    lift (Internal.DecodeError err)
