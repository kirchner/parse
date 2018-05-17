module Internal.ACL
    exposing
        ( ACL
        , RoleName
        , acl
        , anybody
        , roles
        , users
        , Permissions
        , simple
        , extended
        , encode
        , decode
        )

import Internal.ObjectId as ObjectId exposing (ObjectId)
import Internal.ObjectId exposing (ObjectId)
import Internal.Pointer as Pointer exposing (Pointer, pointer)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type ACL user
    = ACL
        { anybody : Permissions
        , roles : List ( RoleName, Permissions )
        , users : List ( Pointer user, Permissions )
        }


type alias RoleName =
    String


acl :
    { anybody : Permissions
    , roles : List ( RoleName, Permissions )
    , users : List ( Pointer user, Permissions )
    }
    -> ACL user
acl { anybody, roles, users } =
    ACL
        { anybody = anybody
        , roles = roles
        , users = users
        }


anybody : ACL user -> Permissions
anybody acl =
    case acl of
        ACL { anybody } ->
            anybody


roles : ACL user -> List ( RoleName, Permissions )
roles acl =
    case acl of
        ACL { roles } ->
            roles


users : ACL user -> List ( Pointer user, Permissions )
users acl =
    case acl of
        ACL { users } ->
            users


encode : ACL user -> Value
encode acl =
    Encode.object
        (List.concat
            [ [ ( "*", encodePermissions (anybody acl) )
              ]
            , roles acl
                |> List.map
                    (\( roleName, permissions ) ->
                        ( "role:" ++ roleName
                        , encodePermissions permissions
                        )
                    )
            , users acl
                |> List.map
                    (\( pointer, permissions ) ->
                        ( ObjectId.toString (Pointer.objectId pointer)
                        , encodePermissions permissions
                        )
                    )
            ]
        )


decode : Decoder (ACL user)
decode =
    Decode.keyValuePairs permissionsDecoder
        |> Decode.map
            (List.foldl
                (\( key, permissions ) ({ anybody, roles, users } as result) ->
                    if key == "*" then
                        ({ result
                            | anybody = ( "*", permissions ) :: roles
                         }
                        )
                    else if String.startsWith "role:" key then
                        let
                            roleName =
                                String.dropLeft 5 key
                        in
                            ({ result
                                | roles = ( roleName, permissions ) :: roles
                             }
                            )
                    else
                        let
                            user =
                                pointer "_User" (ObjectId.fromString key)
                        in
                            ({ result
                                | users = ( user, permissions ) :: users
                             }
                            )
                )
                { anybody = []
                , roles = []
                , users = []
                }
            )
        |> Decode.map
            (\{ anybody, roles, users } ->
                ACL
                    { anybody =
                        anybody
                            |> List.head
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault (simple { read = False, write = False })
                    , roles = roles
                    , users = users
                    }
            )


type alias Permissions =
    { get : Bool
    , find : Bool
    , write : Bool
    , update : Bool
    , delete : Bool
    , addFields : Bool
    }


simple : { read : Bool, write : Bool } -> Permissions
simple { read, write } =
    { get = read
    , find = read
    , write = write
    , update = write
    , delete = write
    , addFields = False
    }


extended :
    { get : Bool
    , find : Bool
    , write : Bool
    , update : Bool
    , delete : Bool
    , addFields : Bool
    }
    -> Permissions
extended { get, find, write, update, delete, addFields } =
    { get = get
    , find = find
    , write = write
    , update = update
    , delete = delete
    , addFields = addFields
    }


encodePermissions : Permissions -> Value
encodePermissions perms =
    Encode.object
        [ ( "get", Encode.bool perms.get )
        , ( "find", Encode.bool perms.find )
        , ( "write", Encode.bool perms.write )
        , ( "update", Encode.bool perms.update )
        , ( "delete", Encode.bool perms.delete )
        , ( "addFields", Encode.bool perms.addFields )
        ]


permissionsDecoder : Decoder Permissions
permissionsDecoder =
    Decode.map6
        (\get find write update delete addFields ->
            { get = get
            , find = find
            , write = write
            , update = update
            , delete = delete
            , addFields = addFields
            }
        )
        (Decode.oneOf
            [ Decode.at [ "get" ] Decode.bool
            , Decode.at [ "read" ] Decode.bool
            , Decode.succeed False
            ]
        )
        (Decode.oneOf
            [ Decode.at [ "find" ] Decode.bool
            , Decode.at [ "read" ] Decode.bool
            , Decode.succeed False
            ]
        )
        (Decode.oneOf
            [ Decode.at [ "write" ] Decode.bool
              -- TODO: not right?
            , Decode.succeed False
            ]
        )
        (Decode.oneOf
            [ Decode.at [ "update" ] Decode.bool
            , Decode.at [ "write" ] Decode.bool
            , Decode.succeed False
            ]
        )
        (Decode.oneOf
            [ Decode.at [ "delete" ] Decode.bool
            , Decode.at [ "write" ] Decode.bool
            , Decode.succeed False
            ]
        )
        (Decode.oneOf
            [ Decode.at [ "addFields" ] Decode.bool
            , Decode.succeed False
            ]
        )
