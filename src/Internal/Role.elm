module Internal.Role
    exposing
        ( Role
        , role
        , createRole
        )

import Date exposing (Date)
import Internal.ACL as ACL exposing (ACL)
import Internal.ACL.Types exposing (Role)
import Internal.Object exposing (Object)
import Internal.ObjectId exposing (ObjectId)
import Internal.Pointer as Pointer exposing (Pointer)
import Internal.Request as Request exposing (request)
import Internal.Request exposing (Request)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse.Decode as Decode
import Parse.Encode as Encode


type alias Role user =
    Internal.ACL.Types.Role user


role :
    { name : String
    , acl : ACL user
    }
    -> Role user
role =
    identity


encode : Role user -> Value
encode role =
    Encode.object
        [ ( "name", Encode.string role.name )
        , ( "acl", ACL.encode role.acl )
        ]



--decode : Decoder (Object (Role user))
decode =
    Decode.decode
        (\objectId createdAt updatedAt name acl ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , name = name
            , acl = acl
            }
        )


createRole :
    Role user
    -> List (Pointer user)
    -> List (Pointer (Role user))
    -> Request { objectId : ObjectId (Role a), createdAt : Date }
createRole ({ name, acl } as role) users roles =
    let
        body =
            Encode.object
                [ ( "name", Encode.string name )
                , ( "acl", ACL.encode acl )
                  --                        , users
                  --                            |> List.map Encode.pointer
                  --                            |> \objects ->
                  --                                Encode.object
                  --                                    [ ( "__op", "AddRelation" )
                  --                                    , ( "objects", Encode.list objects )
                  --                                    ]
                  --                        , roles
                  --                            |> List.map Encode.pointer
                  --                            |> \objects ->
                  --                                Encode.object
                  --                                    [ ( "__op", "AddRelation" )
                  --                                    , ( "objects", Encode.list objects )
                  --                                    ]
                ]
    in
        request
            { method = "POST"
            , endpoint = "/roles"
            , body = Just body
            , decoder = Request.postDecoder
            }
