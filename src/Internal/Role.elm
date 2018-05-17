module Internal.Role
    exposing
        ( addRoles
        , addUsers
        , createRole
        , deleteRole
        , deleteRoles
        , deleteUsers
        , getRole
        , role
        , Role
        )

import Date exposing (Date)
import Internal.ACL as ACL exposing (ACL)
import Internal.Object exposing (Object)
import Internal.ObjectId as ObjectId exposing (ObjectId)
import Internal.Pointer as Pointer exposing (Pointer)
import Internal.Request as Request exposing (request)
import Internal.Request exposing (Request)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse.Decode as Decode
import Parse.Encode as Encode


type alias Role user =
    { name : String
    , acl : ACL user
    }


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


decode : Decoder (Object (Role user))
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
        |> Decode.required "objectId" Decode.objectId
        |> Decode.required "createdAt" Decode.date
        |> Decode.required "updatedAt" Decode.date
        |> Decode.required "name" Decode.string
        |> Decode.required "ACL" ACL.decode


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
                , ( "users"
                  , users
                        |> List.map (Encode.pointer "_User")
                        |> \objects ->
                            Encode.object
                                [ ( "__op", Encode.string "AddRelation" )
                                , ( "objects", Encode.list objects )
                                ]
                  )
                , ( "roles"
                  , roles
                        |> List.map (Encode.pointer "_Role")
                        |> \objects ->
                            Encode.object
                                [ ( "__op", Encode.string "AddRelation" )
                                , ( "objects", Encode.list objects )
                                ]
                  )
                ]
    in
        request
            { method = "POST"
            , endpoint = "/roles"
            , body = Just body
            , decoder = Request.postDecoder
            }


getRole : ObjectId (Role user) -> Request (Object (Role user))
getRole objectId =
    request
        { method = "GET"
        , endpoint = "/roles/" ++ ObjectId.toString objectId
        , body = Nothing
        , decoder = decode
        }


deleteRole : ObjectId (Role user) -> Request {}
deleteRole objectId =
    request
        { method = "DELETE"
        , endpoint = "/roles/" ++ ObjectId.toString objectId
        , body = Nothing
        , decoder = Decode.succeed {}
        }


addUsers : ObjectId (Role user) -> List (Pointer user) -> Request { updatedAt : Date }
addUsers objectId users =
    request
        { method = "PUT"
        , endpoint = "/roles/" ++ ObjectId.toString objectId
        , body = Just (Encode.object [ ( "users", addRelation "_User" users ) ])
        , decoder = Request.putDecoder
        }


deleteUsers : ObjectId (Role user) -> List (Pointer user) -> Request { updatedAt : Date }
deleteUsers objectId users =
    request
        { method = "PUT"
        , endpoint = "/roles/" ++ ObjectId.toString objectId
        , body = Just (Encode.object [ ( "users", removeRelation "_User" users ) ])
        , decoder = Request.putDecoder
        }


addRoles :
    ObjectId (Role user)
    -> List (Pointer (Role user))
    -> Request { updatedAt : Date }
addRoles objectId roles =
    request
        { method = "PUT"
        , endpoint = "/roles/" ++ ObjectId.toString objectId
        , body = Just (Encode.object [ ( "roles", addRelation "_Role" roles ) ])
        , decoder = Request.putDecoder
        }


deleteRoles :
    ObjectId (Role user)
    -> List (Pointer (Role user))
    -> Request { updatedAt : Date }
deleteRoles objectId roles =
    request
        { method = "PUT"
        , endpoint = "/roles/" ++ ObjectId.toString objectId
        , body = Just (Encode.object [ ( "roles", removeRelation "_Role" roles ) ])
        , decoder = Request.putDecoder
        }


{-| @todo(aforemny) Move to Pointer
-}
addRelation : String -> List (Pointer a) -> Value
addRelation className pointers =
    Encode.object
        [ ( "__op", Encode.string "AddRelation" )
        , ( "objects", Encode.list (List.map (Encode.pointer className) pointers) )
        ]


{-| @todo(aforemny) Move to Pointer
-}
removeRelation : String -> List (Pointer a) -> Value
removeRelation className pointers =
    Encode.object
        [ ( "__op", Encode.string "RemoveRelation" )
        , ( "objects", Encode.list (List.map (Encode.pointer className) pointers) )
        ]
