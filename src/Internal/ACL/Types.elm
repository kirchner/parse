module Internal.ACL.Types exposing (Role, ACL(..), Permissions, RoleName)

import Internal.Pointer exposing (Pointer)


type alias Role user
    = { name : String
        , acl : ACL user
        }


type alias RoleName =
    String


type ACL user
    = ACL
        { anybody : Permissions
        , roles : List ( RoleName, Permissions )
        , users : List ( Pointer user, Permissions )
        }


type alias Permissions =
    { get : Bool
    , find : Bool
    , write : Bool
    , update : Bool
    , delete : Bool
    , addFields : Bool
    }
