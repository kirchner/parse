module Parse
    exposing
        ( Cause
        , Config
        , Constraint
        , Error
        , Object
        , ObjectId
        , Pointer
        , pointer
        , Query
        , SessionToken
        , and
        , code
        , create
        , delete
        , deleteUser
        , emailVerificationRequest
        , emptyQuery
        , encodeQuery
        , equalTo
        , exists
        , get
        , getCurrentUser
        , getUser
        , greaterThan
        , greaterThanOrEqualTo
        , lessThan
        , lessThanOrEqualTo
        , logIn
        , notEqualTo
        , or
        , passwordResetRequest
        , query
        , regex
        , signUp
        , simpleConfig
        , update
        , updateUser
        , Request
        , toTask
        , send
        , Role
        , createRole
        , getRole
        , addUsers
        , deleteUsers
        , addRoles
        , deleteRoles
        , deleteRole
        , ACL
        , RoleName
        , acl
        , anybody
        , users
        , roles
        , Permissions
        , simple
        , extended
        , function
        , job
        , Event
        , post
        , postAt
        , getConfig
        , updateConfig
        )

{-|


# Configuration

@docs Config, simpleConfig

@docs SessionToken


# Getting started

@docs Request
@docs send
@docs toTask


# Objects

@docs create, get, update, delete

@docs Object
@docs ObjectId


# Pointers

@docs Pointer
@docs pointer


# Queries

@docs query, Query, emptyQuery, encodeQuery


## Constraints

@docs Constraint

@docs and, or, exists

@docs equalTo, notEqualTo, regex

@docs lessThan, lessThanOrEqualTo, greaterThan, greaterThanOrEqualTo


# Users

@docs signUp

@docs logIn

@docs emailVerificationRequest, passwordResetRequest

@docs getUser, getCurrentUser, updateUser, deleteUser


# Errors

@docs Error, Cause, code


# ACL

@docs ACL
@docs RoleName
@docs acl
@docs anybody, users, roles


## Permissions

@docs Permissions
@docs simple
@docs extended


# Roles

@docs Role
@docs createRole
@docs getRole
@docs deleteRole


## Updating roles

@docs addUsers
@docs deleteUsers
@docs addRoles
@docs deleteRoles


# Config

@docs getConfig
@docs updateConfig


# Analytics

@docs Event
@docs post
@docs postAt


# Cloud code

@docs function
@docs job
-}

import Date exposing (Date)
import Dict
import Http exposing (Request)
import Internal.ACL
import Internal.Analytics
import Internal.CloudCode
import Internal.Config
import Internal.Error
import Internal.Object
import Internal.ObjectId
import Internal.Pointer
import Internal.Query
import Internal.Request
import Internal.Role
import Internal.Role
import Internal.SessionToken
import Internal.User
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Parse.Decode as Decode
import Task exposing (Task)


---- CONFIG


{-| TODO
-}
type alias Config =
    Internal.Request.Config


{-| TODO
-}
simpleConfig : String -> String -> Config
simpleConfig =
    Internal.Request.simpleConfig


{-| TODO
-}
type alias SessionToken =
    Internal.SessionToken.SessionToken



---- OBJECTS


{-| TODO
-}
type alias ObjectId a =
    Internal.ObjectId.ObjectId a


{-| TODO
-}
type alias Object a =
    Internal.Object.Object a


{-| TODO
-}
create :
    String
    -> (a -> Value)
    -> a
    -> Request { objectId : ObjectId a, createdAt : Date }
create =
    Internal.Object.create


{-| TODO
-}
get : String -> Decoder (Object a) -> ObjectId a -> Request (Object a)
get =
    Internal.Object.get


{-|
The type of `update` is more general to facilitate delta updates. Usually when
doing full updates its type signature is

```elm
update : String -> (a -> Value) -> ObjectId a -> a -> Request { updatedAt : Date }
```
-}
update : String -> (b -> Value) -> ObjectId a -> b -> Request { updatedAt : Date }
update =
    Internal.Object.update


{-| TODO
-}
delete : String -> ObjectId a -> Request {}
delete =
    Internal.Object.delete



-- POINTERS


{-| TODO
-}
type alias Pointer a =
    Internal.Pointer.Pointer a


{-| TODO
-}
pointer : String -> ObjectId a -> Pointer a
pointer =
    Internal.Pointer.pointer



-- QUERY


{-| TODO
-}
type alias Query =
    Internal.Query.Query


{-| TODO

@todo(aforemny) type Query a, query : Query a -> Request (List a)
-}
query : Decoder (Object a) -> Query -> Request (List (Object a))
query =
    Internal.Query.query


{-| TODO
-}
emptyQuery : String -> Query
emptyQuery =
    Internal.Query.emptyQuery


{-| TODO
-}
regex : String -> String -> Constraint
regex =
    Internal.Query.regex


{-| TODO
-}
type alias Constraint =
    Internal.Query.Constraint


{-| TODO
-}
or : List Constraint -> Constraint
or =
    Internal.Query.or


{-| TODO
-}
and : List Constraint -> Constraint
and =
    Internal.Query.and


{-| TODO

@todo(aforemny) notEqualTo : String -> Value -> Constraint
-}
notEqualTo : String -> String -> Constraint
notEqualTo =
    Internal.Query.notEqualTo


{-| TODO

@todo(aforemny) lessThanOrEqualTo : String -> Value -> Constraint
-}
lessThanOrEqualTo : String -> Float -> Constraint
lessThanOrEqualTo =
    Internal.Query.lessThanOrEqualTo


{-| TODO

@todo(aforemny) lessThan : String -> Value -> Constraint
-}
lessThan : String -> Float -> Constraint
lessThan =
    Internal.Query.lessThan


{-| TODO

@todo(aforemny) greaterThanOrEqualTo : String -> Value -> Constraint
-}
greaterThanOrEqualTo : String -> Float -> Constraint
greaterThanOrEqualTo =
    Internal.Query.greaterThanOrEqualTo


{-| TODO

@todo(aforemny) greaterThan : String -> Value -> Constraint
-}
greaterThan : String -> Float -> Constraint
greaterThan =
    Internal.Query.greaterThan


{-| TODO
-}
exists : String -> Constraint
exists =
    Internal.Query.exists


{-| TODO

@todo(aforemny) equalTo : String -> Value -> Constraint
-}
equalTo : String -> String -> Constraint
equalTo =
    Internal.Query.equalTo


{-| TODO
-}
encodeQuery : Query -> Value
encodeQuery =
    Internal.Query.encodeQuery



-- USER


{-| TODO
-}
updateUser : (user -> Value) -> ObjectId user -> user -> Request { updatedAt : Date }
updateUser =
    Internal.User.updateUser


{-| TODO

@todo(aforemny) signUp : (user -> Value) -> { user | username : String, password : String } -> Request { objectId : ObjectId user, createdAt : Date, sessionToken : SessionToken }
-}
signUp :
    (user -> List ( String, Value ))
    -> String
    -> String
    -> user
    -> Request { objectId : ObjectId user, createdAt : Date, sessionToken : SessionToken }
signUp =
    Internal.User.signUp


{-| TODO
-}
logIn :
    Decoder (Object a)
    -> String
    -> String
    -> Request { user : Object a, sessionToken : SessionToken }
logIn =
    Internal.User.logIn


{-| TODO
-}
passwordResetRequest : String -> Request {}
passwordResetRequest =
    Internal.User.passwordResetRequest


{-| TODO
-}
emailVerificationRequest : String -> Request {}
emailVerificationRequest =
    Internal.User.emailVerificationRequest


{-| TODO
-}
deleteUser : ObjectId a -> Request {}
deleteUser =
    Internal.User.deleteUser


{-| TODO
-}
getUser : Decoder (Object a) -> ObjectId a -> Request (Object a)
getUser =
    Internal.User.getUser


{-| TODO
-}
getCurrentUser : Decoder (Object a) -> Request (Object a)
getCurrentUser =
    Internal.User.getCurrentUser



-- ROLES


{-| TODO
-}
type alias ACL user =
    Internal.ACL.ACL user


{-| TODO
-}
type alias RoleName =
    Internal.ACL.RoleName


{-| TODO
-}
acl :
    { anybody : Permissions
    , users : List ( Pointer user, Permissions )
    , roles : List ( RoleName, Permissions )
    }
    -> ACL user
acl =
    Internal.ACL.acl


{-| TODO
-}
anybody : ACL user -> Permissions
anybody =
    Internal.ACL.anybody


{-| TODO
-}
users : ACL user -> List ( Pointer user, Permissions )
users =
    Internal.ACL.users


{-| TODO
-}
roles : ACL user -> List ( RoleName, Permissions )
roles =
    Internal.ACL.roles


{-| TODO
-}
type alias Permissions =
    Internal.ACL.Permissions


{-| TODO
-}
simple : { read : Bool, write : Bool } -> Permissions
simple =
    Internal.ACL.simple


{-| TODO
-}
extended :
    { get : Bool
    , find : Bool
    , write : Bool
    , update : Bool
    , delete : Bool
    , addFields : Bool
    }
    -> Permissions
extended =
    Internal.ACL.extended


{-| TODO
-}
type alias Role user =
    Internal.Role.Role user


{-| TODO
-}
role =
    Internal.Role.role


{-| TODO
-}
createRole :
    Role user
    -> List (Pointer user)
    -> List (Pointer (Role user))
    -> Request { objectId : ObjectId (Role user), createdAt : Date }
createRole =
    Internal.Role.createRole


{-| TODO
-}
getRole : ObjectId (Role user) -> Request (Object (Role user))
getRole =
    Internal.Role.getRole


{-| TODO
-}
addUsers : ObjectId (Role user) -> List (Pointer user) -> Request { updatedAt : Date }
addUsers =
    Internal.Role.addUsers


{-| TODO
-}
deleteUsers : ObjectId (Role user) -> List (Pointer user) -> Request { updatedAt : Date }
deleteUsers =
    Internal.Role.addUsers


{-| TODO
-}
addRoles :
    ObjectId (Role user)
    -> List (Pointer (Role user))
    -> Request { updatedAt : Date }
addRoles =
    Internal.Role.addRoles


{-| TODO
-}
deleteRoles :
    ObjectId (Role user)
    -> List (Pointer (Role user))
    -> Request { updatedAt : Date }
deleteRoles =
    Internal.Role.addRoles


{-| TODO
-}
deleteRole : ObjectId (Role user) -> Request {}
deleteRole =
    Internal.Role.deleteRole



-- ERROR


{-| TODO
-}
type alias Error =
    Internal.Error.Error


{-| TODO
-}
type alias Cause =
    Internal.Error.Cause


{-| TODO
-}
code : Cause -> Int
code =
    Internal.Error.code



-- REQUEST


{-| TODO
-}
type alias Request a =
    Internal.Request.Request a


{-| TODO
-}
toTask : Config -> Request a -> Task Error a
toTask =
    Internal.Request.toTask


{-| TODO
-}
send : Config -> (Result Error a -> m) -> Request a -> Cmd m
send =
    Internal.Request.send



-- CONFIG


{-| TODO
-}
getConfig : Decoder a -> Request a
getConfig =
    Internal.Config.getConfig


{-| TODO
-}
updateConfig : List ( String, Value ) -> Request Bool
updateConfig =
    Internal.Config.updateConfig



-- ANALYTICS


{-| TODO
-}
type alias Event a =
    Internal.Analytics.Event a


{-| TODO
-}
post : (Event a -> List ( String, Value )) -> Event a -> Request {}
post =
    Internal.Analytics.post


{-| TODO
-}
postAt : (Event a -> List ( String, Value )) -> Date -> Event a -> Request {}
postAt =
    Internal.Analytics.postAt



-- CLOUD CODE


{-| TODO
-}
function : String -> Decoder a -> Value -> Request a
function =
    Internal.CloudCode.function


{-| TODO
-}
job : String -> Value -> Request {}
job =
    Internal.CloudCode.job
