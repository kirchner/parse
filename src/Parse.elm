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
        )

{-|


# Configuration

@docs Config, simpleConfig

@docs SessionToken


# REST Actions

@docs create, get, update, delete

@docs Object
@docs ObjectId
@docs equal

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

-}

import Date exposing (Date)
import Dict
import Http exposing (Request)
import Internal.ACL
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
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Parse.Decode as Decode
import Task exposing (Task)


---- CONFIGURATION


type alias Config =
    Internal.Config.Config


{-| -}
simpleConfig serverUrl applicationId =
    Internal.Config.simpleConfig


{-| -}
type alias SessionToken =
    Internal.SessionToken.SessionToken



---- OBJECTS


{-| -}
type alias ObjectId a =
    Internal.ObjectId.ObjectId a


{-| -}
type alias Object a =
    Internal.Object.Object a


create =
    Internal.Object.create


get =
    Internal.Object.get


{-|
The type of `update` is more general to facilitate delta updates. Usually when
doing full updates its type signature is

```elm
update : String -> (a -> Value) -> ObjectId a -> a -> Request { updatedAt : Date }
```
-}
update =
    Internal.Object.update


delete =
    Internal.Object.delete



-- POINTERS


{-| -}
type alias Pointer a =
    Internal.Pointer.Pointer a


{-| -}
pointer : String -> ObjectId a -> Pointer a
pointer =
    Internal.Pointer.pointer



-- QUERY


type alias Query =
    Internal.Query.Query


query =
    Internal.Query.query


emptyQuery =
    Internal.Query.emptyQuery


regex =
    Internal.Query.regex


type alias Constraint =
    Internal.Query.Constraint


or =
    Internal.Query.or


and =
    Internal.Query.and


notEqualTo =
    Internal.Query.notEqualTo


lessThanOrEqualTo =
    Internal.Query.lessThanOrEqualTo


lessThan =
    Internal.Query.lessThan


greaterThanOrEqualTo =
    Internal.Query.greaterThanOrEqualTo


greaterThan =
    Internal.Query.greaterThan


exists =
    Internal.Query.exists


equalTo =
    Internal.Query.equalTo


encodeQuery =
    Internal.Query.encodeQuery



-- USER


updateUser =
    Internal.User.updateUser


signUp =
    Internal.User.signUp


logIn =
    Internal.User.logIn


passwordResetRequest =
    Internal.User.passwordResetRequest


emailVerificationRequest =
    Internal.User.emailVerificationRequest


deleteUser =
    Internal.User.deleteUser


getUser =
    Internal.User.getUser


getCurrentUser =
    Internal.User.getCurrentUser



-- ROLES


type alias ACL user =
    Internal.ACL.ACL user


anybody =
    Internal.ACL.anybody


roles =
    Internal.ACL.roles


type alias Permissions =
    Internal.ACL.Permissions


simple =
    Internal.ACL.simple


extended =
    Internal.ACL.extended


type alias Role user =
    Internal.Role.Role user


role =
    Internal.Role.role


createRole =
    Internal.Role.createRole



-- ERROR


type alias Error =
    Internal.Error.Error


type alias Cause =
    Internal.Error.Cause


code =
    Internal.Error.code



-- REQUEST


type alias Request a =
    Internal.Request.Request a


toTask =
    Internal.Request.toTask


send =
    Internal.Request.send
