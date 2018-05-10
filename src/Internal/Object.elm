module Internal.Object exposing (..)

import Date exposing (Date)
import Internal.ObjectId exposing (..)


type alias Object a =
    { a | objectId : ObjectId a
        , createdAt : Date
        , updatedAt : Date
    }
