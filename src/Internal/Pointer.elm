module Internal.Pointer exposing (Pointer(..), pointer, objectId, className)

import Internal.ObjectId exposing (..)


type Pointer a
    = Pointer String (ObjectId a)


pointer : String -> ObjectId a -> Pointer a
pointer =
    Pointer


className : Pointer a -> String
className pointer =
    case pointer of
        Pointer className _ ->
            className


objectId : Pointer a -> ObjectId a
objectId pointer =
    case pointer of
        Pointer _ objectId ->
            objectId
