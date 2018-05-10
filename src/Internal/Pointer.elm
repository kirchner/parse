module Internal.Pointer exposing (..)

import Internal.ObjectId exposing (..)


type Pointer a =
    Pointer String (ObjectId a)
