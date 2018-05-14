module Internal.ObjectId exposing (ObjectId(..), fromString, toString)


type ObjectId a
    = ObjectId String


toString : ObjectId a -> String
toString objectId =
    case objectId of
        ObjectId objectId ->
            objectId


fromString : String -> ObjectId a
fromString objectId =
    ObjectId objectId
