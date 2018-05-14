module Internal.Config exposing (Config, simpleConfig)

import Internal.SessionToken exposing (SessionToken)


{-| -}
type alias Config =
    { serverUrl : String
    , applicationId : String
    , restAPIKey : Maybe String
    , javascriptKey : Maybe String
    , clientKey : Maybe String
    , windowsKey : Maybe String
    , masterKey : Maybe String
    , sessionToken : Maybe SessionToken
    }


simpleConfig : String -> String -> Config
simpleConfig serverUrl applicationId =
    Config
        serverUrl
        applicationId
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
