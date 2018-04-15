module Parse.Config
    exposing
        ( Config
        , defaultConfig
        )


type alias Config =
    { serverUrl : String
    , applicationId : String
    , restAPIKey : Maybe String
    , javascriptKey : Maybe String
    , clientKey : Maybe String
    , windowsKey : Maybe String
    , masterKey : Maybe String
    , sessionToken : Maybe String
    }


defaultConfig =
    { serverUrl = "http://localhost:1337/parse"
    , applicationId = ""
    , restAPIKey = Nothing
    , javascriptKey = Nothing
    , clientKey = Nothing
    , windowsKey = Nothing
    , masterKey = Nothing
    , sessionToken = Nothing
    }
