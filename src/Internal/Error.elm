module Internal.Error exposing (Error(..), Cause, errorDecoder, code)

import Http
import Json.Decode as Decode exposing (Decoder, Value)

{-| -}

---- ERRORS


type Error
    = ParseError
        { cause : Cause
        , error : String
        }
    | HttpError Http.Error


{-| -}
type Cause
    = OtherCause
    | InternalServerError
    | ConnectionFailed
    | ObjectNotFound
    | InvalidQuery
    | InvalidClassName
    | MissingObjectId
    | InvalidKeyName
    | InvalidPointer
    | InvalidJSON
    | CommandUnavailable
    | NotInitialized
    | IncorrectType
    | InvalidChannelName
    | PushMisconfigured
    | ObjectTooLarge
    | OperationForbidden
    | CacheMiss
    | InvalidNestedKey
    | InvalidFileName
    | InvalidACL
    | Timeout
    | InvalidEmailAddress
    | MissingContentType
    | MissingContentLength
    | InvalidContentType
    | FileTooLarge
    | FileSaveError
    | DuplicateValue
    | InvalidRoleName
    | ExceededQuota
    | ScriptFailed
    | ValidationError
    | InvalidImageData
    | UnsavedFileError
    | InvalidPushTimeError
    | FileDeleteError
    | RequestLimitExceeded
    | InvalidEventName
    | UsernameMissing
    | Passwordmissing
    | UsernameTaken
    | EmailTaken
    | EmailMissing
    | EmailNotFound
    | SessionMissing
    | MustCreateUserThroughSignup
    | AccountAlreadyLinked
    | InvalidSessionToken
    | LinkedIdMissing
    | InvalidLinkedSession
    | UnsupportedService
    | InvalidSchemaOperation
    | AggregateError
    | FileReadError
    | XDomainRequest


errorDecoder : Decoder { cause : Cause, error : String }
errorDecoder =
    Decode.map2 (\cause error -> { cause = cause, error = error })
        (Decode.field "code" causeDecoder)
        (Decode.field "error" Decode.string)


{-| -}
code : Cause -> Int
code cause =
    case cause of
        OtherCause ->
            -1

        InternalServerError ->
            1

        ConnectionFailed ->
            100

        ObjectNotFound ->
            101

        InvalidQuery ->
            102

        InvalidClassName ->
            103

        MissingObjectId ->
            104

        InvalidKeyName ->
            105

        InvalidPointer ->
            106

        InvalidJSON ->
            107

        CommandUnavailable ->
            108

        NotInitialized ->
            109

        IncorrectType ->
            111

        InvalidChannelName ->
            112

        PushMisconfigured ->
            115

        ObjectTooLarge ->
            116

        OperationForbidden ->
            119

        CacheMiss ->
            120

        InvalidNestedKey ->
            121

        InvalidFileName ->
            122

        InvalidACL ->
            123

        Timeout ->
            124

        InvalidEmailAddress ->
            125

        MissingContentType ->
            126

        MissingContentLength ->
            127

        InvalidContentType ->
            128

        FileTooLarge ->
            129

        FileSaveError ->
            130

        DuplicateValue ->
            137

        InvalidRoleName ->
            139

        ExceededQuota ->
            140

        ScriptFailed ->
            141

        ValidationError ->
            142

        InvalidImageData ->
            143

        UnsavedFileError ->
            151

        InvalidPushTimeError ->
            152

        FileDeleteError ->
            153

        RequestLimitExceeded ->
            155

        InvalidEventName ->
            160

        UsernameMissing ->
            200

        Passwordmissing ->
            201

        UsernameTaken ->
            202

        EmailTaken ->
            203

        EmailMissing ->
            204

        EmailNotFound ->
            205

        SessionMissing ->
            206

        MustCreateUserThroughSignup ->
            207

        AccountAlreadyLinked ->
            208

        InvalidSessionToken ->
            209

        LinkedIdMissing ->
            250

        InvalidLinkedSession ->
            251

        UnsupportedService ->
            252

        InvalidSchemaOperation ->
            255

        AggregateError ->
            600

        FileReadError ->
            601

        XDomainRequest ->
            602


causeDecoder : Decoder Cause
causeDecoder =
    Decode.int
        |> Decode.andThen
            (\code ->
                case code of
                    (-1) ->
                        Decode.succeed OtherCause

                    1 ->
                        Decode.succeed InternalServerError

                    100 ->
                        Decode.succeed ConnectionFailed

                    101 ->
                        Decode.succeed ObjectNotFound

                    102 ->
                        Decode.succeed InvalidQuery

                    103 ->
                        Decode.succeed InvalidClassName

                    104 ->
                        Decode.succeed MissingObjectId

                    105 ->
                        Decode.succeed InvalidKeyName

                    106 ->
                        Decode.succeed InvalidPointer

                    107 ->
                        Decode.succeed InvalidJSON

                    108 ->
                        Decode.succeed CommandUnavailable

                    109 ->
                        Decode.succeed NotInitialized

                    111 ->
                        Decode.succeed IncorrectType

                    112 ->
                        Decode.succeed InvalidChannelName

                    115 ->
                        Decode.succeed PushMisconfigured

                    116 ->
                        Decode.succeed ObjectTooLarge

                    119 ->
                        Decode.succeed OperationForbidden

                    120 ->
                        Decode.succeed CacheMiss

                    121 ->
                        Decode.succeed InvalidNestedKey

                    122 ->
                        Decode.succeed InvalidFileName

                    123 ->
                        Decode.succeed InvalidACL

                    124 ->
                        Decode.succeed Timeout

                    125 ->
                        Decode.succeed InvalidEmailAddress

                    126 ->
                        Decode.succeed MissingContentType

                    127 ->
                        Decode.succeed MissingContentLength

                    128 ->
                        Decode.succeed InvalidContentType

                    129 ->
                        Decode.succeed FileTooLarge

                    130 ->
                        Decode.succeed FileSaveError

                    137 ->
                        Decode.succeed DuplicateValue

                    139 ->
                        Decode.succeed InvalidRoleName

                    140 ->
                        Decode.succeed ExceededQuota

                    141 ->
                        Decode.succeed ScriptFailed

                    142 ->
                        Decode.succeed ValidationError

                    143 ->
                        Decode.succeed InvalidImageData

                    151 ->
                        Decode.succeed UnsavedFileError

                    152 ->
                        Decode.succeed InvalidPushTimeError

                    153 ->
                        Decode.succeed FileDeleteError

                    155 ->
                        Decode.succeed RequestLimitExceeded

                    160 ->
                        Decode.succeed InvalidEventName

                    200 ->
                        Decode.succeed UsernameMissing

                    201 ->
                        Decode.succeed Passwordmissing

                    202 ->
                        Decode.succeed UsernameTaken

                    203 ->
                        Decode.succeed EmailTaken

                    204 ->
                        Decode.succeed EmailMissing

                    205 ->
                        Decode.succeed EmailNotFound

                    206 ->
                        Decode.succeed SessionMissing

                    207 ->
                        Decode.succeed MustCreateUserThroughSignup

                    208 ->
                        Decode.succeed AccountAlreadyLinked

                    209 ->
                        Decode.succeed InvalidSessionToken

                    250 ->
                        Decode.succeed LinkedIdMissing

                    251 ->
                        Decode.succeed InvalidLinkedSession

                    252 ->
                        Decode.succeed UnsupportedService

                    255 ->
                        Decode.succeed InvalidSchemaOperation

                    600 ->
                        Decode.succeed AggregateError

                    601 ->
                        Decode.succeed FileReadError

                    602 ->
                        Decode.succeed XDomainRequest

                    _ ->
                        [ "'"
                        , toString code
                        , "' is not a valid Parse error code"
                        ]
                            |> String.concat
                            |> Decode.fail
            )
