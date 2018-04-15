module Main exposing (main)

import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Parse exposing (Error, ObjectId, Param)
import Task exposing (Task)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { title : String
    , description : String
    , events : List Event
    , titleQuery : String
    , descriptionQuery : String
    }


init : ( Model, Cmd Msg )
init =
    ( { title = ""
      , description = ""
      , events = []
      , titleQuery = ""
      , descriptionQuery = ""
      }
    , getAllEvents
        |> Task.attempt
            (\result ->
                case result of
                    Ok events ->
                        EventsReceived events

                    Err error ->
                        EventGetAllFailed error
            )
    )



---- UPDATE


type Msg
    = TitleUpdated String
    | DescriptionUpdated String
    | FormSubmitted
    | EventCreated { createdAt : Date, objectId : ObjectId }
    | EventCreateFailed Error
    | EventReceived Event
    | EventGetFailed Error
    | EventsReceived (List Event)
    | EventGetAllFailed Error
    | DeleteEventClicked ObjectId
    | EventDeleted ()
    | EventDeleteFailed Error
      -- FILTER
    | FilterFormSubmitted
    | TitleQueryUpdated String
    | DescriptionQueryUpdated String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        TitleUpdated newTitle ->
            ( { model | title = newTitle }
            , Cmd.none
            )

        DescriptionUpdated newDescription ->
            ( { model | description = newDescription }
            , Cmd.none
            )

        FormSubmitted ->
            ( model
            , createEvent
                { title = model.title
                , description = model.description
                }
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok info ->
                                EventCreated info

                            Err error ->
                                EventCreateFailed error
                    )
            )

        EventCreated { createdAt, objectId } ->
            ( model
            , getEvent objectId
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok event ->
                                EventReceived event

                            Err error ->
                                EventGetFailed error
                    )
            )

        EventCreateFailed error ->
            ( model
            , Cmd.none
            )

        EventReceived event ->
            ( { model | events = event :: model.events }
            , Cmd.none
            )

        EventGetFailed error ->
            ( model
            , Cmd.none
            )

        EventsReceived events ->
            ( { model | events = events }
            , Cmd.none
            )

        EventGetAllFailed error ->
            ( model
            , Cmd.none
            )

        DeleteEventClicked objectId ->
            ( model
            , deleteEvent objectId
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok event ->
                                EventDeleted event

                            Err error ->
                                EventDeleteFailed error
                    )
            )

        EventDeleted _ ->
            ( model
            , getAllEvents
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok events ->
                                EventsReceived events

                            Err error ->
                                EventGetAllFailed error
                    )
            )

        EventDeleteFailed error ->
            ( model
            , Cmd.none
            )

        FilterFormSubmitted ->
            ( model
            , getAllEventsWith
                [ Parse.constraint <|
                    Parse.and
                        [ Parse.regex "title" model.titleQuery
                        , Parse.exists "title"
                        , Parse.regex "description" model.descriptionQuery
                        ]
                , Parse.count
                ]
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok events ->
                                EventsReceived events

                            Err error ->
                                EventGetAllFailed error
                    )
            )

        TitleQueryUpdated newTitleQuery ->
            ( { model | titleQuery = newTitleQuery }
            , Cmd.none
            )

        DescriptionQueryUpdated newDescriptionQuery ->
            ( { model | descriptionQuery = newDescriptionQuery }
            , Cmd.none
            )


parseConfig : Parse.Config
parseConfig =
    { applicationId = "parse-example"
    , restApiKey = "secret"
    , serverUrl = "http://localhost:1337/parse"
    }



---- CLASSES


type alias Event =
    { objectId : ObjectId
    , title : String
    , description : String
    }


createEvent :
    { title : String
    , description : String
    }
    -> Task Error { createdAt : Date, objectId : ObjectId }
createEvent =
    Parse.create "Event" encodeEvent parseConfig


getEvent : ObjectId -> Task Error Event
getEvent =
    Parse.get "Event" eventDecoder parseConfig


getAllEvents : Task Error (List Event)
getAllEvents =
    Parse.query "Event" eventDecoder parseConfig []


getAllEventsWith : List Param -> Task Error (List Event)
getAllEventsWith params =
    Parse.query "Event" eventDecoder parseConfig params


deleteEvent : ObjectId -> Task Error ()
deleteEvent =
    Parse.delete "Event" parseConfig


encodeEvent : { title : String, description : String } -> Value
encodeEvent event =
    [ ( "title", Encode.string event.title )
    , ( "description", Encode.string event.description )
    ]
        |> Encode.object


eventDecoder : Decoder Event
eventDecoder =
    Decode.succeed Event
        |> Decode.required "objectId" Parse.objectIdDecoder
        |> Decode.required "title" Decode.string
        |> Decode.required "description" Decode.string



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.ul [] <|
            List.map viewEvent model.events
        , Html.form
            [ Events.onSubmit FormSubmitted
            ]
            [ Html.label []
                [ Html.text "Title" ]
            , Html.input
                [ Attributes.type_ "text"
                , Attributes.defaultValue model.title
                , Events.onInput TitleUpdated
                ]
                []
            , Html.label []
                [ Html.text "Description" ]
            , Html.input
                [ Attributes.type_ "text"
                , Attributes.defaultValue model.description
                , Events.onInput DescriptionUpdated
                ]
                []
            , Html.button
                []
                [ Html.text "Add" ]
            ]
        , Html.form
            [ Events.onSubmit FilterFormSubmitted ]
            [ Html.label []
                [ Html.text "Title" ]
            , Html.input
                [ Attributes.type_ "text"
                , Attributes.defaultValue model.titleQuery
                , Events.onInput TitleQueryUpdated
                ]
                []
            , Html.label []
                [ Html.text "Description" ]
            , Html.input
                [ Attributes.type_ "text"
                , Attributes.defaultValue model.descriptionQuery
                , Events.onInput DescriptionQueryUpdated
                ]
                []
            , Html.button
                []
                [ Html.text "Filter" ]
            ]
        ]


viewEvent : Event -> Html Msg
viewEvent event =
    Html.li []
        [ Html.strong [] [ Html.text event.title ]
        , Html.text (": " ++ event.description)
        , Html.a
            [ Events.onWithOptions "click"
                { preventDefault = True
                , stopPropagation = False
                }
                (Decode.succeed (DeleteEventClicked event.objectId))
            , Attributes.href "#"
            ]
            [ Html.text "Delete" ]
        ]
