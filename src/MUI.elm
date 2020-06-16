module MUI exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode as Encode
import Array


-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias School = { shortname: String, fullname: String }

type Model = Failure String
           | Loading
           | Success School


-- INIT

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.request
      { method = "GET"
      , headers = [ Http.header "CONTENT_TYPE" "json"
                  , Http.header "ACCEPT" "application/json"
                  , Http.header "AUTHORIZATION" "e786e74631683916cc4cc277707ad5c3" ]
      , url = "http://localhost:8080/webservice/restful/server.php/core_course_get_courses"
      , body = Http.jsonBody (Encode.object [ ( "options", Encode.string "tom" ) ])
      , expect = Http.expectJson GotSchool schoolDecoder
      , timeout = Nothing
      , tracker = Nothing    
      }
  )

schoolDecoder : Decoder School
schoolDecoder =
    Json.Decode.map2
        School
        (field "shortname" string)
        (field "fullname" string)

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotSchool result ->
      case result of
        Ok school ->
          (Success school, Cmd.none)

        Err reason ->
            case reason of
                Http.BadUrl str ->
                    (Failure ("BADURL " ++ str), Cmd.none)
                Http.Timeout ->
                    (Failure "TIMEOUT", Cmd.none)
                Http.NetworkError ->
                    (Failure "NETWORKERROR", Cmd.none)
                Http.BadStatus status ->
                    (Failure ("BADSTATYS" ++ String.fromInt status), Cmd.none)
                Http.BadBody str ->
                    (Failure ("BADBODY " ++ str), Cmd.none)
type Msg
  = GotSchool (Result Http.Error School)
              

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW



view : Model -> Html Msg
view model =
    case model of
        Failure reason ->
            text ("Error connecting to API - " ++ reason)

        Loading ->
            text "Loading..."

        Success school ->
            div []
                [ text (school.fullname ++ " <" ++ school.shortname ++ ">" ) ]

