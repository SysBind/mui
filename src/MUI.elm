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

type alias School = { sitename: String }

type Model = Failure String
           | Loading
           | Success School


-- INIT

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.request
      { method = "GET"
      , headers = [ Http.header "CONTENT_TYPE" "application/urlencoded"
                  , Http.header "ACCEPT" "application/json"
                  , Http.header "AUTHORIZATION" "8410c8268bfa0a3dc1e0ed8fb15aed86" ]
      , url = "http://localhost:8080/webservice/restful/server.php/core_webservice_get_site_info?serviceshortnames[]=x"
      , body = Http.emptyBody
      , expect = Http.expectJson GotSchool schoolDecoder
      , timeout = Nothing
      , tracker = Nothing    
      }
  )

schoolDecoder : Decoder School
schoolDecoder =
    Json.Decode.map
        School
        (field "sitename" string)

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
                [ text (school.sitename) ]

