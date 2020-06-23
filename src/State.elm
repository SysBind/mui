module State exposing (..)

import Http
import Types exposing (..)
import Rest exposing (request)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Rest.request
  )


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotSiteInfo result ->
      case result of
        Ok siteInfo ->
          (Success siteInfo, Cmd.none)

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
      
