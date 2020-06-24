module App exposing (main)

import Browser
import Material.TopAppBar as TopAppBar
import Material.IconButton as IconButton
import Html exposing (..)
import Html.Attributes exposing (style)
import Http
import Json.Decode exposing (Decoder, field, string, int)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = rootView
    }


 
-- MODEL

type alias SiteInfo = { sitename: String, username: String, fullname: String, userid: Int }
    
type Model = Failure String
           | Loading
           | Success SiteInfo

type Msg = GotSiteInfo (Result Http.Error SiteInfo)


-- UPDATE

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
      
init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , request
  )

request : Cmd Msg
request =         
          Http.request
              { method = "GET"
              , headers = [ Http.header "CONTENT_TYPE" "application/urlencoded"
                          , Http.header "ACCEPT" "application/json"
                          , Http.header "AUTHORIZATION" "8410c8268bfa0a3dc1e0ed8fb15aed86" ]
              , url = "http://localhost:8080/webservice/restful/server.php/core_webservice_get_site_info?serviceshortnames[]=x"
              , body = Http.emptyBody
              , expect = Http.expectJson GotSiteInfo siteInfoDecoder
              , timeout = Nothing
              , tracker = Nothing    
              }

siteInfoDecoder : Decoder SiteInfo
siteInfoDecoder =
    Json.Decode.map4        
        SiteInfo
        (field "sitename" string)
        (field "username" string)
        (field "fullname" string)
        (field "userid" int)


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
      
    

-- VIEW                        

rootView : Model -> Html Msg
rootView model =
    case model of
        Failure reason ->
            text ("Error connecting to API - " ++ reason)

        Loading ->
            text "Loading..."

        Success siteInfo ->
            TopAppBar.regular (TopAppBar.config |> TopAppBar.setDense True)
            [ TopAppBar.row []
                [ TopAppBar.section
                    [ TopAppBar.alignStart ]
                    [ IconButton.iconButton
                        (IconButton.config
                            |> IconButton.setAttributes [ TopAppBar.navigationIcon ]
                        )
                        "menu"
                    , Html.span [ TopAppBar.title ] [ text siteInfo.sitename ]
                    ]
                , TopAppBar.section
                    [ TopAppBar.alignEnd ]
                    [  Html.span [ TopAppBar.title ] [ text siteInfo.fullname ]
                    ,IconButton.iconButton
                        (IconButton.config
                            |> IconButton.setAttributes [ TopAppBar.actionItem ]
                        )
                        "account_box"
                    ]
                ]
            ]
                        
