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


authToken = "8410c8268bfa0a3dc1e0ed8fb15aed86"  
 
-- MODEL

type alias SiteInfo = { sitename: String, username: String, fullname: String, userid: Int }
type alias Course = { id: Int, shortname: String, fullname: String }
type alias Courses = List Course
    
type Model = Failure String
           | Loading
           | LoadedSiteInfo SiteInfo
           | LoadedCourses Courses

type Msg = GotSiteInfo (Result Http.Error SiteInfo)
         | GotCourses (Result Http.Error Courses)


-- UPDATE

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
      
init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , siteInfoRequest
  )

siteInfoRequest : Cmd Msg
siteInfoRequest =         
          Http.request
              { method = "GET"
              , headers = [ Http.header "CONTENT_TYPE" "application/urlencoded"
                          , Http.header "ACCEPT" "application/json"
                          , Http.header "AUTHORIZATION" authToken ]
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




courseListRequest : Cmd Msg
courseListRequest =         
          Http.request
              { method = "GET"
              , headers = [ Http.header "CONTENT_TYPE" "application/urlencoded"
                          , Http.header "ACCEPT" "application/json"
                          , Http.header "AUTHORIZATION" authToken ]
              , url = "http://localhost:8080/webservice/restful/server.php/core_enrol_get_users_courses?userid=3"
              , body = Http.emptyBody
              , expect = Http.expectJson GotCourses courseListDecoder
              , timeout = Nothing
              , tracker = Nothing
              }

courseDecoder : Decoder Course
courseDecoder =
    Json.Decode.map3
        Course
        (field "id" int)
        (field "shortname" string)
        (field "fullname" string)
    
courseListDecoder : Decoder Courses
courseListDecoder =
    Json.Decode.list courseDecoder


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotSiteInfo result ->
      case result of
        Ok siteInfo ->
          (LoadedSiteInfo siteInfo, courseListRequest)
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
              
    GotCourses result ->
      case result of
        Ok courses ->
            (LoadedCourses courses, Cmd.none)
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

        LoadedSiteInfo siteInfo ->
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

        LoadedCourses courses ->
             text "Courses"
                        
