module App exposing (main)

import Browser
import Material.TopAppBar as TopAppBar
import Material.IconButton as IconButton
import Material.Snackbar as Snackbar
import Html exposing (..)
import Html.Attributes exposing (style)
import Http
import Json.Decode exposing (Decoder, field, string, int)


authToken = "8410c8268bfa0a3dc1e0ed8fb15aed86"

-- MODEL

type alias SiteInfo = {
        sitename: String,
        username: String,
        userfullname: String,
        userid: Int
    }

type alias Course = {
        id: Int,
        shortname: String,
        fullname: String
    }

type alias Courses = List Course

type alias Model = {
        siteinfo: Maybe SiteInfo,
        courses: Maybe Courses,        
        messages: Snackbar.Queue Msg,
        debug: Maybe String
    }


type Msg = SiteInfoLoaded  (Result Http.Error SiteInfo)
         | SnackbarMsg (Snackbar.Msg Msg)
--         | CoursesLoaded

init : () -> (Model, Cmd Msg)
init _ =
  ( { siteinfo = Nothing,
      courses = Nothing,
      messages = Snackbar.initialQueue,
      debug = Nothing
    }
  , siteInfoRequest
  )
    
-- UPDATE

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

siteInfoRequest : Cmd Msg
siteInfoRequest =         
          Http.request
              { method = "GET"
              , headers = [ Http.header "CONTENT_TYPE" "application/urlencoded"
                          , Http.header "ACCEPT" "application/json"
                          , Http.header "AUTHORIZATION" authToken ]
              , url = "http://localhost:8080/webservice/restful/server.php/core_webservice_get_site_info?serviceshortnames[]=x"
              , body = Http.emptyBody
              , expect = Http.expectJson SiteInfoLoaded siteInfoDecoder
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



{--
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

--}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SiteInfoLoaded result ->
      case result of
        Ok siteInfo ->
           ( { model | siteinfo = Just siteInfo }, Cmd.none )          
        Err reason ->
             let
                message =
                    case reason of
                        Http.BadUrl str ->
                            Snackbar.message
                                |> Snackbar.setLabel (Just ("BADURL " ++ str))
                        Http.Timeout ->
                            Snackbar.message
                                |> Snackbar.setLabel (Just "TIMEOUT")
                        Http.NetworkError ->
                            Snackbar.message
                                |> Snackbar.setLabel (Just "NETWORKERROR")
                        Http.BadStatus status ->
                            Snackbar.message
                                |> Snackbar.setLabel (Just ("BADSTATUS " ++ String.fromInt status))
                        Http.BadBody str ->
                            Snackbar.message
                                |> Snackbar.setLabel (Just ("BADBODY " ++ str))
             in
                 ( model, Snackbar.addMessage SnackbarMsg message )

    SnackbarMsg snackbarMsg ->
        Snackbar.update SnackbarMsg snackbarMsg model.messages
                |> Tuple.mapFirst (\queue -> { model | messages = queue })

-- VIEW                        

siteName : Model -> String
siteName model =
    case model.siteinfo of
        Nothing ->
            "Loading"
        Just siteinfo ->
            siteinfo.sitename

printDebug : Model -> String
printDebug model =
    case model.debug of
        Nothing ->
            ""
        Just str ->
            str
             

rootView : Model -> Html Msg
rootView model =
    div [] [
     TopAppBar.regular (TopAppBar.config |> TopAppBar.setDense True)
        [ TopAppBar.row []
              [ TopAppBar.section
                    [ TopAppBar.alignStart ]
                    [ IconButton.iconButton
                          (IconButton.config
                          |> IconButton.setAttributes [ TopAppBar.navigationIcon ]
                          )
                          "menu"
                    , Html.span [ TopAppBar.title ] [ text (siteName model) ]
                    ]
              , TopAppBar.section
                  [ TopAppBar.alignEnd ]
                  [  Html.span [ TopAppBar.title ] [ text (printDebug model) ]
                  ,IconButton.iconButton
                      (IconButton.config
                      |> IconButton.setAttributes [ TopAppBar.actionItem ]
                      )
                         "account_box"
                  ]
              ]              
        ]
        , Snackbar.snackbar SnackbarMsg Snackbar.config model.messages

    ]

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = rootView
    }
 
