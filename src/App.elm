module App exposing (main)

import Browser
import Material.TopAppBar as TopAppBar
import Material.IconButton as IconButton
import Material.Snackbar as Snackbar
import Material.Card as Card
import Material.LayoutGrid as LayoutGrid
import Material.Elevation as Elevation
import Material.List as List
import Material.List.Item as ListItem
import Material.ImageList as ImageList
import Material.ImageList.Item as ImageListItem
import Html exposing (..)
import Html.Attributes exposing (style)
import Http
import Json.Decode exposing (Decoder, field, string, int)


authToken = "8410c8268bfa0a3dc1e0ed8fb15aed86"

-- MODEL

type alias SiteInfo = {
        sitename: String
        ,username: String
        ,userfullname: String
        ,userid: Int
    }

type alias OverviewFile = {
        filename: String
        ,fileurl: String
        ,mimetype: String
    }
    
type alias Course = {
        id: Int
        ,shortname: String
        ,fullname: String
        ,overviewfiles: List OverviewFile
    }

type alias Courses = List Course

type alias Model = {
        siteinfo: Maybe SiteInfo
        ,courses: Maybe Courses       
        ,messages: Snackbar.Queue Msg
        ,debug: Maybe String
    }


type Msg = SiteInfoLoaded  (Result Http.Error SiteInfo)
         | CoursesLoaded  (Result Http.Error Courses)
         | SnackbarMsg (Snackbar.Msg Msg)

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

    

courseListRequest : Int -> (Cmd Msg)
courseListRequest userid =         
          Http.request
              { method = "GET"
              , headers = [ Http.header "CONTENT_TYPE" "application/urlencoded"
                          , Http.header "ACCEPT" "application/json"
                          , Http.header "AUTHORIZATION" authToken ]
              , url = "http://localhost:8080/webservice/restful/server.php/core_enrol_get_users_courses?userid=" ++ String.fromInt userid
              , body = Http.emptyBody
              , expect = Http.expectJson CoursesLoaded courseListDecoder
              , timeout = Nothing
              , tracker = Nothing
              }

courseDecoder : Decoder Course
courseDecoder =
    Json.Decode.map4
        Course
        (field "id" int)
        (field "shortname" string)
        (field "fullname" string)
        (field "overviewfiles" (Json.Decode.list overviewfileDecoder))

overviewfileDecoder : Decoder OverviewFile
overviewfileDecoder =
    Json.Decode.map3
        OverviewFile
        (field "filename" string)
        (field "fileurl" string)
        (field "mimetype" string)
    
courseListDecoder : Decoder Courses
courseListDecoder =
    Json.Decode.list courseDecoder


        
errSnack : Http.Error -> Model -> (Model, Cmd Msg)
errSnack reason model =
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
        (model, Snackbar.addMessage SnackbarMsg message)

    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SiteInfoLoaded result ->
      case result of
        Ok siteInfo ->
           ( { model | siteinfo = Just siteInfo }, courseListRequest siteInfo.userid )
        Err reason ->
            errSnack reason model
    CoursesLoaded result ->
      case result of
        Ok courses ->
           ( { model | courses = Just courses }, Cmd.none )          
        Err reason ->
            errSnack reason model

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

userName : Model -> String
userName model =
    case model.siteinfo of
        Nothing ->
            "Loading"
        Just siteinfo ->
            siteinfo.userfullname

                
printDebug : Model -> String
printDebug model =
    case model.debug of
        Nothing ->
            ""
        Just str ->
            str
             

justList : Maybe (List a) -> List a
justList list =
    case list of
        Nothing ->
            []
        Just list_ ->
            list_

courseImage : OverviewFile -> ImageListItem.ImageListItem msg
courseImage image =
    ImageListItem.imageListItem
        (ImageListItem.config
        |> ImageListItem.setAttributes
             [ style "width" "calc(100% / 5 - 4px)"
             , style "margin" "2px"
             ]
        )
    (image.fileurl ++ "?token=" ++ authToken)
              
courseCard : Course -> Html msg
courseCard course =
    LayoutGrid.cell [ Elevation.z2 ]
        [(Card.card
            (Card.config |> Card.setOutlined True)
            { blocks =
                  [ Card.block <|
                        Html.div [] [ Html.h1 [] [ text course.shortname ]
                                    , ImageList.imageList ImageList.config
                                        (List.map (\img ->  courseImage img ) course.overviewfiles)
                                    ]
                  ]
            , actions = Nothing
            })]
    
                
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
                  [  Html.span [ TopAppBar.title ] [ text (userName model) ]
                  ,IconButton.iconButton
                      (IconButton.config
                      |> IconButton.setAttributes [ TopAppBar.actionItem ]
                      )
                         "account_box"
                  ]
              ]              
        ]
         ,div [ style "padding-top" "128px" ] [
         LayoutGrid.layoutGrid []
                     [ LayoutGrid.inner []
                           (List.map (\c ->  courseCard c ) (justList model.courses))                     
                     ]         
         ]
        ,Snackbar.snackbar SnackbarMsg Snackbar.config model.messages
        ]

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = rootView
    }
 
