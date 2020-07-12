module App exposing (main)

import Browser
import Material.TopAppBar as TopAppBar
import Material.IconButton as IconButton
import Material.Snackbar as Snackbar
import Material.Card as Card
import Material.LayoutGrid as LayoutGrid
import Material.Elevation as Elevation
import Material.List as MDList
import Material.List.Item as MDListItem
import Material.ImageList as ImageList
import Material.ImageList.Item as ImageListItem
import Html exposing (..)
import Html.Attributes exposing (style,src)
import Html.Events
import Http
import Json.Decode exposing (Decoder, field, string, int)


-- authToken = "8410c8268bfa0a3dc1e0ed8fb15aed86"
authToken = "1d2bad20a3fee88082abc23cc9557a69"
moodleEndpoint = "http://localhost:8000/webservice/restful/server.php"

-- MODEL

type alias SiteInfo = {
        sitename: String
        ,username: String
        ,userfullname: String
        ,userid: Int
    }


-- Course Module
type alias Module = {
        id: Int
        ,name: String
        ,modicon: String
        ,modplural: String
    }

type alias ModuleCompletion = {
        cmid: Int
        ,modname: String
        ,state: Bool
    }

-- Course Section
type alias Section =  {
        id: Int
        ,name: String
        ,modules: List Module
    }

-- Course Image (called overview file in API)
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
        ,sections: Maybe (List Section)
        ,statuses: List ModuleCompletion
    }

type alias Courses = List Course

type alias Model = {
        siteinfo: Maybe SiteInfo
        ,courses: Maybe Courses       
        ,messages: Snackbar.Queue Msg
        ,currentcourse: Maybe Course
        ,debug: Maybe String
    }


-- Messages    
type Msg = SiteInfoLoaded  (Result Http.Error SiteInfo)
         | CoursesLoaded  (Result Http.Error Courses)
         | CourseClicked Int
         | CourseContentLoaded (Result Http.Error (List Section))
         | SnackbarClosed Snackbar.MessageId

init : () -> (Model, Cmd Msg)
init _ =
  ( { siteinfo = Nothing
    ,courses = Nothing
    ,messages = Snackbar.initialQueue
    ,currentcourse = Nothing
    ,debug = Nothing
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
              , url = moodleEndpoint ++ "/core_webservice_get_site_info?serviceshortnames[]=x"
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
              , url = moodleEndpoint ++ "/core_enrol_get_users_courses?userid=" ++ String.fromInt userid
              , body = Http.emptyBody
              , expect = Http.expectJson CoursesLoaded courseListDecoder
              , timeout = Nothing
              , tracker = Nothing
              }

courseDecoder : Decoder Course
courseDecoder =
    Json.Decode.map6
        Course
        (field "id" int)
        (field "shortname" string)
        (field "fullname" string)
        (field "overviewfiles" (Json.Decode.list overviewfileDecoder))
        (Json.Decode.succeed Nothing)
        (Json.Decode.succeed [])

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


-- courseContentRequest - Fetch course sections and modules
courseContentRequest : Int -> (Cmd Msg)
courseContentRequest courseid =         
          Http.request
              { method = "GET"
              , headers = [ Http.header "CONTENT_TYPE" "application/urlencoded"
                          , Http.header "ACCEPT" "application/json"
                          , Http.header "AUTHORIZATION" authToken ]
              , url = moodleEndpoint ++ "/core_course_get_contents?courseid=" ++ String.fromInt courseid
              , body = Http.emptyBody
              , expect = Http.expectJson CourseContentLoaded courseContentDecoder
              , timeout = Nothing
              , tracker = Nothing
              }        


courseContentDecoder : Decoder (List Section)
courseContentDecoder =
    Json.Decode.list sectionDecoder

sectionDecoder : Decoder Section
sectionDecoder =
    Json.Decode.map3
        Section
        (field "id" int)
        (field "name" string)
        (field "modules" (Json.Decode.list moduleDecoder) )

moduleDecoder : Decoder Module
moduleDecoder =
    Json.Decode.map4
        Module
            (field "id" int)
            (field "name" string)
            (field "modicon" string)
            (field "modplural" string)

-- moduleCompletionRequest
moduleCompletionRequest : (Maybe SiteInfo) -> Courses  -> (Cmd Msg)
moduleCompletionRequest siteinfo courses =
    case siteinfo of
        Nothing ->
            Cmd.none
        Just siteInfo ->
            Cmd.batch
                (List.map (\course ->
                  Http.request
                  { method = "GET"
                  , headers = [ Http.header "CONTENT_TYPE" "application/urlencoded"
                              , Http.header "ACCEPT" "application/json"
                              , Http.header "AUTHORIZATION" authToken ]
                  , url = moodleEndpoint ++ "/core_completion_get_activities_completion_status?userid=" ++ String.fromInt siteInfo.userid ++ "&courseid=" ++ String.fromInt course.id
                  , body = Http.emptyBody
                  , expect = Http.expectJson CourseContentLoaded courseContentDecoder
                  , timeout = Nothing
                  , tracker = Nothing
                  })
                courses)
        
                          
errSnack : Http.Error -> Model -> (Model, Cmd Msg)
errSnack reason model =
    let
        message =
            case reason of
                Http.BadUrl str ->
                    Snackbar.message ("BADURL " ++ str)
                        |> Snackbar.setTimeoutMs (Just 4000)
                Http.Timeout ->
                    Snackbar.message "TIMEOUT"
                        |> Snackbar.setTimeoutMs (Just 4000)
                Http.NetworkError ->
                    Snackbar.message "NETWORKERROR"
                        |> Snackbar.setTimeoutMs (Just 4000)
                Http.BadStatus status ->
                    Snackbar.message ("BADSTATUS " ++ String.fromInt status)
                        |> Snackbar.setTimeoutMs (Just 4000)
                Http.BadBody str ->
                    Snackbar.message ("BADBODY " ++ str)
                        |> Snackbar.setTimeoutMs (Just 4000)
    in
        ({model | messages = Snackbar.addMessage message model.messages, debug=Just "http error"}, Cmd.none)

courseById : Courses -> Int -> Maybe Course
courseById courses id =
    List.filter (\course -> course.id == id) courses
        |> List.head
             

setCourseSections: List Section -> Course -> Course
setCourseSections secs course =
    { course | sections = Just secs }

                   
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
           ( { model | courses = Just courses }, moduleCompletionRequest model.siteinfo courses )
        Err reason ->
            errSnack reason model
                
    CourseClicked id ->
        ( { model | currentcourse = courseById (justList model.courses) id }, courseContentRequest id )


    CourseContentLoaded result ->
        case result of
          Ok sections ->
              case model.currentcourse of
                  Nothing ->
                      ( model , Cmd.none )
                  Just course ->
                      ( { model | currentcourse = Just (course |> setCourseSections sections) }, Cmd.none )          
          Err reason ->
            errSnack reason model
    SnackbarClosed msgid ->
        ({ model | messages = Snackbar.close msgid model.messages }, Cmd.none )

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
              
courseCard : Course -> Html Msg
courseCard course =
    LayoutGrid.cell [ Elevation.z2 ]
        [(Card.card
            (Card.config |> Card.setOutlined True)
            { blocks =
                  Card.primaryAction
                  [ Html.Events.onClick (CourseClicked course.id) ]
                  [ Card.block <|
                        Html.div [] [
                                     ImageList.imageList ImageList.config
                                        (List.map (\img ->  courseImage img ) course.overviewfiles)
                                          ,Html.h1 [] [ text course.shortname ]
                                    ]
                  ]
            , actions = Nothing
            })]
    
-- Container View            
rootView : Model -> Html Msg
rootView model =
    let
        centerView =
            case model.currentcourse of
                Nothing ->
                    homeView
                Just course ->
                    courseView
    in
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
            ,div [ style "padding-top" "128px"
                 , style "width" "100%"
                 , style "max-width" "1200px"
                 , style "margin" "0 auto"
                 ] [ centerView model ]
            , div [] [ text (printDebug model) ]
            ,Snackbar.snackbar ( Snackbar.config  { onClosed = SnackbarClosed } ) model.messages
            ]

-- HomeView        
homeView : Model -> Html Msg
homeView model =    
    LayoutGrid.layoutGrid []
        [ LayoutGrid.inner []
              (List.map (\c ->  courseCard c ) (justList model.courses))                     
        ]         
        
courseView : Model -> Html Msg
courseView model =
    case model.currentcourse of
        Just course ->
            Html.div [] [
                 ImageList.imageList ImageList.config
                     (List.map (\img ->  courseImage img ) course.overviewfiles)
                ,Html.h2 [ style "user-select" "none" ] [ text course.shortname ]                            
                ,LayoutGrid.layoutGrid []
                     [ LayoutGrid.inner []
                           (List.map (\sec ->  sectionView sec ) (justList course.sections))
                     ]
                ]
        Nothing ->
            div [] [ text "NO CURRENT COURSE !" ]
                    
                
sectionView : Section -> Html Msg
sectionView section =
    let
        content =
            case section.modules of
                [] ->
                    div [] []
                        
                x :: xs ->       
                    div [] [
                         Html.h3 [ style "user-select" "none" ] [text section.name]
                        ,MDList.list (MDList.config
                                     |> MDList.setTwoLine True
                                     |> MDList.setAvatarList True)
                             (moduleView x)
                             (List.map (\mod ->  moduleView mod) xs)
                        ]
    in
        LayoutGrid.cell [ Elevation.z2, LayoutGrid.span4 ] [ content ]

moduleView : Module -> MDListItem.ListItem Msg
moduleView mod =
    MDListItem.listItem MDListItem.config
        [
         MDListItem.graphic [] [ Html.img [ src mod.modicon  ] [] ] 
        ,MDListItem.text [style "user-select" "none"]
              { primary = [ text mod.name ]
              , secondary = [ text mod.modplural ]
            }            
        ]
    
-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = rootView
    }
 
