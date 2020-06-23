module Rest exposing (..)

import Http
import Json.Decode exposing (Decoder, field, string)
import Types exposing (..)


request : Cmd Msg
request =         
          Http.request
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

schoolDecoder : Decoder School
schoolDecoder =
    Json.Decode.map
        School
        (field "sitename" string)
              
