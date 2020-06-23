module Types exposing (..)

import Http

type alias SiteInfo = { sitename: String, username: String, fullname: String, userid: Int }
    
type Model = Failure String
           | Loading
           | Success SiteInfo

type Msg
  = GotSiteInfo (Result Http.Error SiteInfo)
             
