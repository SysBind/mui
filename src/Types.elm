module Types exposing (..)

import Http

type alias School = { sitename: String }
    
type Model = Failure String
           | Loading
           | Success School

type Msg
  = GotSchool (Result Http.Error School)
             
