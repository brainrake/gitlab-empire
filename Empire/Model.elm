module Empire.Model exposing (Msg(..), Model, Project, Branch, MR, Status(..), status_color)

import Http
import Time exposing (Time)

type Msg = Tick Time
         | ProjectsResponse (Result Http.Error (List Project))
         | BranchesResponse Int (Result Http.Error (List Branch))
         | MRsResponse Int (Result Http.Error (List MR))
         | PipelinesResponse Int (Result Http.Error
           (List { status : String, ref : String }))
         | ToggleConfig
         | ChangeToken String


type alias Model =
  { projects : List Project
  , error : Maybe String
  , config_visible : Bool
  , token : String }

type alias Project =
  { id : Int
  , org : String
  , name : String
  , description : String
  , avatar_url : String
  , open_issues_count : Int
  , pipeline_id : Int
  , status: Maybe Status
  , branches : List Branch }

type alias Branch =
  { name : String
  , plus : Int
  , minus : Int
  , pipeline_id : Int
  , status : Maybe Status
  , mr : Maybe MR }

type alias MR =
  { id : Int
  , title : String
  , source_branch : String
  , pipeline_id : Int
  , status : Maybe Status }

type Status = Warn | Running | Fail | Pass


status_color : Status -> String
status_color status = case status of
  Warn -> "yellow"
  Running -> "blue"
  Fail -> "red"
  Pass -> "green"
