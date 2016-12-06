module Empire.Model exposing (Msg(..), Model, Token, Project, Branch, Pipeline, MR, Status(..), status_color)

import Http
import Dict exposing (Dict)
import Time exposing (Time)

type Msg = Tick Time
         | ToggleConfig
         | ChangeToken String
         | ProjectsResponse (Result Http.Error (Dict Int Project))
         | BranchesResponse Int (Result Http.Error (Dict String Branch))
         | MRsResponse Int (Result Http.Error (List MR))
         | PipelinesResponse Int (Result Http.Error (List Pipeline))

type alias Token = String

type alias Model =
  { projects : Dict Int Project
  , error : Maybe String
  , config_visible : Bool
  , token : String }

type alias Project =
  { id : Int
  , org : String
  , name : String
  , path : String
  , description : String
  , avatar_url : String
  , open_issues_count : Int
  , branches : Dict String Branch }

type alias Branch =
  { name : String
  , plus : Int
  , minus : Int
  , pipeline : Maybe Pipeline
  , mr : Maybe MR }

type alias Pipeline =
  { id : Int
  , status : Status
  , ref : String }

type alias MR =
  { id : Int
  , title : String
  , source_branch : String
  , status : Maybe Status }

type Status = Warn | Running | Fail | Pass


status_color : Status -> String
status_color status = case status of
  Warn -> "yellow"
  Running -> "blue"
  Fail -> "red"
  Pass -> "green"
