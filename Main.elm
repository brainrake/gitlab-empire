import List exposing (map, filter, head, foldr)
import Dict exposing (Dict, empty, insert, values, get)
import Dict.Extra exposing (fromListBy)
import Html exposing (program)
import Maybe exposing (andThen)
import Maybe.Extra exposing ((?))
import Http
import Platform.Cmd
import Json.Decode as J exposing (Decoder, list, field, string, int, succeed, at)
import Json.Decode.Extra as JE exposing ((|:))
import Time exposing (every, second)

import Empire.Model exposing (Msg(..), Model, Token, Project, Branch, Pipeline, MR, Status(..))
import Empire.View exposing (view)


status_map str = case str of
  "success" -> Pass
  "running" -> Running
  "failed" -> Fail
  _ -> Warn

mr_status_map str = case str of
  "can_be_merged" -> Pass
  "unchecked" -> Running
  _ -> Warn


request : String -> (Result Http.Error a -> Msg) -> Decoder a -> Token -> Cmd Msg
request path constructor decoder token =
  let url = "https://gitlab.com/api/v3" ++ path ++ "private_token=" ++ token
  in Http.send constructor (Http.get url decoder)

get_projects : Token ->  Cmd Msg
get_projects = request "/projects?" ProjectsResponse
  (J.map (fromListBy .id) <| list <| succeed Project
    |: field "id" int
    |: at [ "namespace", "name" ] string
    |: field "name" string
    |: field "path" string
    |: (field "description" string |> JE.withDefault "")
    |: (field "avatar_url" string |> JE.withDefault "")
    |: field "open_issues_count" int
    |: succeed empty)

get_branches : Int -> Token -> Cmd Msg
get_branches project_id = request
  ("/projects/" ++ toString project_id ++ "/repository/branches?")
  (BranchesResponse project_id)
  (J.map (fromListBy .name) <| list <| succeed Branch
    |: field "name" string
    |: succeed 0
    |: succeed 0
    |: succeed Nothing
    |: succeed Nothing)

get_mrs : Int -> Token -> Cmd Msg
get_mrs project_id = request
  ("/projects/" ++ toString project_id ++ "/merge_requests?state=opened&")
  (MRsResponse project_id)
  (list <| succeed MR
    |: field "iid" int
    |: field "title" string
    |: field "source_branch" string
    |: (field "merge_status" string |> J.map (mr_status_map >> Just) ))

get_pipelines : Int -> Token -> Cmd Msg
get_pipelines project_id = request
  ("/projects/" ++ toString project_id ++ "/pipelines?")
  (PipelinesResponse project_id)
  (list <| succeed Pipeline
    |: field "id" int
    |: (field "status" string |> J.map status_map)
    |: field "ref" string )


update_project : Dict Int Project -> Int -> Project -> Project
update_project projects id project =
  { project | branches = (Dict.get id projects |> Maybe.map .branches) ? empty }

update_branches : Int -> Dict String Branch -> Int -> Project -> Project
update_branches project_id branches id project =
  if project.id /= project_id then project
    else { project | branches = branches |> Dict.map (\name branch ->
      let get_old = Dict.get name project.branches |> flip andThen
      in { branch | mr = get_old .mr
                  , pipeline = get_old .pipeline })}

update_mrs : List MR -> Int -> Project -> Project
update_mrs mrs id project =
  if project.id /= id then project
    else { project | branches = project.branches |> Dict.map (\name branch ->
      { branch | mr = mrs |> filter (.source_branch >> (==) name) |> head } ) }

update_pipelines : Int -> List Pipeline -> Int -> Project -> Project
update_pipelines project_id pipelines id project =
  if project.id /= project_id then project
    else { project | branches = project.branches |> Dict.map (\name branch ->
      { branch | pipeline = pipelines |> filter (.ref >> (==) name) |> head } ) }


error_message : String
error_message =
  "There was an error fetching the results. Please check the console for details."

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Tick t -> ( model, if model.config_visible || String.isEmpty model.token
                     then Cmd.none
                     else get_projects model.token)
  ToggleConfig ->
    ( { model | config_visible = not model.config_visible }
    , if model.config_visible then get_projects model.token else Cmd.none)
  ChangeToken token ->
    ( { model | token = token }, Cmd.none )
  ProjectsResponse (Err err) ->
    ( { model | error = (always <| Just error_message) (Debug.log "Error:" err) } , Cmd.none)
  ProjectsResponse (Ok projects) ->
    ( { model | projects = Dict.map (update_project model.projects) projects
              , error = Nothing }
    , projects |> values |> map (\p -> get_branches p.id model.token) |> Cmd.batch )
  BranchesResponse project_id (Ok branches) ->
    ( { model | projects = Dict.map (update_branches project_id branches) model.projects}
    , get_mrs project_id model.token )
  MRsResponse project_id (Ok mrs) ->
    ( { model | projects = Dict.map (update_mrs mrs) model.projects }
    , get_pipelines project_id model.token)
  PipelinesResponse project_id (Ok pipelines) ->
    let projects = Dict.map (update_pipelines project_id pipelines) model.projects
    in ( { model | projects = projects }, Cmd.none)
  _ -> (model, Cmd.none)


init : (Model, Cmd Msg)
init = ( { projects = empty
         , error = Nothing
         , config_visible = True
         , token = "" }
       , Cmd.none )


main = program
  { init = init
  , update = update
  , view = view
  , subscriptions = \_ -> every (60 * second) Tick }
