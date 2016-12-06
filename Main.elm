import List exposing (map, filter, head)
import Html exposing (program)
import Maybe exposing (andThen)
import Maybe.Extra exposing ((?))
import Http
import Platform.Cmd
import Json.Decode as J
import Json.Decode.Extra as JE exposing ((|:))
import Time exposing (every, second)

import Empire.Model exposing (Msg(..), Model, Project, Branch, MR, Status(..))
import Empire.View exposing (view)


status_map : String -> Status
status_map str =
  if str == "success" then Pass
  else if str == "running" then Running
  else Warn

mr_status_map : String -> Status
mr_status_map str =
  if str == "can_be_merged" then Pass
  else if str == "unchecked" then Running
  else Warn

mk_url path token = "https://gitlab.com/api/v3" ++ path ++ "private_token=" ++ token

get_pipelines token project_id = Http.send (PipelinesResponse project_id) <|
  Http.get (mk_url ("/projects/" ++ (toString project_id) ++ "/pipelines?") token) d_pipelines

d_pipelines = J.list <| J.map2 (\status ref -> { status = status, ref = ref })
  (J.field "status" J.string)
  (J.field "ref" J.string)

get_mrs token project_id = Http.send (MRsResponse project_id) <|
  Http.get (mk_url ("/projects/" ++ (toString project_id) ++ "/merge_requests?state=opened&") token) d_mrs

d_mrs = J.list <| J.map5 MR
  (J.field "iid" J.int)
  (J.field "title" J.string)
  (J.field "source_branch" J.string)
  (J.succeed 0)
  (J.map (mr_status_map >> Just) <| J.field "merge_status" J.string)

get_branches token project_id = Http.send (BranchesResponse project_id) <|
  Http.get (mk_url ("/projects/" ++ (toString project_id) ++ "/repository/branches?") token) d_branches

d_branches = J.list <| J.map6 Branch
  (J.field "name" J.string)
  (J.succeed 0)
  (J.succeed 0)
  (J.succeed 0)
  (J.succeed Nothing)
  (J.succeed Nothing)

get_projects token = Http.send ProjectsResponse <|
  Http.get (mk_url "/projects?" token) d_projects


d_projects = J.list <| J.succeed Project
  |: (J.field "id" J.int)
  |: (J.at [ "namespace", "name" ] J.string)
  |: (J.field "name" J.string)
  |: (J.field "description" J.string)
  |: (J.field "avatar_url" J.string |> JE.withDefault "")
  |: (J.field "open_issues_count" J.int)
  |: (J.succeed 0)
  |: (J.succeed Nothing)
  |: (J.succeed [])


update_project projects project =
  let mp = projects |> filter (.id >> ((==) project.id)) |> head
  in { project | branches = (mp |> Maybe.map .branches) ? []
               , status = mp |> andThen .status }
update_branches project_id branches project =
  if project.id == project_id
  then { project | branches = branches |> filter (.name >> ((/=) "master")) |> map (\ b ->
    let mmr = project.branches |> filter (.name >> ((==) b.name)) |> head
    in { b | mr = mmr |> andThen .mr
           , status = mmr |> andThen .status })}
  else project

update_mrs project_id mrs project =
  if project.id == project_id
  then { project | branches = project.branches |> map (\b ->
    { b | mr = mrs |> filter (.source_branch >> ((==) b.name)) |> head }) }
  else project

update_pipelines project_id pipelines project =
  if project.id /= project_id then project else { project
    | branches = project.branches |> map (\b ->
      { b | status = (pipelines |> filter (.ref >> ((==) b.name)) |> head
                              |> Maybe.map (.status >> status_map)) })
    , status = (pipelines |> filter (.ref >> ((==) "master")) |> head
                          |> Maybe.map (.status >> status_map)) }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Tick t -> ( model, if model.config_visible || String.isEmpty model.token
                     then Cmd.none
                     else get_projects model.token)
  ProjectsResponse (Ok projects) ->
    ( { model | projects = map (update_project model.projects) projects
              , error = Nothing }
    , projects |> map (.id >> get_branches model.token) |> Cmd.batch )
  ProjectsResponse (Err err) ->
    ( { model | error = Just <| toString err }
    , Cmd.none)
  BranchesResponse project_id (Ok branches) ->
    ( { model | projects = map (update_branches project_id branches) model.projects}
    , get_mrs model.token project_id )
  BranchesResponse project_id (Err err) -> (model, Cmd.none)
  MRsResponse project_id (Ok mrs) ->
    ( { model | projects = map (update_mrs project_id mrs) model.projects }
    , get_pipelines model.token project_id)
  MRsResponse project_id (Err err) -> (model, Cmd.none)
  PipelinesResponse project_id (Ok pipelines) ->
    ( { model | projects = map (update_pipelines project_id pipelines) model.projects }
    , Cmd.none)
  PipelinesResponse project_id (Err err) -> (model, Cmd.none)
  ToggleConfig ->
    ( { model | config_visible = not model.config_visible }
    , if model.config_visible then get_projects model.token else Cmd.none)
  ChangeToken token -> ({ model | token = token }, Cmd.none)

init : (Model, Cmd Msg)
init = ( { projects = []
         , error = Nothing
         , config_visible = True
         , token = "" }
       , Cmd.none )

main = program
  { init = init
  , update = update
  , view = view
  , subscriptions = \_ -> every (60 * second) Tick }
