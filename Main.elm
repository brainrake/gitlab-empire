import List exposing (map, filter, head)
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


mk_url : String -> Token -> String
mk_url path token = "https://gitlab.com/api/v3" ++ path ++ "private_token=" ++ token

get : String -> (Result Http.Error a -> Msg) -> Decoder a -> Token -> Cmd Msg
get fragment constructor decoder token =
  Http.send constructor (Http.get (mk_url fragment token) decoder)

get_pipelines : Int -> Token -> Cmd Msg
get_pipelines project_id = get
  ("/projects/" ++ toString project_id ++ "/pipelines?")
  (PipelinesResponse project_id)
  (list <| succeed Pipeline
      |: field "id" int
      |: (field "status" string |> J.map status_map)
      |: field "ref" string )

get_mrs : Int -> Token -> Cmd Msg
get_mrs project_id = get
  ("/projects/" ++ (toString project_id) ++ "/merge_requests?state=opened&")
  (MRsResponse project_id)
  (list <| succeed MR
    |: field "iid" int
    |: field "title" string
    |: field "source_branch" string
    |: (field "merge_status" string |> J.map (mr_status_map >> Just) ))

get_branches : Int -> Token -> Cmd Msg
get_branches project_id = get
  ("/projects/" ++ toString project_id ++ "/repository/branches?")
  (BranchesResponse project_id)
  (list <| succeed Branch
    |: field "name" string
    |: succeed 0
    |: succeed 0
    |: succeed Nothing
    |: succeed Nothing)

get_projects : Token ->  Cmd Msg
get_projects = get "/projects?" ProjectsResponse
  (list <| succeed Project
    |: field "id" int
    |: at [ "namespace", "name" ] string
    |: field "name" string
    |: field "path" string
    |: field "description" string
    |: (field "avatar_url" string |> JE.withDefault "")
    |: field "open_issues_count" int
    |: succeed [])


update_project : List Project -> Project -> Project
update_project projects project =
  { project | branches = (projects |> filter (.id >> ((==) project.id))
                                   |> map .branches |> head) ? []}

update_branches : Int -> List Branch -> Project -> Project
update_branches project_id branches project =
  if project.id == project_id
  then { project | branches = branches |> map (\ b ->
    let mb = project.branches |> filter (.name >> ((==) b.name)) |> head
    in { b | mr = mb |> andThen .mr
           , pipeline = mb |> andThen .pipeline })}
  else project

update_mrs : Int -> List MR -> Project -> Project
update_mrs project_id mrs project =
  if project.id == project_id
  then { project | branches = project.branches |> map (\b ->
    { b | mr = mrs |> filter (.source_branch >> ((==) b.name)) |> head }) }
  else project

update_pipelines : Int -> List Pipeline -> Project -> Project
update_pipelines project_id pipelines project =
  if project.id /= project_id then project else { project
    | branches = project.branches |> map (\b ->
      { b | pipeline = pipelines |> filter (.ref >> ((==) b.name)) |> head } ) }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Tick t -> ( model, if model.config_visible || String.isEmpty model.token
                     then Cmd.none
                     else get_projects model.token)
  ProjectsResponse (Ok projects) ->
    ( { model | projects = map (update_project model.projects) projects
              , error = Nothing }
    , projects |> map (\p -> get_branches p.id model.token) |> Cmd.batch )
  ProjectsResponse (Err err) ->
    ( { model | error = Just <| toString err }
    , Cmd.none)
  BranchesResponse project_id (Ok branches) ->
    ( { model | projects = map (update_branches project_id branches) model.projects}
    , get_mrs project_id model.token )
  BranchesResponse project_id (Err err) -> (model, Cmd.none)
  MRsResponse project_id (Ok mrs) ->
    ( { model | projects = map (update_mrs project_id mrs) model.projects }
    , get_pipelines project_id model.token)
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
