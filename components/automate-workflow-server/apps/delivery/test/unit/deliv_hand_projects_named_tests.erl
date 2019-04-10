-module(deliv_hand_projects_named_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

-record(project_named, {
          project :: d_project(),
          ent_name :: binary(),
          org_name :: binary(),
          user_name :: binary(),
          state :: req_handler()
          }).

deliv_hand_projects_test_() ->
    [
        hoax:fixture(?MODULE, resource_exists),
        hoax:fixture(?MODULE, delete_resource),
        hoax:fixture(?MODULE, from_json),
        hoax:fixture(?MODULE, to_json)
    ].

content_types_provided_provides_json_test() ->
    hoax:test(fun() ->
        hoax:expect(receive
                        deliv_web_utils:content_type_json_or_any_map(to_json) -> expected_map
                    end),
        Actual =
            deliv_hand_projects_named:content_types_provided(req, #handler{}),

        ?assertEqual({expected_map, req, #handler{}}, Actual),
        ?verifyAll
    end).


content_types_accepted_accepts_json_test() ->
    hoax:test(fun() ->
        hoax:expect(receive
                        deliv_web_utils:content_type_json_map(from_json) -> expected_map
                    end),
        Actual =
            deliv_hand_projects_named:content_types_accepted(req, #handler{}),

        ?assertEqual({expected_map, req, #handler{}}, Actual),
        ?verifyAll
    end).

% UserName, EntName, OrgName, Proj, Req, State
resource_exists_returns_true_with_the_project_if_the_project_exists() ->
    EntName = <<"Hollywoo">>,
    OrgName = <<"Vigor">>,
    ProjName = <<"Secretariat">>,
    UserName = <<"Princess Carolyn">>,
    Project = project,
    State = #handler{ent_name = EntName, user_name = UserName},

    hoax:expect(receive
                    deliv_web_utils:extract_bindings([org_name, proj_name], req) -> {[OrgName, ProjName], req1};
                    deliv_project:fetch(EntName, OrgName, ProjName) -> {ok, Project}
                end),

    ProjectNamed = #project_named{project = Project,
                   ent_name = EntName,
                   org_name = OrgName,
                   user_name = UserName,
                   state = State},

    ?assertEqual({true, req1, ProjectNamed}, deliv_hand_projects_named:resource_exists(req, State)),
    ?verifyAll.

resource_exists_returns_false_if_error() ->
    EntName = <<"Hollywoo">>,
    OrgName = <<"Vigor">>,
    ProjName = <<"Secretariat">>,
    State = #handler{ent_name = EntName},
    Why = bojack_being_difficult_again,

    hoax:expect(receive
                    deliv_web_utils:extract_bindings([org_name, proj_name], req) -> {[OrgName, ProjName], req1};
                    deliv_project:fetch(EntName, OrgName, ProjName) -> {error, Why}
                end),

    ?assertEqual({false, req1, State}, deliv_hand_projects_named:resource_exists(req, State)),
    ?verifyAll.

delete_resource_returns_true_if_project_successfully_deletes() ->
    Project = project,
    ProjectNamed = #project_named{project = Project,
                                  state = state},

    hoax:expect(receive
                    deliv_project:delete(Project) -> ok
                end),

    Result = deliv_hand_projects_named:delete_resource(req, ProjectNamed),
    ?assertEqual({true, req, state}, Result),
    ?verifyAll.

delete_resource_returns_error_if_project_successfully_deletes() ->
    Project = project,
    ProjectNamed = #project_named{project = Project,
                                  state = state},

    hoax:expect(receive
                    deliv_project:delete(Project) -> {error, nononono};
                    deliv_web_utils:error_response(500, internal_server_error, req, state) -> return
                end),

    Result = deliv_hand_projects_named:delete_resource(req, ProjectNamed),
    ?assertEqual(return, Result),
    ?verifyAll.

to_json_returns_200_when_fetched_project_is_local() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    ProjId = 1,
    ScmModule = <<"deliv_scm_local">>,
    ScmType = <<"local">>,
    UserName = <<"username">>,
    PipeName = <<"pipelinename">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,

    ProjectNamed = #project_named{project = Proj,
                   ent_name = EntName,
                   org_name = OrgName,
                   user_name = UserName,
                   state = State},

    ProjectJson = json,

    hoax:expect(receive
                    deliv_project_json:create_body(EntName, OrgName, ProjName,
                                                   ScmType, UserName, PipeName) -> ProjectJson;
                    deliv_pipeline:fetch_names(EntName, OrgName, ProjName) -> PipeName;
                    deliv_web_utils:content(ProjectJson, request, State) -> ok
                end),

    ?assertEqual(ok, deliv_hand_projects_named:to_json(request, ProjectNamed)),
    ?verifyAll.

to_json_returns_200_when_fetched_project_is_githubv2() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    ProjId = 1,
    ScmModule = <<"github_scm">>,
    ScmType = <<"githubV2">>,
    UserName = <<"username">>,
    PipeName = <<"pipelinename">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ScmUrl = <<"https://github.com">>,

    ProjectNamed = #project_named{project = Proj,
                   ent_name = EntName,
                   org_name = OrgName,
                   user_name = UserName,
                   state = State},

    ProjectJson = json,

    hoax:expect(receive
                    deliv_pipeline:fetch_names(EntName, OrgName, ProjName) -> PipeName;
                    scm_github_project_metadata:fetch_by_id(ProjId) -> {ok, metadata};
                    deliv_hand_projects:fetch_scm_url(EntName, ScmType, []) -> {ScmUrl, ignored};
                    deliv_project_json:create_body(EntName, OrgName, ProjName, ScmType,
                                                   metadata, UserName, PipeName, ScmUrl) -> ProjectJson;
                    deliv_web_utils:content(ProjectJson, request, State) -> ok
                end),

    ?assertEqual(ok, deliv_hand_projects_named:to_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_an_error_if_json_is_malformed() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    UserName = <<"username">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, <<"deliv_scm_github">>}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                   ent_name = EntName,
                   org_name = OrgName,
                   user_name = UserName,
                   state = State},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {{error, whyyy}, request1};
                    deliv_web_utils:error_response(400, malformed_json,  <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_an_error_if_project_is_currently_github() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    UserName = <<"username">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, <<"deliv_scm_github">>}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                   ent_name = EntName,
                   org_name = OrgName,
                   user_name = UserName,
                   state = State},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>,<<"DEMO">>},
                  {<<"repo_name">>,<<"demo">>}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_web_utils:error_response(501, not_implemented, <<"This is not yet supported.">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_an_error_if_there_are_open_changes_when_proj_is_local_going_to_bitbucket_scm() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, <<"deliv_scm_local">>}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                   ent_name = EntName,
                   org_name = OrgName,
                   user_name = UserName,
                   state = State},
    Params = {state, open},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>,<<"DEMO">>},
                  {<<"repo_name">>,<<"demo">>}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_web_utils:error_response(412, precondition_failed, <<"Open changes exist in the system. Please close them and try again.">>, request1, State) -> {halt, request2, State};
                    deliv_change:changes(EntName, OrgName, ProjName, [Params]) -> {ok, [change]}
                end),

    Result = deliv_hand_projects_named:from_json(request, ProjectNamed),

    ?assertEqual({halt, request2, State}, Result),
    ?verifyAll.

from_json_returns_an_error_if_there_are_open_changes_when_proj_is_local_going_to_githubV2_scm() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, <<"deliv_scm_local">>}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                   ent_name = EntName,
                   org_name = OrgName,
                   user_name = UserName,
                   state = State},
    Params = {state, open},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"github">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/github-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/scm/github/servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"repo_owner">>,<<"DEMO">>},
                  {<<"repo_name">>,<<"demo">>}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_web_utils:error_response(412, precondition_failed, <<"Open changes exist in the system. Please close them and try again.">>, request1, State) -> {halt, request2, State};
                    deliv_change:changes(EntName, OrgName, ProjName, [Params]) -> {ok, [change]}
                end),

    Result = deliv_hand_projects_named:from_json(request, ProjectNamed),

    ?assertEqual({halt, request2, State}, Result),
    ?verifyAll.

from_json_updates_project_if_there_are_no_open_changes_when_proj_is_local_going_to_bitbucket_scm() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = <<"ProjId">>,
    ScmModule = <<"deliv_scm_local">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Params = {state, open},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>,<<"DEMO">>},
                  {<<"repo_name">>,<<"demo">>}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_change:changes(EntName, OrgName, ProjName, [Params]) -> {ok, []};
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:convert_to_bb(Json, Proj) -> updated_proj
                end),

    ?assertEqual({true, request1, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_updates_project_if_there_are_no_open_changes_when_proj_is_local_going_to_githubV2_scm() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = <<"ProjId">>,
    ScmModule =  <<"deliv_scm_local">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    GithubRepoOwner = <<"DEMO">>,
    GithubRepoName = <<"demo">>,
    Params = {state, open},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"github">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/github-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/github-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"repo_name">>,GithubRepoName},
                  {<<"repo_owner">>,GithubRepoOwner}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_change:changes(EntName, OrgName, ProjName, [Params]) -> {ok, []};
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:convert_to_githubV2(GithubRepoOwner, GithubRepoName, Proj) -> updated_proj
                end),

    ?assertEqual({true, request1, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_500_if_project_does_not_update_local_going_to_bitbucket_scm() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = <<"ProjId">>,
    ScmModule = <<"deliv_scm_local">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Params = {state, open},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>,<<"DEMO">>},
                  {<<"repo_name">>,<<"demo">>}]}},
               {<<"name">>,<<"a_project">>}]},


        hoax:expect(receive
                        deliv_web_utils:parse_json_req(request) -> {Json, request1};
                        deliv_change:changes(EntName, OrgName, ProjName, [Params]) -> {ok, []};
                        deliv_project:getval(scm_module, Proj) -> ScmModule;
                        deliv_project:getval(name, Proj) -> ProjName;
                        deliv_project:convert_to_bb(Json, Proj) -> {error, why};
                        deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                    end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_500_if_error_fetching_changes_local_going_to_bitbucket_scm() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = <<"ProjId">>,
    ScmModule = <<"deliv_scm_local">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Params = {state, open},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>,<<"DEMO">>},
                  {<<"repo_name">>,<<"demo">>}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_change:changes(EntName, OrgName, ProjName, [Params]) -> {error, whoopsie};
                    deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_500_if_error_fetching_changes_local_going_to_githubv2_scm() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = <<"ProjId">>,
    ScmModule = <<"deliv_scm_local">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Params = {state, open},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"github">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/github-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/scm/github/servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"repo_owner">>,<<"DEMO">>},
                  {<<"repo_name">>,<<"demo">>}]}},
               {<<"name">>,<<"a_project">>}]},

     hoax:expect(receive
                     deliv_web_utils:parse_json_req(request) -> {Json, request1};
                     deliv_change:changes(EntName, OrgName, ProjName, [Params]) -> {error, whoopsie};
                     deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                 end),

     ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
     ?verifyAll.

from_json_returns_500_if_project_does_not_update_local_going_to_githubV2_scm() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = <<"ProjId">>,
    ScmModule = <<"deliv_scm_local">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    GitHubRepoOwner = <<"DEMO">>,
    GitHubRepoName = <<"demo">>,
    Params = {state, open},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"github">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/github-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/scm/server/github/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"repo_owner">>,<<"DEMO">>},
                  {<<"repo_name">>,<<"demo">>}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_change:changes(EntName, OrgName, ProjName, [Params]) -> {ok, []};
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:convert_to_githubV2(GitHubRepoOwner, GitHubRepoName, Proj) -> {error, whoops};
                    deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

%% TODO: apparently bitbucket to local doesn't check for open changes. maybe fix that
from_json_returns_204_if_project_successfully_converts_from_bitbucket_to_local() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    ScmModule = <<"bitbucket_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[
        {<<"name">>,<<"a_project">>},
        {<<"scm">>, {[
            {<<"type">>,<<"local">>}
        ]}}
    ]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_project:convert_to_local(Proj) -> updated_proj
                end),

    ?assertEqual({true, request1, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_500_if_project_fails_to_convert_from_bitbucket_to_local() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    ScmModule = <<"bitbucket_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[
        {<<"name">>,<<"a_project">>},
        {<<"scm">>, {[
            {<<"type">>,<<"local">>}
        ]}}
    ]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_project:convert_to_local(Proj) -> {error, whoops};
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_409_if_project_does_not_update_local_going_to_bitbucket_scm_because_of_db_conflict() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = <<"ProjId">>,
    ScmModule = <<"deliv_scm_local">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Params = {state, open},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>,<<"DEMO">>},
                  {<<"repo_name">>,<<"demo">>}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_change:changes(EntName, OrgName, ProjName, [Params]) -> {ok, []};
                    deliv_project:convert_to_bb(Json, Proj) -> {error, {conflict, why}};
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_web_utils:error_response(409, conflict, <<"This Bitbucket Project and Repo combination is already in use">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_is_successful_if_updating_from_local_to_local() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ScmModule = <<"deliv_scm_local">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"local">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>,<<"DEMO">>},
                  {<<"repo_name">>,<<"demo">>}]}},
               {<<"name">>,<<"a_project">>}]},


    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1}
                end),

    Result = deliv_hand_projects_named:from_json(request, ProjectNamed),

    ?assertEqual({true, request1, State}, Result),
    ?verifyAll.

from_json_is_successful_if_updating_from_bitbucket_to_bitbucket_and_no_errors() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    BBProj = <<"DEMO">>,
    BBRepo = <<"demo">>,
    ScmModule = <<"bitbucket_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>, BBProj},
                  {<<"repo_name">>, BBRepo}]}},
               {<<"name">>,<<"a_project">>}]},

        hoax:expect(receive
                        deliv_web_utils:parse_json_req(request) -> {Json, request1};
                        deliv_project:getval(scm_module, Proj) -> ScmModule;
                        deliv_project:getval(id, Proj) -> ProjId;
                        deliv_project:getval(name, Proj) -> ProjName;
                        deliv_project:metadata_module(Proj) -> scm_bitbucket_project_metadata;
                        scm_bitbucket_project_metadata:update_by_id(ProjId, BBProj, BBRepo) -> {ok, metadata}
                    end),

        ?assertEqual({true, request1, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
        ?verifyAll.

from_json_is_successful_if_updating_from_githubV2_to_githubV2_and_no_errors() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    GitHubRepoOwner = <<"DEMO">>,
    GitHubRepoName = <<"demo">>,
    ScmModule = <<"github_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"github">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/github/projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/github/servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"repo_owner">>, GitHubRepoOwner},
                  {<<"repo_name">>, GitHubRepoName}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_project:getval(id, Proj) -> ProjId;
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:metadata_module(Proj) -> scm_github_project_metadata;
                    scm_github_project_metadata:update_by_id(ProjId, GitHubRepoOwner, GitHubRepoName) -> {ok, metadata}
                end),

    ?assertEqual({true, request1, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_404_if_updating_from_bitbucket_to_bitbucket_and_record_is_not_found() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    BBProj = <<"DEMO">>,
    BBRepo = <<"demo">>,
    ScmModule = <<"bitbucket_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>, BBProj},
                  {<<"repo_name">>, BBRepo}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_project:getval(id, Proj) -> ProjId;
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:metadata_module(Proj) -> scm_bitbucket_project_metadata;
                    scm_bitbucket_project_metadata:update_by_id(ProjId, BBProj, BBRepo) -> {error, not_found};
                    deliv_web_utils:error_response(404, not_found, <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_404_if_updating_from_githubV2_to_githubV2_and_record_is_not_found() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    GitHubRepoOwner = <<"DEMO">>,
    GitHubRepoName = <<"demo">>,
    ScmModule = <<"github_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"github">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/github/projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/github/servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"repo_owner">>, GitHubRepoOwner},
                  {<<"repo_name">>, GitHubRepoName}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_project:getval(id, Proj) -> ProjId;
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:metadata_module(Proj) -> scm_github_project_metadata;
                    scm_github_project_metadata:update_by_id(ProjId, GitHubRepoOwner, GitHubRepoName) -> {error, not_found};
                    deliv_web_utils:error_response(404, not_found, <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_409_conflict_if_updating_from_bitbucket_to_bitbucket_and_bbrepo_has_already_been_linked_to_another_deliv_project() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    BBProj = <<"DEMO">>,
    BBRepo = <<"demo">>,
    ScmModule = <<"bitbucket_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>, BBProj},
                  {<<"repo_name">>, BBRepo}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_project:getval(id, Proj) -> ProjId;
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:metadata_module(Proj) -> scm_bitbucket_project_metadata;
                    scm_bitbucket_project_metadata:update_by_id(ProjId, BBProj, BBRepo) -> {error, {conflict, db_conflict}};
                    deliv_web_utils:error_response(409, conflict, <<"This Bitbucket Project and Repo combination is already in use">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

frmo_json_returns_409_conflict_if_updating_from_githubV2_to_githubV2_and_ghrepo_has_already_been_linked_to_another_deliv_project() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    GitHubRepoOwner = <<"DEMO">>,
    GitHubRepoName = <<"demo">>,
    ScmModule = <<"github_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"github">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/github/projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/github/servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"repo_owner">>, GitHubRepoOwner},
                  {<<"repo_name">>, GitHubRepoName}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_project:getval(id, Proj) -> ProjId;
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:metadata_module(Proj) -> scm_github_project_metadata;
                    scm_github_project_metadata:update_by_id(ProjId, GitHubRepoOwner, GitHubRepoName) -> {error, {conflict, db_conflict}};
                    deliv_web_utils:error_response(409, conflict, <<"This Bitbucket Project and Repo combination is already in use">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_500_if_updating_from_bitbucket_to_bitbucket_and_record_is_not_found() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    BBProj = <<"DEMO">>,
    BBRepo = <<"demo">>,
    ScmModule = <<"bitbucket_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>, BBProj},
                  {<<"repo_name">>, BBRepo}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_project:getval(id, Proj) -> ProjId;
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:metadata_module(Proj) -> scm_bitbucket_project_metadata;
                    scm_bitbucket_project_metadata:update_by_id(ProjId, BBProj, BBRepo) -> {error, database_go_boom};
                    deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_500_if_updating_from_githubV2_to_githubV2_and_database_record_is_not_found() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    GitHubRepoOwner = <<"DEMO">>,
    GitHubRepoName = <<"demo">>,
    ScmModule = <<"github_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"github">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/github/projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/github/servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"repo_owner">>, GitHubRepoOwner},
                  {<<"repo_name">>, GitHubRepoName}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_project:getval(scm_module, Proj) -> ScmModule;
                    deliv_project:getval(id, Proj) -> ProjId;
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:metadata_module(Proj) -> scm_github_project_metadata;
                    scm_github_project_metadata:update_by_id(ProjId, GitHubRepoOwner, GitHubRepoName) -> {error, database_go_boom};
                    deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_a_400_error_if_no_type_in_json() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, <<"deliv_scm_local">>}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[{<<"scm">>,
                {[{<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>,<<"DEMO">>},
                  {<<"repo_name">>,<<"demo">>}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_web_utils:error_response(400, malformed_json, <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                end),

    ?assertEqual({halt, request2, State}, deliv_hand_projects_named:from_json(request, ProjectNamed)),
    ?verifyAll.

from_json_returns_500_error_when_githubV2_to_local_transition_errors_out_on_update() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    ScmModule = <<"github_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[
        {<<"name">>,<<"a_project">>},
        {<<"scm">>, {[
            {<<"type">>,<<"local">>}
        ]}}
    ]},

    hoax:expect(receive
                    deliv_project:getval(scm_module, Proj) -> <<"github_scm">>;
                    deliv_project:getval(name, Proj) -> ProjName;
                    deliv_project:convert_ghv2_to_local(Proj) -> {error, why};
                    chef_log:error("Could not convert project ~p from GitHub scm to local because ~p", [ProjName, why]) -> ignored;
                    deliv_web_utils:parse_json_req(request) -> {Json, request1};
                    deliv_web_utils:error_response(500, internal_server_error, <<"Project update failed.">>, request1, State) -> {halt, request2, State}
                end),

    Result = deliv_hand_projects_named:from_json(request, ProjectNamed),

    ?assertEqual({halt, request2, State}, Result),
    ?verifyAll.

from_json_returns_204_if_project_successfully_converts_from_githubV2_to_local() ->
    UserName =  <<"User">>,
    EntName = <<"Enterprise Name">>,
    OrgName = <<"Org Name">>,
    ProjName = <<"Project Name">>,
    ProjId = 1,
    ScmModule = <<"github_scm">>,
    Proj = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}, {id, ProjId}]),
    State = state,
    ProjectNamed = #project_named{project = Proj,
                           ent_name = EntName,
                           org_name = OrgName,
                           user_name = UserName,
                           state = State},
    Json = {[
        {<<"name">>,<<"a_project">>},
        {<<"scm">>, {[
            {<<"type">>,<<"local">>}
        ]}}
    ]},

    hoax:expect(receive
                    deliv_project:getval(scm_module, Proj) -> <<"github_scm">>;
                    deliv_project:convert_ghv2_to_local(Proj) -> Proj;
                    deliv_web_utils:parse_json_req(request) -> {Json, request1}
                end),

    Result = deliv_hand_projects_named:from_json(request, ProjectNamed),

    ?assertEqual({true, request1, State}, Result),
    ?verifyAll.
