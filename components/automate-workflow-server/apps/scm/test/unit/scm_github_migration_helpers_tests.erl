-module(scm_github_migration_helpers_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

scm_github_migration_helpers_test_() ->
    [
     hoax:fixture(?MODULE, 'validate_input_'),
     hoax:fixture(?MODULE, 'validate_project_scm_module_'),
     hoax:fixture(?MODULE, 'validate_no_open_changes_'),
     hoax:fixture(?MODULE, 'validate_no_local_git_repo_'),
     hoax:fixture(?MODULE, 'validate_github_scm_configured_'),
     hoax:fixture(?MODULE, 'validate_automate_'),
     hoax:fixture(?MODULE, 'list_project_webhooks_'),
     hoax:fixture(?MODULE, 'delete_webhooks_'),
     hoax:fixture(?MODULE, 'set_scm_module_to_'),
     hoax:fixture(?MODULE, 'initialize_local_git_'),
     hoax:fixture(?MODULE, 'list_ghv1_'),
     hoax:fixture(?MODULE, 'list_orgs_')
    ].

validate_input_when_project_exists_returns_true() ->
   Proj = project_record,

   ?assertEqual(project_record, scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, Proj})).

validate_input_when_enterprise_doesnt_exist_throws_error() ->
   ?assertThrow({<<"Failed to lookup enterprise">>, reason}, scm_github_migration_helpers:validate_input({error, reason}, {error, org}, {error, proj})).

validate_input_when_organization_doesnt_exist_throws_error() ->
   ?assertThrow({<<"Failed to lookup organization">>, reason}, scm_github_migration_helpers:validate_input({ok, ent}, {error, reason}, {error, reason})).

validate_input_when_project_doesnt_exist_throws_error() ->
   ?assertThrow({<<"Failed to lookup project">>, reason}, scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {error, reason})).

validate_project_scm_module_when_deliv_scm_github_returns_true() ->
    DProj = deliv_project:fromlist([{scm_module, <<"deliv_scm_github">>}]),

    ?assertEqual(true, scm_github_migration_helpers:validate_project_scm_module(DProj)).

validate_project_scm_module_when_other_returns_false() ->
    DProj = deliv_project:fromlist([{scm_module, <<"bitbucket_scm">>}]),

    ?assertThrow({<<"The specified project is not integrated with GitHub">>, <<"bitbucket_scm">>}, scm_github_migration_helpers:validate_project_scm_module(DProj)).

validate_no_open_changes_when_no_open_changes_returns_true()->
    EntName = <<"ent">>,
    OrgName = <<"org">>,
    ProjName= <<"proj">>,
    Params  = [{state, open}],

    hoax:expect(receive
                  deliv_change:changes(EntName, OrgName, ProjName, Params) -> {ok, []}
                end),

    ?assertEqual(true, scm_github_migration_helpers:validate_no_open_changes(EntName, OrgName, ProjName)),
    ?verifyAll.

validate_no_open_changes_when_open_changes_returns_false_throws_error()->
    EntName = <<"ent">>,
    OrgName = <<"org">>,
    ProjName= <<"proj">>,
    Params  = [{state, open}],
    ExpectedError = <<"There are open changes associated with the project.\n",
                      "Please merge or delete the open changes and try again">>,

    hoax:expect(receive
                  deliv_change:changes(EntName, OrgName, ProjName, Params) -> {ok, [change1, change1, changen]}
                end),

    ?assertThrow(ExpectedError, scm_github_migration_helpers:validate_no_open_changes(EntName, OrgName, ProjName)),
    ?verifyAll.

validate_no_open_changes_when_errors_returns_error_message_throws_error()->
    EntName = <<"ent">>,
    OrgName = <<"org">>,
    ProjName= <<"proj">>,
    Params  = [{state, open}],

    hoax:expect(receive
                  deliv_change:changes(EntName, OrgName, ProjName, Params) -> {error, why}
                end),

    ?assertThrow(why, scm_github_migration_helpers:validate_no_open_changes(EntName, OrgName, ProjName)),
    ?verifyAll.

validate_github_scm_configured_if_configured_returns_true() ->
    EntName = <<"ent">>,
    ScmType = <<"github">>,

    hoax:expect(receive
                  scm_basic_auth:load_basic_auth_credentials(EntName, ScmType) -> {ok, auth}
                end),

    ?assertEqual(true, scm_github_migration_helpers:validate_github_scm_configured(EntName)),
    ?verifyAll.

validate_github_scm_configured_if_not_configured_returns_throws_error() ->
    EntName = <<"ent">>,
    ScmType = <<"github">>,
    ExpectedError = <<"SCM link has not been set up for the new version of GitHub integration">>,

    hoax:expect(receive
                  scm_basic_auth:load_basic_auth_credentials(EntName, ScmType) -> {error, not_found}
                end),

    ?assertThrow(ExpectedError, scm_github_migration_helpers:validate_github_scm_configured(EntName)),
    ?verifyAll.

validate_github_scm_configured_if_errors_throws_error() ->
    EntName = <<"ent">>,
    ScmType = <<"github">>,

    hoax:expect(receive
                     scm_basic_auth:load_basic_auth_credentials(EntName, ScmType) -> {error, why}
                end),

    ?assertThrow(why, scm_github_migration_helpers:validate_github_scm_configured(EntName)),
    ?verifyAll.

validate_automate_returns_true_if_user_does_not_have_push_permissions() ->
    EntName = <<"ent">>,
    OrgName = <<"org">>,
    ProjName = <<"proj">>,
    Coords = #proj_coordinates{ent_name=EntName, org_name=OrgName, proj_name=ProjName},
    Scope = scope,
    ExpectedResponse = true,
    Ejson = {[
              {<<"permissions">>, {[
                 { <<"push">>, true }
               ]}
             }
            ]},

    hoax:expect(receive
                    deliv_scopes:from_coords(Coords) -> Scope;
                    deliv_github_client:req(Scope, get, []) -> {ok, 200, head, body};
                    chef_json:decode(body) -> Ejson
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration_helpers:validate_automate_has_push_permissions(Coords)),
    ?verifyAll.

validate_automate_returns_false_if_user_does_not_have_push_permissions() ->
    EntName = <<"ent">>,
    OrgName = <<"org">>,
    ProjName = <<"proj">>,
    Coords = #proj_coordinates{ent_name=EntName, org_name=OrgName, proj_name=ProjName},
    Scope = scope,
    ExpectedResponse = <<"Please ensure that automate service account in GitHub has write permissions to repositories">>,
    Ejson = {[
              {<<"permissions">>, {[
                 { <<"push">>, false }
               ]}
             }
            ]},

    hoax:expect(receive
                    deliv_scopes:from_coords(Coords) -> Scope;
                    deliv_github_client:req(Scope, get, []) -> {ok, 200, head, body};
                    chef_json:decode(body) -> Ejson
                end),

    ?assertThrow(ExpectedResponse, scm_github_migration_helpers:validate_automate_has_push_permissions(Coords)),
    ?verifyAll.

validate_automate_returns_error_for_any_response_code_other_than_200() ->
    EntName = <<"ent">>,
    OrgName = <<"org">>,
    ProjName = <<"proj">>,
    Coords = #proj_coordinates{ent_name=EntName, org_name=OrgName, proj_name=ProjName},
    Scope = scope,
    ExpectedResponse = <<"Could not read repository info from GitHub">>,

    hoax:expect(receive
                    deliv_scopes:from_coords(Coords) -> Scope;
                    deliv_github_client:req(Scope, get, []) -> {ok, 400, head, body}
                end),

    ?assertThrow(ExpectedResponse, scm_github_migration_helpers:validate_automate_has_push_permissions(Coords)),
    ?verifyAll.

list_project_webhooks_returns_error_why_for_any_code_other_than_200() ->
    EntName = <<"ent">>,
    OrgName = <<"org">>,
    ProjName = <<"proj">>,
    Coords = #proj_coordinates{ent_name=EntName, org_name=OrgName, proj_name=ProjName},
    Scope = scope,

    hoax:expect(receive
                  deliv_scopes:from_coords(Coords) -> Scope;
                  deliv_github_client:req(Scope, get, ["/hooks"]) -> {error, responsebody}
                end),

    ?assertThrow(responsebody, scm_github_migration_helpers:list_project_webhooks(Coords)),
    ?verifyAll.

list_project_webhooks_returns_error_if_cannot_list_webhooks_for_a_given_project() ->
    EntName = <<"ent">>,
    OrgName = <<"org">>,
    ProjName = <<"proj">>,
    Coords = #proj_coordinates{ent_name=EntName, org_name=OrgName, proj_name=ProjName},
    Scope = scope,

    hoax:expect(receive
                    deliv_scopes:from_coords(Coords) -> Scope;
                    deliv_github_client:req(Scope, get, ["/hooks"]) -> {ok, 204, head, why}
                end),

    ?assertThrow(why, scm_github_migration_helpers:list_project_webhooks(Coords)),
    ?verifyAll.

list_project_webhooks_lists_all_webhooks_for_given_project() ->
    Coords = #proj_coordinates{},
    ExpectedResponse = [Id1, Id2] = [123, 456],
    DelivUrl = <<"http://someurl.com/github-webhook">>,
    ResponseBody = <<"response">>,
    ResponseEjson = [{[
          {<<"id">>, Id1},
          {<<"url">>, <<"http://github.com/owner/project/hooks/123">>},
          {<<"config">>, [
              {<<"url">>, DelivUrl}
          ]}
         ]},
         {[
          {<<"id">>, Id2},
          {<<"url">>, <<"http://github.com/owner/project/hooks/456">>},
          {<<"config">>, [
              {<<"url">>, DelivUrl}
          ]}
         ]},
         {[
          {<<"id">>, 789},
          {<<"url">>, <<"http://github.com/owner/project/hooks/789">>},
          {<<"config">>, [
              {<<"url">>, <<"http://uh-huh/wrong/again">>}
          ]}
         ]}],
    Response = {ok, 200, [], ResponseBody},
    Scope = scope,

    hoax:expect(receive
                    deliv_scopes:from_coords(Coords) -> Scope;
                    chef_json:decode(ResponseBody) -> ResponseEjson;
                    github_repo:webhook_callback_url(Coords) -> DelivUrl;
                    deliv_github_client:req(Scope, get, ["/hooks"]) -> Response
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration_helpers:list_project_webhooks(Coords)),
    ?verifyAll.

delete_webhooks_deletes_all_webhooks_in_list() ->
    EntName = <<"ent">>,
    OrgName = <<"org">>,
    ProjName = <<"proj">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    WebhookIds = [123, 456],
    Scope = scope,

    hoax:expect(receive
                    deliv_scopes:from_coords(Coords) -> Scope;
                    deliv_github_client:req(Scope, delete, ["/hooks/", "123"]) -> {ok, 204, headers, response};
                    deliv_github_client:req(Scope, delete, ["/hooks/", "456"]) -> {ok, 204, headers, response}
                end),

    ?assertEqual(ok, scm_github_migration_helpers:delete_webhooks(Coords, WebhookIds)),
    ?verifyAll.

set_scm_module_to_new_github_returns_ok_if_it_can_update_to_github_scm() ->
    ProjName = <<"proj_name">>,
    OrgId = <<"123">>,
    GHV1Project0 = deliv_project:'#new'(),
    GHV1Project1 = deliv_project:setvals([{name, ProjName},
                                          {organization_id, OrgId},
                                          {scm_module, <<"deliv_scm_github">>}], GHV1Project0),
    GHV2Project = deliv_project:setvals([{name, ProjName},
                                        {organization_id, OrgId},
                                        {scm_module, <<"github_scm">>}], GHV1Project0),

    hoax:expect(receive
                    deliv_project:setvals([{scm_module, <<"github_scm">>}], GHV1Project1) -> updatedproject;
                    deliv_project:update(updatedproject) -> {ok, GHV2Project}
                end),

    ?assertEqual(ok, scm_github_migration_helpers:set_scm_module_to_new_github(GHV1Project1)),
    ?verifyAll.

set_scm_module_to_new_github_returns_error_if_it_cannot_update_to_github_scm() ->
    ProjName = <<"proj_name">>,
    OrgId = <<"123">>,
    GHV1Project0 = deliv_project:'#new'(),
    GHV1Project1 = deliv_project:setvals([{name, ProjName},
                                          {organization_id, OrgId},
                                          {scm_module, <<"deliv_scm_github">>}], GHV1Project0),

    hoax:expect(receive
                    deliv_project:setvals([{scm_module, <<"github_scm">>}], GHV1Project1) -> updatedproject;
                    deliv_project:update(updatedproject) -> {error, why}
                end),

    ?assertThrow({<<"Could not update scm_module to github_scm">>, why}, scm_github_migration_helpers:set_scm_module_to_new_github(GHV1Project1)),
    ?verifyAll.

initialize_local_git_repo_initializes_local_git_repo() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    Coords = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj},
    RootUrl = <<"http://github.com/ent/org/proj">>,
    RepoPath = <<"/Users/prajakta/code">>,
    TrustedCertsFile = <<"filename">>,
    TemplateDir = <<"/template/directory">>,
    GitCmd = ["clone", "--bare",
              "--template=" ++ TemplateDir,
              RootUrl, "."],

    hoax:expect(receive
                    scm_repo:url(Coords) -> {ok, RootUrl};
                    delivery_app:get_env(trusted_certificates_file) -> TrustedCertsFile;
                    delivery_app:get_env(deliv_git_repo_template) -> TemplateDir;
                    deliv_project:repo_path(Ent, Org, Proj) -> {ok, RepoPath};
                    deliv_git:run_git(clone_github, RepoPath, GitCmd, [{"GIT_SSL_CAINFO", TrustedCertsFile}]) -> {ok, term}
                end),

    ?assertEqual(ok, scm_github_migration_helpers:initialize_local_git_repo(Coords)),
    ?verifyAll.

initialize_local_git_repo_throws_error_if_cannot_initialize_local_git_repo() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    Coords = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj},
    RootUrl = <<"http://github.com/ent/org/proj">>,
    RepoPath = <<"/Users/prajakta/code">>,
    TrustedCertsFile = <<"filename">>,
    TemplateDir = <<"/template/directory">>,
    GitCmd = ["clone", "--bare",
              "--template=" ++ TemplateDir,
              RootUrl, "."],

    hoax:expect(receive
                    scm_repo:url(Coords) -> {ok, RootUrl};
                    delivery_app:get_env(trusted_certificates_file) -> TrustedCertsFile;
                    delivery_app:get_env(deliv_git_repo_template) -> TemplateDir;
                    deliv_project:repo_path(Ent, Org, Proj) -> {ok, RepoPath};
                    deliv_git:run_git(clone_github, RepoPath, GitCmd, [{"GIT_SSL_CAINFO", TrustedCertsFile}]) -> {error, why}
                end),

    ?assertThrow(why, scm_github_migration_helpers:initialize_local_git_repo(Coords)),
    ?verifyAll.

list_ghv1_projects_lists_all_ghv1_projects_in_org() ->
    EntName = <<"ent_name">>,
    OrgName = <<"org_name">>,
    ExpectedResult = [<<"proj_11">>, <<"proj_13">>],

    hoax:expect(receive
                    deliv_project:projects(EntName, OrgName) -> [proj_rec11, proj_rec22, proj_rec13];
                    deliv_project:getval(scm_module, proj_rec11) -> <<"deliv_scm_github">>;
                    deliv_project:getval(scm_module, proj_rec22) -> <<"deliv_scm_bitbucket">>;
                    deliv_project:getval(scm_module, proj_rec13) -> <<"deliv_scm_github">>;
                    deliv_project:getval(name, proj_rec11) -> <<"proj_11">>;
                    deliv_project:getval(name, proj_rec13) -> <<"proj_13">>
                end),

    ?assertEqual(ExpectedResult, scm_github_migration_helpers:list_ghv1_projects(EntName, OrgName)),
    ?verifyAll.

list_orgs_lists_all_orgs_in_an_ent() ->
    EntName = <<"ent_name">>,
    EjOrgs = [{[{<<"name">>,<<"org1">>},{<<"project_count">>,1}]},
      {[{<<"name">>,<<"org2">>},{<<"project_count">>,4}]}],
    ExpectedResult = [<<"org1">>, <<"org2">>],

    hoax:expect(receive
                    deliv_organization:fetch_for_ent(EntName) -> EjOrgs
                end),

    ?assertEqual(ExpectedResult, scm_github_migration_helpers:list_orgs(EntName)),
    ?verifyAll.

list_orgs_throws_error_if_it_cannot_fetch_orgs() ->
    EntName = <<"ent_name">>,

    hoax:expect(receive
                    deliv_organization:fetch_for_ent(EntName) -> {error, why}
                end),

    ?assertThrow({<<"Failed to lookup organization for Enterprise">>, why}, scm_github_migration_helpers:list_orgs(EntName)),
    ?verifyAll.

validate_no_local_git_repo_succeeds_when_path_does_not_exist() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    Coords = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj},
    RepoPath = <<"/path/to/uuid/dir">>,

    hoax:expect(receive
                    deliv_project:repo_path(Ent, Org, Proj) -> {ok, RepoPath};
                    filelib:is_dir(RepoPath) -> false
                end),

    ?assertEqual(ok, scm_github_migration_helpers:validate_no_local_git_repo(Coords)),

    ?verifyAll.

validate_no_local_git_repo_throws_when_path_exists() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    Coords = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj},
    RepoPath = <<"/path/to/uuid/dir">>,

    hoax:expect(receive
                    deliv_project:repo_path(Ent, Org, Proj) -> {ok, RepoPath};
                    filelib:is_dir(RepoPath) -> true
                end),

    ExpectedError = <<"The local git repository path already exists. ",
                      "This could be due to an error in a previous migration attempt. ",
                      "The directory ", RepoPath/binary, " must be removed before rerunning the migration.">>,
    ?assertThrow(ExpectedError, scm_github_migration_helpers:validate_no_local_git_repo(Coords)),

    ?verifyAll.
