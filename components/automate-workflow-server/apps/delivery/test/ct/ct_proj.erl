%% @doc Helpers for interacting with Chef Delivery Git repositories in
%% a Common Test environment
-module(ct_proj).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("delivery/include/deliv_types.hrl").

-export([
         create_pipeline/2,
         create_pipeline/3,
         create_pipeline/5,
         new_project_and_git_repo_with_pipeline/1,
         clone_empty_project/1,
         new_project_and_git_repo/1,
         add_delivery_config_file/1
        ]).

%% @doc Create a pipeline via the API.  Enterprise, Organization, and
%% Project name, as well as AuthData, are all taken from the `Config'
%% object. `PipeInfo' is either a tuple containing the pipeline name
%% and base, or a bare binary for the pipeline name.
create_pipeline(Config, PipeInfo) ->
    AuthData = ?config(auth_data, Config),
    create_pipeline(Config, PipeInfo, AuthData).

%% @doc Refinement of `create_pipeline/2', which allows you to
%% specifically set the authentication information for the request.
create_pipeline(Config, PipeInfo, AuthData) ->
    EntName  = ?config(ent_name, Config),
    OrgName  = ?config(org_name, Config),
    ProjName = ?config(proj_name, Config),
    create_pipeline(EntName, OrgName, ProjName, PipeInfo, AuthData).

%% @doc Refinement of `create_pipeline/3', which allows you to fully
%% specify all information for the request.
create_pipeline(EntName, OrgName, ProjName, PipeInfo, AuthData) ->
    Route = http_test_helpers:base_ent_route(EntName) ++
        "/orgs/" ++ chef_utils:to_str(OrgName) ++
        "/projects/" ++ chef_utils:to_str(ProjName) ++
        "/pipelines",

    {201, _Headers, _Body} = http_test_helpers:auth_req(AuthData, post,
                                                        Route,
                                                        case PipeInfo of
                                                            {PipeName, Base} ->
                                                                {[{<<"name">>, PipeName},
                                                                  {<<"base">>, Base}]};
                                                            PipeName ->
                                                                {[{<<"name">>, PipeName}]}
                                                        end),
    ok.

%% doc Create a new project (Ent and Org should already be created) and
%% create a pipeline, which creates a corresponding branch
new_project_and_git_repo_with_pipeline(Config0) ->
    Config = new_project_and_git_repo(Config0),

    %% Add a pipeline via direct DB interaction; we'll be testing the
    %% API elsewhere
    EntName  = ct_utils:get_config(ent_name, Config),
    OrgName  = ct_utils:get_config(org_name, Config),
    ProjName = ct_utils:get_config(proj_name, Config),
    PipeName = ct_utils:get_config(pipe_name, Config),

    [_Pipe]  = deliv_pipeline:insert(EntName, OrgName, ProjName, PipeName),

    Config.

%% @doc Create a brand new project (granting the current user rights
%% on it) checkout its git repo, and push a dummy commit upstream.
%%
%% TODO: This assumes that `ct_utils:get_config(roles, Config)' returns compatible
%% role(s) for checking out and pushing to a repository. I *think*
%% this should be OK given our test structure. We'll see. Might be
%% better to grant admin rights, and then revoke them after the
%% push. Alternatively, add a "superuser" do the Config for this kind
%% of setup, so things are completely independent.
new_project_and_git_repo(Config0) ->
    Config = new_project(Config0),

    GitSsh = ct_utils:get_config(git_ssh, Config),

    ct_authz:assign_roles(ct_utils:get_config(scope, Config),
                          ct_utils:get_config(roles, Config),
                          Config),

    CheckoutDir = clone_empty_project(Config),

    PipeName = ct_utils:get_config(pipe_name, Config),
    ct_git:push_dummy_commit(CheckoutDir, GitSsh, PipeName),

    [{repo_dir, CheckoutDir} | Config].

%% @doc Clones a project (assuming it's got no commits yet). Returns
%% the path to the new checkout in the testcase's priv_dir.
%%
%% (Note that our CT run is configured to automatically create a
%% separate priv_dir for each testcase run, so there is no danger of
%% conflicting with checkouts from other runs. This also means we
%% don't need to worry about cleaning up after ourselves!)
clone_empty_project(Config) ->
    ct:pal("Config is ~p", [Config]),

    EntName  = ct_utils:get_config(ent_name, Config),
    UserName = ct_utils:get_config(user_name, Config),
    OrgName  = ct_utils:get_config(org_name, Config),
    ProjName = ct_utils:get_config(proj_name, Config),
    GitSsh   = ct_utils:get_config(git_ssh, Config),

    %% TODO: deliv_git:uri/4 relies on hardcoding / app environment
    %% for host and port. We'll need to ensure these are set
    %% appropriately in our SUT... should be fine for now, though.
    SshUrl = deliv_git:uri(UserName, EntName, OrgName, ProjName),
    ct:pal("SshUrl is ~p", [SshUrl]),

    CloneDir = ct_git:clone_empty(SshUrl, GitSsh, Config),
    ct:pal("CloneDir is ~p", [CloneDir]),
    CloneDir.

%% @doc Create a new project, based on existing `Config'
%% values. Modifies `Config' and returns the updated value.
new_project(Config) ->
    Prefix  = ct_utils:get_config(prefix, Config),
    EntName = ct_utils:get_config(ent_name, Config),
    OrgName = ct_utils:get_config(org_name, Config),

    {ProjName, _} = db_test_helpers:new_project(<<Prefix/binary, "-project-">>,
                                                EntName,
                                                OrgName),
    [{proj_name, ProjName} | Config].

%% @doc Creates a dummy .delivery/config.json in the project
%% (needed to trigger stages on a project)
-spec add_delivery_config_file(_CtConfig) -> binary().
add_delivery_config_file(Config) ->
    RepoDir  = ct_utils:get_config(repo_dir, Config),
    GitSsh   = ct_utils:get_config(git_ssh, Config),
    PipeName = ct_utils:get_config(pipe_name, Config),

    CommonDataDir = common_data_test_helpers:common_data_dir_path(),
    DummyConfigFilePath = filename:join(CommonDataDir,
                                        "dummy_delivery_config.json"),
    {ok, DummyConfigFileContent} = file:read_file(DummyConfigFilePath),

    ct_git:push_dummy_commit(RepoDir, GitSsh, PipeName,
                             ".delivery/config.json",
                             DummyConfigFileContent).
