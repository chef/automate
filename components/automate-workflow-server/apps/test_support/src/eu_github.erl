%% @doc EUnit helper functions for concisely creating test data for
%% github-dependent tests.
%%
%% Example Usage:
%%
%% setup() ->
%%   eu_github:setup()
%%   eu_data:with_enterprise(<<"deliv_changeset_test_enterprise">>,
%%     eu_data:with_organization(<<"deliv_changeset_test_organization">>,
%%       eu_github:with_project(<<"deliv_changeset_test_project">>,
%%         eu_data:with_pipeline(<<"master">>,
%%           fun(Enterprise, Organization, Project, Pipeline) ->
%%             %% Assert something fun in here
%%             %% Or return data as part of a Hoax parameterized fixture
%%           end)))).
%%
%% teardown({Enterprise, Organization, Project, ...}) ->
%%     eu_github:teardown(Enterprise, Organization, Project),
%%     other_stuff.

-module(eu_github).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

-define(GITHUB_ROOT_URL, <<"https://github.chef.co">>).
-define(GITHUB_API_URL, <<"https://github.chef.co/api/v3">>).
%% This is a personal token associated with the delivery-team user on github.chef.co.
%% This token requires the following permissions: admin:public_key, admin:repo_hook,
%% delete_repo, repo, user
-define(GITHUB_TOKEN, <<"9eb5447039cc5560c0545e44b727b9d99e4d3e9b">>).
-define(GITHUB_REPO_OWNER, <<"automated-tests">>).

fetch_or_create_oauth_app() ->
    [Application] = case deliv_oauth_application:fetch(<<"github">>) of
                        {error, not_found} ->
                            deliv_oauth_application:insert(<<"github">>,
                                                           <<"deliv_github_oauth">>,
                                                           ?GITHUB_ROOT_URL,
                                                           ?GITHUB_API_URL,
                                                           <<"client_id">>,
                                                           <<"client_secret">>);
                        {ok, App} ->
                            [App]
                    end,
    Application.

fetch_or_create_oauth_token(Enterprise) ->
    EntId = deliv_enterprise:getval(id, Enterprise),
    [Token] = case deliv_oauth_token:fetch(enterprise, EntId, deliv_github_oauth) of
                  {error, not_found} ->
                      {ok, TokenRec} = deliv_oauth_token:initialize(<<"github">>, enterprise, EntId),
                      {ok, TokenRecWithToken} = deliv_oauth_token:save_token(?GITHUB_TOKEN, TokenRec),
                      [TokenRecWithToken];
                  {ok, TokenRec} ->
                      [TokenRec]
              end,
    Token.

with_enterprise(EntName, Fun) ->
    Enterprise = eu_data:fetch_or_create_enterprise(EntName),
    db_test_helpers:new_builder_user(EntName),
    fetch_or_create_oauth_token(Enterprise),
    Fun(Enterprise).

repo_name(ProjName) ->
    Pid = os:getpid(),
    chef_utils:to_bin([ProjName, Pid]).

create_github_repo(Coords, RepoName) ->
    Route = ["/repos"],
    ReqJson = {[{<<"name">>, RepoName}]},
    case deliv_github_client:org_req(Coords, post, Route, ReqJson) of
        {ok, 201, _, _} ->
            ok;
        {error, _Why} = Err ->
            Err
    end.

destroy_github_repo(Enterprise, Organization, Project) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    OrgName = deliv_organization:getval(name, Organization),
    ProjName = deliv_project:getval(name, Project),
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    case deliv_github_client:req(Coords, delete, "", <<>>, []) of
        {ok, 204, _, _} ->
            ok;
        {error, _Why} = Err ->
            Err
    end.

remove_key(Enterprise, KeyId) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    case deliv_github_client:user_req(EntName, delete, ["/keys/", chef_utils:to_str(KeyId)], <<>>) of
        {ok, 204, _, _} ->
            ok;
        {ok, _, _, _} ->
            {error, invalid_status};
        {error, _Why} = Err ->
            Err
    end.

applications() ->
    [ibrowse].

setup() ->
    ok = application:set_env(delivery, trusted_certificates_file, "/etc/ssl/certs/ca-certificates.crt"),
    [ application:start(A) || A <- applications() ],
    sqerl:execute(<<"TRUNCATE external_oauth_applications CASCADE">>),
    fetch_or_create_oauth_app().

teardown(Enterprise, Organization, Project) ->
    destroy_github_repo(Enterprise, Organization, Project),
    teardown().

teardown() ->
    [ application:stop(A) || A <- lists:reverse(applications()) ].
