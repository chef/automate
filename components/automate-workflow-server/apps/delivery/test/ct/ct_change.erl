%% @doc Common utilities for testing changes. Woo.
-module(ct_change).

-include_lib("common_test/include/ct.hrl").

-include_lib("delivery/include/deliv_types.hrl").

%% Common setup and assertion functions
-export([
         assert_successful_get_request/2,
         assert_successful_get_request/3,
         generate_route/1,
         setup_repo/1,
         setup_repo/2
        ]).

%% Public ej:valid/2 matchers
-export([
         date_matcher/0,
         uuid_matcher/0
        ]).

%% Helpers
-export([
         extract_change_id/1
        ]).

%% @doc Generate a route to one of the change-related endpoints, based
%% on the contents of a CT config object.
-spec generate_route([{atom(), term()}]) -> string().
generate_route(Config) ->
    EntName  = ct_utils:get_config(ent_name, Config),
    OrgName  = ct_utils:get_config(org_name, Config),
    ProjName = ct_utils:get_config(proj_name, Config),

    URL = http_test_helpers:base_ent_route(EntName) ++
        "/orgs/" ++ chef_utils:to_str(OrgName) ++
        "/projects/" ++ chef_utils:to_str(ProjName) ++
        "/changes/",

    %% Using bare ?config macro since this key isn't required to be
    %% present
    case ?config(change_uuid, Config) of
        undefined ->
            URL;
        UUID ->
            URL ++ chef_utils:to_str(UUID)
    end.

%% @doc Shortcut to `setup_repo/2', which sets up no changes in the
%% project.
-spec setup_repo(Config :: [{atom(), term()}]) -> NewConfig :: [{atom(), term()}].
setup_repo(Config) ->
    setup_repo(Config, []).

%% @doc Creates a new project, checks out the repository, populates it
%% with some minimal commits, creates a pipeline for the "master"
%% branch, and then creates a series of changes.
%% See ct_git:change() type for more info
-spec setup_repo(Config :: [{atom(), term()}],
                 [ct_git:change()]) -> NewConfig :: [{atom(), term()}].
setup_repo(Config0, Changes) ->
    Config1 = ct_proj:new_project_and_git_repo_with_pipeline(Config0),

    RepoDir = ?config(repo_dir, Config1),
    GitSsh = ?config(git_ssh, Config1),
    PipeName = ?config(pipe_name, Config1),

    %% Add a file, so we can do deletions, should we so choose
    ct_git:create_commit(RepoDir, {<<"Make a readme">>,
                                   <<"add a file, woo">>,
                                   [{"readme.md", <<"'sup, peeps?">>}]}),
    ct_git:create_commit(RepoDir, {<<"Add delivery config">>,
                                   <<"Because we need it!">>,
                                   [{".delivery/config.json",
                                     chef_json:encode({[{<<"version">>, <<"1">>},
                                                         {<<"build_cookbook">>, <<"simple_build">>}]})}]}),
    ct_git:push_head_to(RepoDir, GitSsh, PipeName),

    ChangeIds = ct_git:create_changes(RepoDir, GitSsh, Changes),

    [{change_ids, ChangeIds} | Config1].

%% @doc Execute a request that should be successful, and compare the
%% response to an ej:valid/2 spec to validate structure. Returns the
%% Ejson-ified response body for further testcase-specific assertions
%% (e.g., body contains 2 changes, first change has 2 patchsets, etc.)
-spec assert_successful_get_request({ej, EjSpec :: term()} |
                                    {exact, Ejson :: ej:json_object() | ej:json_array()} |
                                    {jesse, JesseSpec :: term(), CTConfig :: [{atom(), term()}]},
                                    [{atom(), term()}]) ->
                                           ResponseEjson :: ej:json_object() | ej:json_array().
assert_successful_get_request(Match, Config) ->
    assert_successful_get_request(Match, Config,
                                  ct_change:generate_route(Config)).

%% @doc Same as `assert_succesful_get_requests/2`, but allows the
%% specifying of a route
-spec assert_successful_get_request({ej, EjSpec :: term()} |
                                    {exact, Ejson :: ej:json_object() | ej:json_array()} |
                                    {jesse, JesseSpec :: term(), CTConfig :: [{atom(), term()}]},
                                    [{atom(), term()}],
                                    binary()) ->
                                           ResponseEjson :: ej:json_object() | ej:json_array().
assert_successful_get_request(Match, Config, Route) ->
    ct:pal("Route is ~p", [Route]),
    AuthData = ct_utils:get_config(auth_data, Config),
    Response = http_test_helpers:auth_req(AuthData, get, Route),
    http_test_helpers:test_json_response({200,
                                          Match},
                                         Response),
    {_Status, _Headers, Body} = Response,
    ct:pal("Body is:~p", [Body]),
    chef_json:decode(Body).

%% Matchers
%%
%% We'll set up some functions to use with ej:valid/2 in order to
%% verify the basic structure of change objects. Due to current
%% limitations of ej, we can't yet use it to (easily) verify specific
%% content of each change or patchset, since they're all different.
%%
%% Instead we'll take the following approach. We'll have tests that
%% should retrieve a single change (with multiple patchsets) and
%% verify the content of that. Then, we'll test that the endpoint can
%% return multiple changes, each of which structurally correspond to a
%% "change". Since the sorting and filtering logic has been pretty
%% thoroughly exercised in our pgTAP tests, we'll assume here that our
%% multiple changes correspond to what they should properly
%% be. Additionally, we'll have some unit tests on the handler code
%% ensuring that query string parameters on an incoming request are
%% appropriately converted into the correct inputs to the underlying
%% database stored procedure.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc ej matcher to assert that a string matches our type 4 UUID
%% identifiers; that is:
%%
%%    XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX
%%
%% where "X" is a valid hexadecimal digit.
%%
%% Technically, I believe we only ever return lower-cased UUIDs,
%% though we can accept either lower or upper case without problem.
uuid_matcher() ->
    {ok, Regex} = re:compile("^[a-fA-F0-9]{8}(?:-[a-fA-F0-9]{4}){3}-[a-fA-F0-9]{12}$"),
    {string_match, {Regex, "Invalid Change Id"}}.

%% @doc ej matcher to assert that a string matches our timestamp
%% format.
%%
%% Currently, this is just "YYYY-MM-DD HH:MM:SS", with no indication
%% of time zone, which will need to be addressed at some point.
%%
%% Additionally, the regex here currently allows for weird dates like
%% "2014-92-73 53:63:99"... meh.
date_matcher() ->
    {ok, Regex} = re:compile("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"),
    {string_match, {Regex, "Invalid Date"}}.

%% @doc This extracts the change id as part of the uri returned
%% from creating a new review. This is hacky and maybe should
%% be replaced in the future
-spec extract_change_id(str_or_binary()) -> binary().
extract_change_id(Str) ->
    StrL = chef_utils:to_str(Str),
    Start = string:str(StrL, "changes/") + 8,
    erlang:list_to_binary(string:sub_string(StrL, Start, Start+35)).
