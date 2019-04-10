%% doc This module is intended to be a wrapper around rest calls
%% we want to perform against the bitbucket instance.
-module(scm_bitbucket_rest_api).

-include_lib("delivery/include/deliv_coordinates.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-export([
         ensure_pull_request/4,
         check_reachability/3
        ]).

%% @doc This function attempts to create a pull request in Bitbucket idempotently.
%% If we get a 409 from bitbucket it means the pr already exists so we just return
%% the pr url as in the new case.
-spec ensure_pull_request(d_patchset(), binary(), binary(), #proj_coordinates{}) -> {ok, term()} | {exists, term()} | {error, term()}.
ensure_pull_request(Patchset, SourceBranch, DestinationBranch, Coords = #proj_coordinates{ent_name = EntName}) ->
    BasicAuth = scm_basic_auth:load_basic_auth_credentials(EntName, <<"bitbucket">>),
    Metadata = scm_bitbucket_project_metadata:fetch_by_project_coords(Coords),
    ensure_pull_request(
      repo_url(BasicAuth, Metadata),
      pull_request_body(Patchset, SourceBranch, DestinationBranch, Metadata),
      request_headers(BasicAuth)
    ).

%% private
-spec ensure_pull_request(list(), ej:json_object(), list()) -> {ok, term()} | {exists, term()} | {error, term()}.
ensure_pull_request(Url, RequestBody, Headers) ->
    case deliv_http:req(post, Url, RequestBody, Headers) of
        {ok, 201, _Head, ResponseBody} ->
            BodyEJson = chef_json:decode(ResponseBody),
            {ok, BodyEJson};
        {ok, 409, _Head, ResponseBody} ->
            BodyEJson = chef_json:decode(ResponseBody),
            PRUrl = ej:get([<<"errors">>,
                    {select, {<<"exceptionName">>,
                              <<"com.atlassian.stash.pull.DuplicatePullRequestException">>}},
                    <<"existingPullRequest">>, first, <<"links">>, <<"self">>, first, <<"href">>], BodyEJson),
            {exists, <<"Bitbucket Pull Request: ", PRUrl/binary>>};
        {ok, ResCode, _Head, _ResponseBody} ->
            BinCode = chef_utils:to_bin(ResCode),
            chef_log:info("Failed to open a Pull Request in Bitbucket. ResponseCode=~p Url=~p", [ResCode, Url]),
            {error, <<"Failed to open a Pull Request in Bitbucket with response code ", BinCode/binary, ".">>};
        {error, Why} ->
            {error, <<"Failed to open a Pull Request in Bitbucket with error: ", Why/binary>>}
    end.

%% private
-spec repo_url({atom(), term()}, {atom(), term()}) -> list().
repo_url({ok, BasicAuth}, {ok, BitbucketMetadata}) ->
    RootApi = deliv_basic_auth_application:getval(root_api_url, BasicAuth),

    Project = scm_bitbucket_project_metadata:getval(bitbucket_project, BitbucketMetadata),
    Repo = scm_bitbucket_project_metadata:getval(repo_name, BitbucketMetadata),

    [RootApi, "/rest/api/1.0/projects/", Project, "/repos/", Repo, "/pull-requests"].

%% private
-spec request_headers({atom(), term()}) -> list().
request_headers({ok, BasicAuth}) ->
    User = deliv_basic_auth_application:getval(user_id, BasicAuth),
    Password = deliv_basic_auth_application:getval(password, BasicAuth),
    basic_auth_headers(User, Password).

%% private
-spec pull_request_body(d_patchset(), binary(), binary(), {atom(), term()}) -> ej:json_object().
pull_request_body(Patchset, SourceBranch, DestinationBranch, {ok, BitbucketMetadata}) ->
    ChangeId = deliv_patchset:getval(change_id, Patchset),
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),
    Title = deliv_change:getval(title, Change),
    [Ent, Org, Proj, _] = deliv_change:scoping_names(ChangeId),
    ChangeUrl = deliv_web_utils:make_web_url_for_change(Ent, Org, Proj, ChangeId),
    ChangeDescription = deliv_change:getval(description, Change),
    Project = scm_bitbucket_project_metadata:getval(bitbucket_project, BitbucketMetadata),
    Repo = scm_bitbucket_project_metadata:getval(repo_name, BitbucketMetadata),
    Description = <<ChangeDescription/binary, "\n\nReady to merge? View this change in ",
                    "Chef Automate and click the Approve button:\n", ChangeUrl/binary>>,

    {[{<<"title">>, Title},
      {<<"description">>, Description},
      {<<"state">>, <<"OPEN">>},
      {<<"open">>, true},
      {<<"closed">>, false},
      {<<"fromRef">>,
        {[{<<"id">>, SourceBranch},
          {<<"repository">>,
            {[{<<"slug">>, Repo},
              {<<"name">>, null},
              {<<"project">>,
                {[{<<"key">>, Project}]}}]}}]}},
      {<<"toRef">>,
        {[{<<"id">>, DestinationBranch},
          {<<"repository">>,
            {[{<<"slug">>, Repo},
              {<<"name">>, null},
              {<<"project">>,
                {[{<<"key">>, Project}]}}]}}]}},
      {<<"locked">>, false},
      {<<"reviewers">>, []}]}.

-spec check_reachability(binary(), binary(), binary()) -> {ok, authenticated} | {error, unauthorized} | {error, term()}.
check_reachability(Url, Username, Password) ->
    Headers = basic_auth_headers(Username, Password),
    RootApi = <<Url/binary, "/rest/api/1.0/projects">>,

    case deliv_http:req(get, RootApi, [], Headers) of
        {ok, 200, _Head, _ResponseBody} ->
            {ok, authenticated};
        {ok, 401, _Head, _ResponseBody} ->
            {error, unauthorized};
        {ok, 400, _Head, _ResponseBody} ->
            {error, bad_request};
        {ok, 301, ResponseHeaders, _ResponseBody} ->
            Location = proplists:get_value("Location", ResponseHeaders),
            {error, {redirect, Location}};
        {ok, 404, _Head, _ResponseBody} ->
            {error, not_found};
        {error, Why} ->
            {error, Why}
    end.

%% private
-spec basic_auth_headers(binary(), binary()) -> list().
basic_auth_headers(Username, Password) ->
    Encoded = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    [{"Authorization", "Basic " ++ Encoded}].
