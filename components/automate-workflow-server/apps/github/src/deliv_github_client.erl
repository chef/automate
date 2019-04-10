%%% @doc Handle low-level HTTP requests to the Github API.
%%%
%%% Includes operations related to obtaining OAuth tokens from Github,
%%% as that is a low-level implementation detail that the rest of the
%%% system generally doesn't need to care about.
-module(deliv_github_client).

%% Core request processing functions
-export([
         org_req/4,
         user_req/4,
         req/3,
         req/4,
         req/5
        ]).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-record(github_client, {
          base_url :: iodata(),
          headers  :: http_headers(str_or_binary())
         }).

-record(github_org_client, {
          base_url :: iodata(),
          headers  :: http_headers(str_or_binary())
         }).

%% @private
%%
%% @doc Given a Github repository owner/org and a repository name,
%% generate the base Github API URL needed for interacting with that repository.
%%
%% Note: Assumes Github API version 3.
-spec repo_base_url(binary(), binary(), d_oauth_application()) -> iolist().
repo_base_url(RepoOwner, RepoName, Application) ->
    [base_url(Application), "/repos/", RepoOwner, "/", RepoName].

org_base_url(RepoOwner, Application) ->
    [base_url(Application), "/orgs/", RepoOwner].

user_base_url(Application) ->
    [base_url(Application), "/user"].

base_url(Application) ->
    deliv_oauth_application:getval(root_api_url, Application).

%% @private
%%
%% @doc HTTP headers required to make an OAuth-authenticated Github
%% API request.
github_req_headers(Token) ->
    [{"Authorization", <<"token ", Token/binary>>},
     {"Accept", <<"application/json">>},
     {"User-Agent", <<"Chef-Delivery-API">>}].

%% @private
%%
%% @doc Create a client object for interacting with a repository via
%% the Github API
-spec github_client(binary(), binary(), binary(), d_oauth_application()) -> #github_client{}.
github_client(RepoOwner, RepoName, Token, Application) ->
    #github_client{base_url = repo_base_url(RepoOwner, RepoName, Application),
                   headers  = github_req_headers(Token)}.

github_org_client(RepoOwner, Token, Application) ->
    #github_org_client{base_url = org_base_url(RepoOwner, Application),
                       headers  = github_req_headers(Token)}.

github_user_client(Token, Application) ->
    #github_client{base_url = user_base_url(Application),
                   headers = github_req_headers(Token)}.

%% @doc Makes an API call for said repo, adding the right auth header
%% Note that this method already adds the `/api/v3/repos/:owner/:repo' part of
%% the route. So for example to perform a merge, you only need to make the
%% `Route' param be "/merges"; or to list webhooks, simply "/hooks"
%% Returns `{error, no_token_found}' if we don't have a token for that project
%% in the DB
%%
%% See more at:
%% * https://developer.github.com/v3/repos/
%% * https://developer.github.com/v3/oauth/#use-the-access-token-to-access-the-api
-spec req(d_common_scope(), http_method(), iodata())
         -> {ok, http_status(), http_headers(str_or_binary()), binary()} |
            {error, no_token_found | _Why}.
req(Scope, Method, Route) ->
    Coords = scope_to_coordinates(Scope),
    req(Coords, Method, Route, <<>>, []).

-spec req(d_common_scope(), http_method(), iodata(), http_body())
         -> {ok, http_status(), http_headers(str_or_binary()), binary()} |
            {error, no_token_found | _Why}.
req(Scope, Method, Route, Body) ->
    Coords = scope_to_coordinates(Scope),
    req(Coords, Method, Route, Body, []).

-spec req(#proj_coordinates{}, http_method(), iodata(), http_body(), http_headers(str_or_binary()))
         -> {ok, http_status(), http_headers(str_or_binary()), binary()} |
            {error, no_token_found | _Why}.
req(Coords=#proj_coordinates{}, Method, Route, Body, Headers) ->
    case fetch_client(Coords) of
        {ok, #github_client{headers=RequiredHeaders, base_url=BaseUrl}} ->
            Url = [BaseUrl, Route],
            Headers2 = RequiredHeaders ++ Headers,
            do_req(Method, Url, Body, Headers2);
        {error, _Why} = Err ->
            Err
    end.

-spec org_req(#proj_coordinates{}, http_method(), iodata(), http_body())
             -> {ok, http_status(), http_headers(str_or_binary()), binary()} |
                {error, term()}.
org_req(Coords=#proj_coordinates{}, Method, Route, Body) ->
    case fetch_org_client(Coords) of
        {ok, #github_org_client{headers=RequiredHeaders, base_url=BaseUrl}} ->
            Url = [BaseUrl, Route],
            do_req(Method, Url, Body, RequiredHeaders);
        {error, _Why} = Err ->
            Err
    end.

-spec user_req(binary(), http_method(), iodata(), http_body())
             -> {ok, http_status(), http_headers(str_or_binary()), binary()} |
                {error, term()}.
user_req(EntName, Method, Route, Body) ->
    case fetch_user_client(EntName) of
        {ok, #github_client{headers=RequiredHeaders, base_url=BaseUrl}} ->
            Url = [BaseUrl, Route],
            do_req(Method, Url, Body, RequiredHeaders);
        {error, _Why} = Err ->
            Err
    end.

%% @private
%% Given a `Scope' object for a given project, provide information
%% necessary for issuing an OAuth-authenticated HTTP request to the
%% Github API.
%%
%% Obviously, said project needs to have been previously associated
%% with a Github repository; furthermore, we must have a token for it already.
-spec fetch_client(#proj_coordinates{}) -> {ok, #github_client{}} |
                                           {error, no_token_found}.
fetch_client(#proj_coordinates{ent_name=EntName,
                               org_name=OrgName,
                               proj_name=ProjName}) ->
    {ok, Project} = deliv_project:fetch(EntName, OrgName, ProjName),
    ProjectId = deliv_project:getval(id, Project),

    case {deliv_project_github_metadata:client_details(ProjectId),
          deliv_oauth_token:fetch_by_enterprise(EntName, deliv_github_oauth)} of
        {{ok, RepoOwner, RepoName}, {ok, TokenRec}} ->
            {ok, Application} = deliv_oauth_application:fetch_by_token_record(TokenRec),
            Token = deliv_oauth_token:getval(token, TokenRec),
            {ok, github_client(RepoOwner, RepoName, Token, Application)};
        {_, _} ->
            {error, no_token_found}
    end.


fetch_org_client(#proj_coordinates{ent_name=EntName,
                                   org_name=OrgName,
                                   proj_name=ProjName}) ->
    {ok, Project} = deliv_project:fetch(EntName, OrgName, ProjName),
    ProjectId = deliv_project:getval(id, Project),

    case {deliv_project_github_metadata:client_details(ProjectId),
          deliv_oauth_token:fetch_by_enterprise(EntName, deliv_github_oauth)} of
        {{ok, RepoOwner, _}, {ok, TokenRec}} ->
            {ok, Application} = deliv_oauth_application:fetch_by_token_record(TokenRec),
            Token = deliv_oauth_token:getval(token, TokenRec),
            {ok, github_org_client(RepoOwner, Token, Application)};
        {_, _} ->
            {error, no_token_found}
    end.

fetch_user_client(EntName) ->
    case deliv_oauth_token:fetch_by_enterprise(EntName, deliv_github_oauth) of
        {ok, TokenRec} ->
            {ok, Application} = deliv_oauth_application:fetch_by_token_record(TokenRec),
            Token = deliv_oauth_token:getval(token, TokenRec),
            {ok, github_user_client(Token, Application)};
        _ ->
            {error, no_token_found}
    end.

%% @private
-spec do_req(http_method(), iodata(), http_body(), http_headers(str_or_binary()))
            -> {ok, http_status(), http_headers(str_or_binary()), binary()} |
               {error, _Why}.
do_req(Method, Url, Body, Headers) ->
    case deliv_http:req(Method, Url, Body, Headers) of
        {ok, Status, _, ResponseBody} = Response ->
            handle_response(Status, ResponseBody, Url),
            Response;
        {error, Reason} ->
            chef_log:error("failed_http_req=~p; reason=~p", [[Method, Url], Reason]),
            {error, Reason}
    end.

%% @private
%% Convert `deliv_scopes` object into #proj_coordinates record
scope_to_coordinates(CommonScope) ->
    [EntName, OrgName, ProjName, _] = deliv_scopes:'#get'(scoping_names, CommonScope),
    #proj_coordinates{ent_name = EntName,
                      org_name = OrgName,
                      proj_name = ProjName}.

%% @private
handle_response(Status,_,_) when Status < 300 ->
    ok;
handle_response(Status, Body, Url) ->
    chef_log:error("failed_http_req=~s; body=~s; reason=~p", [Url, Body, Status]).
