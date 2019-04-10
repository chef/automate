%% @doc Endpoint handler for dependencies failures.
%%
%% This module provides a endpoint that returns an array of
%% all the currently blocked projects (i.e. dependency failures)
%% for an enterprise.

-module(deliv_hand_blocked_projects).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         init/3,
         to_json/2,
         rest_init/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, #handler{ent_name = EntName} = State) ->
    case deliv_dependency_failures:get_blocked_project_names(EntName) of
        {ok, BlockedProjectNameList} ->
            Json = {[{<<"blocked_projects">>, BlockedProjectNameList}]},
            deliv_web_utils:content(Json, Req, State);
        {error, not_found} ->
            deliv_web_utils:error_response(404, enterprise_not_found, Req, State)
    end.
