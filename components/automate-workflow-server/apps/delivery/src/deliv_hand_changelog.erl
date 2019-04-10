-module(deliv_hand_changelog).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include("deliv_types.hrl").

-export([
         init/3,
         rest_init/2,
         handle/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

handle(Req, #handler{ent_name = EntName} = State) ->
    {OrgName, Req1} = cowboy_req:binding(org_name, Req),
    {ProjName, Req2} = cowboy_req:binding(proj_name, Req1),
    {PipeName, Req3} = cowboy_req:binding(pipe_name, Req2),
    case deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName) of
        {error, not_found} ->
            deliv_web_utils:error_response(404, not_found, Req1, State);
        {ok, Pipe} ->
            do_handle(Pipe, Req3, State)
    end.

do_handle(Pipe, Req, State) ->
    PipelineId = deliv_pipeline:getval(id, Pipe),
    case deliv_change:changelog_for_open_changeset(PipelineId) of
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Req, State);
        Changes when erlang:is_list(Changes) ->
            Ejson = generate_changelog(Changes),
            deliv_web_utils:content(Ejson, Req, State)
    end.

generate_changelog(Changes) ->
    lists:map(fun change_to_json/1, Changes).

change_to_json(Change) ->
    {[{<<"id">>, deliv_change:getval(id, Change)},
      {<<"title">>, deliv_change:getval(title, Change)},
      {<<"description">>, deliv_change:getval(description, Change)},
      {<<"feature_branch">>, deliv_change:getval(feature_branch, Change)},
      {<<"approved_by">>, deliv_change:getval(approved_by, Change)}]}.
