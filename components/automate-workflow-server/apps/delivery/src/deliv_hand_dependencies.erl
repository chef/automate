-module(deliv_hand_dependencies).
-behaviour(deliv_rest).

-include("deliv_types.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         to_json/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, State) ->
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),
    {OrgName, Req2} = cowboy_req:binding(org_name, Req1),
    {ProjName, Req3} = cowboy_req:binding(proj_name, Req2),
    case deliv_changeset:dependency_ejson_for_project(EntName, OrgName, ProjName) of
        {ok, Ejson} ->
            deliv_web_utils:content(Ejson, Req3, State);
        {error, Why} ->
            handle_error({Why, Req3})
    end.

handle_error({not_found, Req}) ->
    handle_error({not_found, Req, <<>>});
handle_error({_, Req}) ->
    deliv_web_utils:error_response(500, server_error, Req, <<>>);
handle_error({not_found, Req, What}) ->
    deliv_web_utils:error_response(404, not_found, Req, What).
