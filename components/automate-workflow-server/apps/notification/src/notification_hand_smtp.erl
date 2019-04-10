-module(notification_hand_smtp).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include("notification_types.hrl").

-export([
         allowed_methods/2,
         delete_resource/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         init/3,
         resource_exists/2,
         rest_init/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"DELETE">>, <<"GET">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

content_types_provided(Req, #handler{} = State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

delete_resource(Req,  #handler{ent_name = EntName} = State) ->
    case notification_config_db:delete(EntName) of
        ok ->
            {true, Req, State};
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Req, State)
    end.

from_json(Req, State) ->
    handle_parse(
        deliv_web_utils:parse_json_req(Req, notification_smtp_config),
        State
    ).

to_json(Req, State) ->
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),
    case notification_config_db:fetch(EntName) of
        [] -> deliv_web_utils:error_response(404, not_found, Req1, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_error, Req1, State);
        [Config] ->
            Ejson = notification_web_utils:to_json(Config),
            deliv_web_utils:content(Ejson, Req1, State)
    end.

%% Required to properly reply a 201
resource_exists(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Method =/= <<"PUT">>, Req2, State}.

%% PRIVATE

handle_parse({{error, Why}, Req}, State) ->
    chef_log:failed_call(?MODULE, from_json, [Req], Why),
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req}, State) ->
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),

    case notification_config:save(smtp, [EntName], Ejson) of
        {error, no_password} ->
            chef_log:failed_call(?MODULE, from_json, [Req1], bad_request),
            deliv_web_utils:error_response(400, bad_request, Req1, State);
        {error, Why} ->
            chef_log:error("Could not save SMTP notification configuration for enterprise ~s: ~p", [EntName, Why]),
            deliv_web_utils:error_response(500, internal_server_error, Req1, State);
        _Config ->
            {true, Req1, State}
    end.
