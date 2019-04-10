-module(notification_hand_slack_webhook).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include("notification_types.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         init/3,
         from_json/2,
         rest_init/2,
         resource_exists/2,
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

delete_resource(Req, State) ->
    {[EntName, OrgName], Req1} = deliv_web_utils:extract_bindings([ent_name, org_name], Req),
    case notification_config_db:delete(EntName, OrgName) of
        ok ->
            {true, Req1, State};
        {error, org_not_found} ->
            chef_log:error("Cannot delete notification configuration for organization ~s in enterprise ~s: organization not found",
                            [OrgName, EntName]),
            deliv_web_utils:error_response(412, precondition_failed, Req1, State);
        {error, Why} ->
            chef_log:failed_call(notification_config_db, delete,
                                  [EntName, OrgName], Why),
            deliv_web_utils:error_response(500, internal_server_error, Req1, State)
    end.

from_json(Req, State) ->
    handle_parse(
        deliv_web_utils:parse_json_req(Req, slack_webhook_jesse_spec()),
        State
    ).

to_json(Req, State) ->
    {[EntName, OrgName], Req1} = deliv_web_utils:extract_bindings([ent_name, org_name], Req),
    case notification_config_db:fetch(EntName, OrgName) of
        [] ->
            deliv_web_utils:error_response(404, not_found, Req1, State);
        {error, Why} ->
            chef_log:failed_call(?MODULE, fetch, [EntName, OrgName], Why),
            deliv_web_utils:error_response(500, internal_server_error, Req1, State);
        [Config] ->
            Ejson = notification_web_utils:to_json(Config),
            deliv_web_utils:content(Ejson, Req1, State)
        end.

%% Required to properly reply a 201
resource_exists(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Method =/= <<"PUT">>, Req2, State}.

% private

slack_webhook_jesse_spec() ->
    chef_json:rigid_object_spec([{<<"enabled">>, boolean},
                                  {<<"name">>, string},
                                  {<<"url">>, string}]).

handle_parse({{error, _Why}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req}, State) ->
    {[EntName, OrgName], Req1} = deliv_web_utils:extract_bindings([ent_name, org_name], Req),

    case notification_config:save(slack_webhook, [EntName, OrgName], Ejson) of
        {error, org_not_found} ->
            deliv_web_utils:error_response(412, precondition_failed, Req1, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Req1, State);
        _Config ->
            {true, Req1, State}
    end.
