-module(notification_hand_test_smtp).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         from_json/2,
         init/3,
         rest_init/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

from_json(Req, State) ->
    handle_parse(deliv_web_utils:parse_json_req(Req, notification_smtp_config), State).

%% PRIVATE

handle_parse({{error, _Why} = Error, Req}, State) ->
    chef_log:failed_call(deliv_web_utils, parse_json_req, [Req, notification_smtp_config], Error),
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({SmtpConfig, Req}, #handler{ent_name = EntName, user_name = UserName} = State) ->
    Result = notification_smtp_test:send(EntName, UserName, SmtpConfig),
    log_send(Result, [EntName, UserName, SmtpConfig]),
    respond(Result, Req, State).

log_send({error, _} = Error, Params) ->
    chef_log:failed_call(notification_smtp_test, send, Params, Error);
log_send({error, _, _} = Error, Params) ->
    chef_log:failed_call(notification_smtp_test, send, Params, Error);
log_send(_, _Params) ->
    ok.

respond({error, Reason}, Req, State) when Reason == user_not_found orelse Reason == no_user_email ->
    deliv_web_utils:error_response(412, precondition_failed, Req, State);
respond({error, user_not_found, _Why}, Req, State) ->
    deliv_web_utils:error_response(412, precondition_failed, Req, State);
respond({error, _Type, _Message}, Req, State) ->
    deliv_web_utils:error_response(502, bad_gateway, Req, State);
respond({error, _Why}, Req, State) ->
    deliv_web_utils:error_response(500, internal_server_error, Req, State);
respond(_, Req, State) ->
    {true, Req, State}.
