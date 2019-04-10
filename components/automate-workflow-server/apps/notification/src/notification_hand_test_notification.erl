-module(notification_hand_test_notification).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include("notification_macros.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         init/3,
         from_json/2,
         rest_init/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

from_json(Req, State) ->
    handle_parse(
        deliv_web_utils:parse_json_req(Req, test_notification_jesse_spec()),
        State
    ).

test_notification_jesse_spec() ->
    chef_json:simple_string_dict_spec([<<"url">>]).

handle_parse({{error, _Why}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req}, State) ->
    case submit(Ejson) of
        {ok, 200, _RespHeaders, _RespBody} ->
            {true, Req, State};
        {ok, Status, _RespHeaders, _RespBody} ->
            deliv_web_utils:error_response(Status, test_notification_failed,
                                           Req, State);
        {error, _Why} ->
            deliv_web_utils:error_response(504, gateway_timeout, Req, State)
    end.

submit(Ejson) ->
    JSON = test_message_json(),
    Headers = [{"Content-Type", "application/json"}],
    Webhook = ej:get([<<"url">>], Ejson),
    deliv_http:req(post, Webhook, JSON, Headers).

test_message_json() ->
    {[
        {<<"username">>, <<"Chef_Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, <<"Test message from Chef Automate!">>},
                {<<"text">>, <<"Test message from Chef Automate!">>}
            ]}
        ]}
    ]}.
