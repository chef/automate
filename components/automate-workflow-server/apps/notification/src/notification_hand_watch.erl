-module(notification_hand_watch).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         init/3,
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

from_json(Req, State) ->
    handle_parse(
        deliv_web_utils:parse_json_req(Req, categories_spec()), State
    ).

to_json(Req, #handler{ent_name = EntName, user_name = UserName} = State) ->
    {[OrgName, ProjName], Req1} =
        deliv_web_utils:extract_bindings([org_name, proj_name], Req),

    handle_subscription(
        notification_subscriptions:subscription(EntName, OrgName, ProjName, UserName),
        Req1,
        State
    ).

%% PRIVATE

categories_spec() ->
    chef_json:rigid_object_spec([{<<"categories">>, <<"[binary()]">>}]).

handle_parse({{error, Why}, Req}, State) ->
    chef_log:failed_call(deliv_web_utils, parse_json_req, [Req, categories_spec()], Why),
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req}, #handler{ent_name = EntName,
                                    user_name = UserName} = State) ->
    {[OrgName, ProjName], Req1} =
        deliv_web_utils:extract_bindings([org_name, proj_name], Req),
    Subscriptions = ej:get([<<"categories">>], Ejson),

    handle_subscribe(
        notification_subscriptions:subscribe(EntName, OrgName, ProjName, UserName, Subscriptions),
        Req1,
        State
    ).

handle_subscribe({error, cannot_subscribe}, Req, State) ->
    deliv_web_utils:error_response(412, precondition_failed, Req, State);
handle_subscribe({error, _}, Req, State) ->
    deliv_web_utils:error_response(500, internal_server_error, Req, State);
handle_subscribe(ok, Req, State) ->
    {true, Req, State}.

handle_subscription({error, _Why}, Req, State) ->
    deliv_web_utils:error_response(500, internal_server_error, Req, State);
handle_subscription(Result, Req, State) when is_list(Result) ->
    deliv_web_utils:content({[{<<"categories">>, Result}]}, Req, State).
