-module(audit_hand_audit).
-behaviour(deliv_rest).

-export([
         allowed_methods/2,
         init/3,
         content_types_provided/2,
         to_json/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, State) ->
    {audit_web_utils:to_json(audit_subscriptions:audit_log()), Req, State}.
