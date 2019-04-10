-module(deliv_hand_canonical_enterprise).
-behaviour(deliv_rest).

-include("deliv_types.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         init/3,
         to_json/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

to_json(Req, State) ->
    case deliv_enterprise:get_canonical_enterprise() of
        {ok, CanonicalEntName} ->
            Json = {[{<<"ent_name">>, CanonicalEntName}]},
            deliv_web_utils:content(Json, Req, State);
        _ ->
            deliv_web_utils:error_response(404, no_canonical_enterprise, Req, State)
    end.
