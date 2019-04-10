-module(notification_hand_config_for_ent).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
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
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, State) ->
     {EntName, Req1} = cowboy_req:binding(ent_name, Req),
     case notification_config_db:fetch(EntName) of
        [] ->
            deliv_web_utils:content({[{<<"notifications">>, []}]}, Req1, State);
        [_Config] ->
            deliv_web_utils:content({[{<<"notifications">>, [<<"smtp">>]}]}, Req1, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Req1, State)
    end.
