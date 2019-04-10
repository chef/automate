-module(auth_hand_saml_enabled).
-behaviour(deliv_rest).

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         rest_init/2,
         resource_exists/2,
         to_json/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

to_json(Req, State) ->
    {[EntName], Req1} = deliv_web_utils:extract_bindings([ent_name], Req),
    handle_config(
        auth_saml_config:fetch(EntName),
        Req1,
        State).

handle_config({ok, _Config}, Req, State) ->
    deliv_web_utils:content({[{<<"enabled">>, true}]}, Req, State);
handle_config({error, not_found}, Req, State) ->
    deliv_web_utils:content({[{<<"enabled">>, false}]}, Req, State);
handle_config({error, _Why}, Req, State) ->
    deliv_web_utils:error_response(500, internal_server_error, Req, State).
