-module(auth_hand_saml_config).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include_lib("auth_types.hrl").

-export([
          allowed_methods/2,
          content_types_provided/2,
          content_types_accepted/2,
          from_json/2,
          init/3,
          resource_exists/2,
          delete_resource/2,
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
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

%% Required to properly reply a 201
resource_exists(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Method =/= <<"POST">>, Req2, State}.

delete_resource(Req, #handler{ent_name = EntName} = State) ->
    case auth_saml_config:delete(EntName) of
        ok ->
            {true, Req, State};
        {error, Why} ->
            chef_log:error("Error deleting SAML config for ~s: ~p", [EntName, Why]),
            deliv_web_utils:error_response(500, internal_server_error, Req, State)
    end.

to_json(Req, State) ->
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),
    case auth_saml_config:fetch(EntName) of
        {ok, Config} ->
            deliv_web_utils:content(auth_saml_config:to_json(Config), Req1, State);
        {error, not_found} ->
            chef_log:error("SAML config not found for ~p", [EntName]),
            deliv_web_utils:error_response(404, not_found, Req1, State);
        {error, Why} ->
            chef_log:error("Failed to retrieve SAML config for ~p: ~p", [EntName, Why]),
            deliv_web_utils:error_response(500, internal_server_error, Req1, State)
    end.

from_json(Req, State) ->
    handle_parse(
        deliv_web_utils:parse_json_req(Req, saml_config),
        State
    ).

handle_parse({{error, {[{data_invalid, _Spec, wrong_size, _Value, [<<"default_roles">>]}], _Input}}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, <<"Please select at least one default role.">>, Req, State);
handle_parse({{error, _Why}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req}, State) ->
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),
    Config = auth_saml_config:from_json(Ejson, EntName),
    handle_metadata(auth_saml_config:refresh_metadata(Config), Req1, State).

handle_metadata({ok, Config}, Req, State) ->
    case auth_saml_config:upsert(Config) of
        {ok, _} -> {true, Req, State};
        {error, _} = Error -> handle_metadata(Error, Req, State)
    end;
handle_metadata({error, too_big}, Req, State) ->
    deliv_web_utils:error_response(400, bad_request,
                                   auth_saml_utils:metadata_too_big_error_message(), Req, State);
handle_metadata({error, status, Status}, Req, State) ->
    deliv_web_utils:error_response(400, bad_request,
                                   <<"received status ",
                                     (chef_utils:to_bin(Status))/binary,
                                     " when retrieving metadata">>, Req, State);
handle_metadata({error, Why}, Req, State) ->
    chef_log:error("Failed to upsert config: ~p", [Why]),
    deliv_web_utils:error_response(500, internal_server_error, Why, Req, State).
