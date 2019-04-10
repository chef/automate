-module(deliv_hand_user_authn).
-behaviour(deliv_rest).

-include("deliv_types.hrl").

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         from_json/2,
         rest_init/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(from_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

from_json(Req, State) ->
    handle_parse_get_token_req(parse_get_token_req(Req), State).

handle_parse_get_token_req({ok, {EntName, UserName, Password}, Req}, State) ->
    case deliv_user:fetch(EntName, UserName) of
        {ok, User} ->
            UserType = deliv_user:getval(user_type, User),
            deliv_user:get_token(deliv_user:verify_password(UserType, UserName, EntName, Password),
                      EntName, UserName, Req, State);
        {error, _} ->
            deliv_web_utils:reply_unauthorized(Req, State)
    end;
handle_parse_get_token_req({{error, bad_request}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State).

parse_get_token_req(Req) ->
    case deliv_web_utils:parse_json_req(Req, token_spec()) of
        {{error, _Why}, Req1} ->
            {{error, bad_request}, Req1};
        {Ejson, Req1} ->
            extract_ent_user_pass(Ejson, Req1)
    end.

extract_ent_user_pass(Json, Req) ->
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),
    {ok, {EntName,
          ej:get([<<"username">>], Json),
          ej:get([<<"password">>], Json)}, Req1}.

token_spec() ->
    chef_json:simple_string_dict_spec([<<"username">>, <<"password">>]).
