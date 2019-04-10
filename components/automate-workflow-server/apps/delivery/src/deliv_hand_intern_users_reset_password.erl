-module(deliv_hand_intern_users_reset_password).
-behaviour(deliv_rest).

-include("deliv_types.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         handle/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

handle(Req, State) ->
    {[EntName, UserName], Req1} = deliv_web_utils:extract_bindings([ent_name, user_name], Req),
    case deliv_web_utils:parse_json_req(Req1, jesse_input_spec()) of
        {{error, _}, Req2} ->
            deliv_web_utils:error_response(400, bad_request, Req2, State);
        {Json, Req2} ->
            NewPassword = ej:get([<<"password">>], Json),
            Token = ej:get([<<"token">>], Json),
            process_input(UserName, EntName, Token, NewPassword, Req2, State)
    end.

process_input(UserName, EntName, Token, NewPassword, Req, State) ->
    Return = deliv_reset_password:consume_reset_token(EntName, UserName, Token, NewPassword),
    handle_db_result(Return, Req, State).

handle_db_result({ok, _}, Req, State) ->
    {true, Req, State};
handle_db_result({error, token_not_found}, Req, State) ->
    deliv_web_utils:error_response(401, not_authorized, Req, State);
handle_db_result({error, bad_password, Reason, Message}, Req, State) ->
    deliv_web_utils:error_response(400, Reason, Message, Req, State);
handle_db_result(_Error, Req, State) ->
    deliv_web_utils:error_response(500, internal_server_error, Req, State).

jesse_input_spec() ->
    chef_json:simple_string_dict_spec([<<"password">>, <<"token">>]).
