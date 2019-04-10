-module(deliv_hand_intern_users_change_password).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include("deliv_types.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         handle/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        %% as per authz_rules, this is only or admins; however,
        %% any user can update their own password, even if they're not admin.
        {deliv_authz, [{forbidden_for_editing_user/2, forbidden}]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

handle(Req, #handler{ent_name = EntName} = State) ->
    {UserName, Req1} = cowboy_req:binding(user_name, Req),
    case deliv_web_utils:parse_json_req(Req1, jesse_input_spec()) of
        {{error, _}, Req2} ->
            deliv_web_utils:error_response(400, bad_request, Req2, State);
        {Json, Req2} ->
            Password = ej:get([<<"password">>], Json),
            process_input(UserName, EntName, Password, Req2, State)
    end.

process_input(UserName, EntName, Password, Req, State) ->
    handle_db_result(deliv_intern_user:reset_password(EntName, UserName, Password),
                     Req, State).

handle_db_result({ok, _NewUser}, Req, State) ->
    {true, Req, State};
handle_db_result({error, user_not_found}, Req, State) ->
    deliv_web_utils:error_response(404, not_found, Req, State);
handle_db_result({error, bad_password, Reason, Message}, Req, State) ->
    deliv_web_utils:error_response(400, Reason, Message, Req, State);
handle_db_result(_Error, Req, State) ->
    deliv_web_utils:error_response(500, internal_server_error, Req, State).

jesse_input_spec() ->
    chef_json:simple_string_dict_spec([<<"password">>]).
