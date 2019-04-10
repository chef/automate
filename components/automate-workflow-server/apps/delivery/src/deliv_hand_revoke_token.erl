-module(deliv_hand_revoke_token).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include("deliv_types.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         handle/2
        ]).

-mixin([{deliv_token, [is_authorized/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

handle(Req, #handler{ent_name = EntName} = State) ->
    %% first sanity check, you can only revoke your own tokens
    {UserNameFromUri, Req1} = cowboy_req:binding(user_name, Req),
    {EntName, UserNameFromAuthHeader, Token, Req2} = deliv_token:extract_token_info(Req1),
    case UserNameFromAuthHeader =:= UserNameFromUri of
        true ->
            handle_revoke(deliv_user:revoke_token(EntName, UserNameFromUri, Token), Req2, State);
        false ->
            deliv_web_utils:error_response(403, <<"operation_not_permitted">>, Req2, State)
    end.

handle_revoke(OkOrNotFound, Req, State) when
    OkOrNotFound =:= ok orelse OkOrNotFound =:= not_found ->
    {true, Req, State};
handle_revoke({error, _Why}, Req, State) ->
    deliv_web_utils:error_response(500, internal_server_error, Req, State).
