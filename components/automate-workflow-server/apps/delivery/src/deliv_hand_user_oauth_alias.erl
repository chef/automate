%% @doc Cowboy handler for setting OAuth user aliases
-module(deliv_hand_user_oauth_alias).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").

%% Cowboy Callbacks
-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         from_json/2
        ]).

-include("deliv_types.hrl").

%% Authz
-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [{forbidden_for_editing_user/2, forbidden}]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(from_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(from_json), Req, State}.

from_json(Req, #handler{ent_name = EntName} = State) ->
    {UrlUserName, Req1} = cowboy_req:binding(user_name, Req),
    case deliv_user:fetch(EntName, UrlUserName) of
        {error, not_found} ->
            deliv_web_utils:error_response(404, not_found, Req1, State);
        {ok, User} ->
            Parsed = deliv_web_utils:parse_json_req(Req1, user_set_oauth_alias),
            validate_and_process_input(Parsed, User, EntName, State)
    end.

validate_and_process_input({{error, {Why, _Ejson}}, Req}, _User, _EntName, State) ->
    deliv_web_utils:error_response(400, bad_request, Why, Req, State);
validate_and_process_input({Ejson, Req}, User, _EntName, State) ->
    UserId = deliv_user:getval(id, User),
    AppName = ej:get([<<"app_name">>], Ejson),
    Alias = ej:get([<<"alias">>], Ejson),
    case deliv_oauth_application:fetch(AppName) of
        {ok, OauthApp} ->
            insert_alias(OauthApp, UserId, Alias, Req, State);
        {error, not_found} ->
            deliv_web_utils:error_response(400, oauth_app_not_found,
                                           <<"OAuth Application not found.">>,
                                           Req, State);
        _ ->
            deliv_web_utils:error_response(500, internal_server_error, Req, State)
    end.

%% @private
%% @doc Insert a new Oauth alias for the specified user. If we run into a conflict,
%% that means that either a) that user already has an alias or b) that alias already
%% exists within the system. We currently don't have a great way of figuring out
%% which one caused the conflict so we will parse find out in `handle_conflict`
insert_alias(OauthApp, UserId, Alias, Req, State) ->
    OauthAppId = deliv_oauth_application:getval(id, OauthApp),
    case deliv_oauth_user_alias:insert(OauthAppId, UserId, Alias) of
        {ok, _} ->
            {true, Req, State};
        {error, conflict} ->
            handle_conflict(OauthApp, UserId, Alias, Req, State);
        _ ->
            deliv_web_utils:error_response(500, internal_server_error, Req, State)
    end.

%% @private
%% @doc We ran into a conflict when trying to insert a new alias into the DB. To
%% determine the cause and proceed we will try and fetch the offending record. If
%% we can fetch the record that means that the user already has an alias and we
%% just need to update it.
%%
%% If we can't find a record that means that the requested alias is currently
%% being used by another user.
handle_conflict(OauthApp, UserId, Alias, Req, State) ->
    case deliv_oauth_user_alias:fetch(OauthApp, UserId) of
        {ok, AliasRec} ->
            handle_update_result(update(AliasRec, Alias), Req, State);
        {error, not_found} ->
            deliv_web_utils:error_response(400, oauth_alias_unavailable,
                                           <<"Oauth username has already been associated with another user.">>,
                                           Req, State);
        _ ->
            deliv_web_utils:error_response(500, internal_server_error, Req, State)
    end.


%% @private
%% @doc Check to see if the user_id for the oauth app alias and the user_id for
%% the user are identical. If they are, update the oauth app alias. If they are
%% different, return an error.
update(AliasRec, Alias) ->
    deliv_oauth_user_alias:update(deliv_oauth_user_alias:setvals([{alias, Alias}], AliasRec)).

%% @private
handle_update_result({ok, _NewUser}, Req, State) ->
    {true, Req, State};
handle_update_result({error, conflict}, Req, State) ->
    deliv_web_utils:error_response(400, oauth_alias_unavailable,
                                   <<"Oauth username has already been associated with another user.">>,
                                   Req, State);
handle_update_result({error, _Why}, Req, State) ->
    deliv_web_utils:error_response(500, internal_server_error, Req, State).
