-module(deliv_hand_users_named).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         handle/2,
         delete_resource/2,
         delete_completed/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [{forbidden_for_editing_user/2, forbidden}]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

%% deliv_token:is_authorized -> checks with a2 auth endpoint(in a2_mode) if the token is authorized

%% deliv_authz:forbidden -> checks for the roles associated with the users.
%%                          in the a2 mode, if the user does not exist, always allow i.e bypass forbidden check.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

delete_resource(Req, #handler{ent_name=EntName, user_name=UserName} = State) ->
    {TargetUserName, Req1} = cowboy_req:binding(user_name, Req),
    %% no one can delete themselves
    case TargetUserName =:= UserName of
        true ->
            deliv_web_utils:error_response(403, <<"operation_not_permitted">>, Req1, State);
        false ->
            handle_delete_result(deliv_user:delete(EntName, TargetUserName), Req1, State)
    end.

handle_delete_result(ok, Req, State) -> {true, Req, State};
handle_delete_result(_Error, Req, State) -> deliv_web_utils:error_response(500, internal_server_error, Req, State).

delete_completed(Req, State) ->
    {true, Req, State}.

%% check is the user exists
%% --> not_found - check if in a2_mode
%% -----------------> insert_user
%% --------------------> conflict - never be a conflict
%% --------------------> error - error inserting
%% --------------------> success - assign roles to this user
%%                                 handle the actual request that came in
%% --> user_record - handle the actual request that came in

handle(Req, #handler{ent_name=EntName} = State) ->
    {UserName, Req1} = cowboy_req:binding(user_name, Req),
    case deliv_user:fetch(EntName, UserName) of
        {error, not_found} ->
            %% if the user is not found in a2_mode, insert user record and authz info.
            case envy:get(delivery, a2_mode, false, boolean) of
                %% TODO: create a function for create_user_if_missing
                true ->
                        UserData = [{name, UserName},
                                    {user_type, <<"a2">>}],
                        case deliv_a2_user:insert(EntName, UserData) of
                            {error, conflict} ->
                                deliv_web_utils:error_response(409, conflict, Req, State);
                            {error, _Why} ->
                                deliv_web_utils:error_response(500, internal_error, Req, State);
                            [_] ->
                                setup_a2_user(UserName, Req, State)
                        end;
                false -> deliv_web_utils:error_response(404, not_found, Req1, State)
            end;
        {ok, User} ->
            {Method, Req2} = cowboy_req:method(Req1),
            User2 = maybe_convert_to_a2_user(User, State),
            do_handle(Method, User2, Req2, State)
    end.

setup_a2_user(UserName, Req, #handler{ent_name=EntName} = State) ->
    %% Is the hal code needed to insert authz permissions?
    %% Username = deliv_a2_user:getval(name, InternUser),
    %% Link = deliv_web_utils:href(EntName, ["/users/", Username]),
    %% Links = [{full, Link},
    %%          {'change-password',
    %%           deliv_web_utils:href(EntName, ["/internal-users/",
    %%                                          Username,
    %%                                          "/change-password"])}],
    %% HAL = deliv_web_utils:make_hal(Links),
    %% Req1 = deliv_web_utils:set_json_body({[{<<"_links">>, HAL}]}, Req),
    %% {{true, Link}, Req1, State};

    %% We could make this configurable; last argument is list of roles (e.g. [<<"admin">>])
    deliv_authz:assign_roles(EntName, UserName, []),
    {Method, Req1} = cowboy_req:method(Req),
    %% This should be a case for error handling
    {ok, User} = deliv_user:fetch(EntName, UserName),
    %% The semantics of this are a little weird, in that they turn a
    %% 'GET', normally a idempotent, read only operation into an 'UPDATE'
    User1 = maybe_convert_to_a2_user(User, State),
    do_handle(Method, User1, Req1, State).

%% @doc Fetch data for the specified user.
%% If the user is an internal user, links will include a rel "change-password".
do_handle(<<"GET">>, User, Req, #handler{ent_name=EntName} = State) ->
    Username = deliv_user:getval(name, User),
    BaseJson = {[
        {<<"name">>, Username},
        {<<"user_type">>, deliv_user:getval(user_type, User)},
        {<<"first">>, deliv_user:getval(first_name, User)},
        {<<"last">>, deliv_user:getval(last_name, User)},
        {<<"email">>, deliv_user:getval(email, User)},
        {<<"ssh_pub_key">>, deliv_user:getval(ssh_pub_key, User)},
        {<<"telemetry_enabled">>, deliv_user:getval(telemetry_enabled, User)}
    ]},
    %% now we generate the HAL json
    SelfLink = deliv_web_utils:href(EntName, <<"/users/", Username/binary>>),
    HalList = [{<<"self">>, SelfLink}] ++ case deliv_user:is_internal(User) of
        true ->
            %% we need to add password-management links for internal users
            ChangePasswordLink = deliv_web_utils:href(EntName, <<"/internal-users/", Username/binary, "/change-password">>),
            [
                {<<"change-password">>, ChangePasswordLink}
            ];
        false ->
            []
    end,
    Hal = deliv_web_utils:make_hal(HalList),
    Body = ej:set([<<"_links">>], BaseJson, Hal),
    %% and we can finally send all that back!
    deliv_web_utils:content(Body, Req, State);
%% @doc Update data fields for a user
do_handle(<<"PUT">>, User, Req, #handler{user_name=UserName} = State) ->
    handle_json_input(
        case deliv_web_utils:parse_json_req(Req) of
            {{error, _}, _} = Error -> Error;
            {Body, Req1} -> {not_logged_in_user(deliv_user_json:validate_update(User, Body),
                                                User,
                                                UserName),
                             Req1}
        end,
        User,
        State
    ).

maybe_convert_to_a2_user(User, #handler{ent_name=EntName} = _State) ->
    case envy:get(delivery, a2_mode, false, boolean) andalso is_user_eligible_to_convert(User) of
        true ->
            %% There is some question whether this should be done before or after updating to a2 type.
            UserName = deliv_user:getval(name, User),
            deliv_intern_user:unset_password(EntName, UserName),

            UpdatedUser = deliv_user:setvals([{user_type, <<"a2">>}], User),
            deliv_user:update(UpdatedUser),
            UpdatedUser;
        _ ->
            User
    end.

is_user_eligible_to_convert(User) ->
    Username = deliv_user:getval(name, User),
    Ineligible = [<<"admin">>, <<"builder">>],
    UserType = deliv_user:getval(user_type, User),
    IsEligible = not lists:member(Username, Ineligible),
    case UserType of
        <<"a2">> ->
            false;
        _ ->
            IsEligible
    end.

handle_json_input({{error, invalid_type_change}, Req}, _User, State) ->
    bad_request(<<"Requested user authentication type change is not permitted.">>, Req, State);
handle_json_input({{error, logged_in_user}, Req}, _User, State) ->
    bad_request(<<"Cannot change name of logged in user.">>, Req, State);
handle_json_input({{error, _Why}, Req}, _User, State) ->
    bad_request(<<"Please fill out all required fields.">>, Req, State);
handle_json_input({{List}, Req}, User, State) ->
    update_user(
        deliv_web_utils:translate_proplist(List, deliv_user_json:translate_input(User)),
        User, Req, State
    ).

%% @doc Catch attempts to change the username of the user that is logged in.
not_logged_in_user({error, _} = Error, _, _) -> Error; % pass-through
not_logged_in_user(UpdateJson, UserFromDatabase, UserNameFromToken) ->
  case deliv_user:getval(name, UserFromDatabase) =:= UserNameFromToken andalso  % logged in user is target
       ej:get([<<"name">>], UpdateJson) =/= UserNameFromToken of % want to change username
      true -> {error, logged_in_user};
      _ ->  UpdateJson
  end.

update_user({error, Message}, _User, Req, State) when erlang:is_binary(Message) ->
    bad_request(Message, Req, State);
update_user({ok, PropList}, User, Req, State) ->
    UpdatedUser = deliv_user:setvals(PropList, User),
    case deliv_user:update(UpdatedUser) of
        {ok, _NewUser} ->
            {true, Req, State};
        {error, conflict} ->
            deliv_web_utils:error_response(409, bad_request, <<"user already exists">>, Req, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Req, State)
    end.

bad_request(Msg, Req, State) ->
    deliv_web_utils:error_response(400, bad_request, Msg, Req, State).
