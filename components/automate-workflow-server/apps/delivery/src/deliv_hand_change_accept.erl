-module(deliv_hand_change_accept).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include("deliv_types.hrl").

-export([
         init/3,
         rest_init/2,
         handle/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [{forbidden_for_change_action/2, forbidden}]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

handle(Req, #handler{user_name=UserName,
                     ent_name=EntName}=State) ->
    %% Note: All consistency and scope checks are done with the custom authz mixin
    {ChangeId, Req1} = cowboy_req:binding(change_id, Req),
    %% TODO: I've already made the remark somewhere else, but we really should
    %% keep the whole user in the handler's state, not just the username
    {ok, Accepter} = deliv_user:fetch(EntName, UserName),
    case deliv_change:accept(ChangeId, Accepter) of
        ok ->
            chef_log:info("ChangeId ~p accept request by ~p successful",
                           [ChangeId, UserName]),
            {true, Req1, State};
        {error, change_not_found} ->
            chef_log:debug("ChangeId ~p accept request by ~p not found",
                            [ChangeId, UserName]),
            deliv_web_utils:error_response(404, change_not_found, Req1, State);
        {error, invalid_config}  ->
            chef_log:debug("ChangeId ~p accept request by ~p failed, "
                            "Invalid config.", [ChangeId, UserName]),
            deliv_web_utils:error_response(404, invalid_config, Req1, State);
        {error, invalid_state} ->
            chef_log:log(debug, "Change is in wrong state to trigger next stage"),
            deliv_web_utils:error_response(412, precondition_failed, Req, State);
        {error, Reason} ->
            chef_log:error("ChangeId ~p accept request by ~p failed, "
                            "reason: ~p.", [ChangeId, UserName, Reason]),
            deliv_web_utils:error_response(500, internal_server_error, Req1, State)
    end.
