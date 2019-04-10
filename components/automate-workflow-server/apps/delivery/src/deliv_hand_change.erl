-module(deliv_hand_change).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         to_json/2,
         delete_resource/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [{forbidden_for_change_action/2, forbidden}]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

delete_resource(Req, #handler{ent_name=EntName, user_name=UserName}=State) ->
    {ChangeId, Req1} = cowboy_req:binding(change_id, Req),

    %% TODO: I've already made the remark somewhere else, but we really should
    %% keep the whole user in the handler's state, not just the username
    {ok, Deleter} = deliv_user:fetch(EntName, UserName),
    case deliv_change:delete_change_and_branch(ChangeId, Deleter) of
        ok ->
            chef_log:info("Successfully deleted Change Id ~s, enterprise ~s, request by ~s.",
                           [ChangeId, EntName, UserName]),
            {true, Req1, State};
        {error, change_not_found} ->
            chef_log:error("ChangeId ~s delete request by ~s not found",
                            [ChangeId, UserName]),
            deliv_web_utils:error_response(404, change_not_found, Req1, State);
        {error, merged_change} ->
            chef_log:error("ChangeId ~s delete request by ~s rejected, change already merged.",
                            [ChangeId, UserName]),
            deliv_web_utils:error_response(409, merged_change, Req1, State);
        {error, Reason} ->
            chef_log:error("ChangeId ~s delete request by ~s failed, "
                            "reason: ~p.", [ChangeId, UserName, Reason]),
            deliv_web_utils:error_response(500, internal_server_error, Req1, State)
    end.

to_json(Req, #handler{ent_name = EntName, user_name = UserName} = State) ->
    {OrgName, Req1}  = cowboy_req:binding(org_name, Req),
    {ProjName, Req2} = cowboy_req:binding(proj_name, Req1),
    {ChangeId, Req3} = cowboy_req:binding(change_id, Req2),

    case deliv_change_common:get_change_as_ejson(EntName, OrgName, ProjName, UserName, ChangeId) of
        {ok, Ejson} ->
            deliv_web_utils:content(Ejson, Req3, State);
        {error, not_found} ->
            deliv_web_utils:error_response(404, not_found, <<>>, Req3, State);
        {error, Reason} ->
            chef_log:log(error, "Could not retrieve change ~s: ~p", [ChangeId, Reason]),
            deliv_web_utils:error_response(500, system_error, <<>>, Req3, State)
    end.
