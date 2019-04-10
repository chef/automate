-module(deliv_hand_change_merge).
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

handle(Req, #handler{ent_name=EntName,
                     user_name=UserName} = State) ->
    %% Note: All consistency and scope checks are done with the custon authz mixin
    {ChangeId, Req1} = cowboy_req:binding(change_id, Req),
    %% TODO: I've already made the remark somewhere else, but we really should
    %% keep the whole user in the handler's state, not just the username
    %% TODO: We should probably handle error cases here.
    {ok, Merger} = deliv_user:fetch(EntName, UserName),
    handle_merge(deliv_change:merge(ChangeId, Merger), Req1, State).

handle_merge(ok, Req, State) ->
    {true, Req, State};
handle_merge({error, change_not_found}, Req, State) ->
    chef_log:log(debug, "Change not found"),
    deliv_web_utils:error_response(404, change_not_found, Req, State);
handle_merge({error, patchset_already_merged}, Req, State) ->
    chef_log:log(debug, "Change Already Merged"),
    deliv_web_utils:error_response(412, precondition_failed, Req, State);
handle_merge({error, pipeline_branch_missing}, Req, State) ->
    chef_log:log(debug, "Pipeline Branch Does Not Exist"),
    deliv_web_utils:error_response(412, precondition_failed, Req, State);
handle_merge({error, feature_branch_merge_failed}, Req, State) ->
    chef_log:log(debug, "Merge Is Not Fast-Forward"),
    ErrorMessage = <<"Automatic merge failed. Fix merge conflicts and resubmit for review.">>,
    deliv_web_utils:error_response(412, precondition_failed, ErrorMessage, Req, State);
handle_merge({error, feature_branch_delete_failed}, Req, State) ->
    chef_log:log(debug, "Could Not Delete Review Branches"),
    deliv_web_utils:error_response(500, internal_server_error, Req, State);
handle_merge({error, update_patchset_failed}, Req, State) ->
    chef_log:log(debug, "Could Not Update Patchset"),
    deliv_web_utils:error_response(500, internal_server_error, Req, State);
handle_merge({error, update_change_failed}, Req, State) ->
    chef_log:log(debug, "Could Not Update Change"),
    deliv_web_utils:error_response(500, internal_server_error, Req, State).
