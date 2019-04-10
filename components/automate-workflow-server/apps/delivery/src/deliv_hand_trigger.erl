-module(deliv_hand_trigger).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include("deliv_types.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         handle/2,
         init/3,
         rest_init/2
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

handle(Req, State) ->
    %% Note: All consistency and scope checks are done with the custom
    %% authz mixin for the change but we still need to check that the
    %% user can do the trigger they are attempting to because the scope
    %% is different if we are ultimately triggering union or further.
    {ChangeId, Req1}    = cowboy_req:binding(change_id, Req),
    {Stage, Req2}    = cowboy_req:binding(stage, Req1),

    handle_params(Req2, State, ChangeId, Stage).

handle_params(Req, State, ChangeId, Stage) when Stage =:= <<"verify">>;
                                                Stage =:= <<"build">>;
                                                Stage =:= <<"acceptance">>;
                                                Stage =:= <<"union">>;
                                                Stage =:= <<"rehearsal">>;
                                                Stage =:= <<"delivered">> ->
  trigger_stage_if_authorized(Req, State, ChangeId, Stage);
handle_params(Req, State, ChangeId, Stage) ->
    chef_log:error("trigger_stage:~s change_id:~s error:unsupported_stage",
                    [Stage, ChangeId]),
    deliv_web_utils:error_response(404, unsupported_stage, Req, State).

trigger_stage_if_authorized(Req,
                            #handler{user_name = UserName} = State,
                            ChangeId,
                            Stage) ->
  case authorize_stage_trigger(Stage, ChangeId, State) of
    allow ->
      trigger_stage(Stage, ChangeId, Req, State);
    forbid ->
      chef_log:debug("ChangeId ~s ~s request by ~s failed, "
                      "Unauthorized.", [ChangeId, Stage, UserName]),
      deliv_web_utils:error_response(401, unauthorized, Req, State)
  end.

trigger_stage(Stage, ChangeId, Req, #handler{user_name = UserName} = State) ->
  StageAtom = erlang:binary_to_atom(Stage, utf8),

  case deliv_change:trigger_stage(StageAtom, ChangeId) of
      ok ->
          chef_log:info("ChangeId ~p ~p request by ~p successful",
                         [ChangeId, Stage, UserName]),
          {true, Req, State};
      {error, change_not_found} ->
          chef_log:debug("ChangeId ~p ~p request by ~p not found",
                          [ChangeId, Stage, UserName]),
          deliv_web_utils:error_response(404, change_not_found, Req, State);
      {error, invalid_state} ->
          chef_log:debug("ChangeId ~p ~p request by ~p is in an invalid state.",
                          [ChangeId, Stage, UserName]),
          deliv_web_utils:error_response(412, precondition_failed, Req, State);
      {error, invalid_config}  ->
          chef_log:debug("ChangeId ~p ~p request by ~p failed, "
                          "Invalid config.", [ChangeId, Stage, UserName]),
          deliv_web_utils:error_response(404, invalid_config, Req, State);
      {error, Reason} ->
          chef_log:error("ChangeId ~p ~p request by ~p failed, "
                          "reason: ~p.", [ChangeId, Stage, UserName, Reason]),
          deliv_web_utils:error_response(500, internal_server_error, Req, State)
  end.

authorize_stage_trigger(Stage,
                        ChangeId,
                        #handler{user_name = UserName}) ->
    ChangeAction = erlang:binary_to_atom(<<"trigger_", Stage/binary>>, utf8),
    [EntName, OrgName, ProjName, Pipeline]
                                         = deliv_change:scoping_names(ChangeId),
    deliv_change_auth:authorized_change_action(EntName, OrgName, ProjName,
        Pipeline, UserName, ChangeAction).
