-module(deliv_hand_phase_runs_named).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         handle/2
        ]).

-include("deliv_types.hrl").

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

handle(Req, State) ->
    case do_handle(Req) of
        {ok, Req1, Return} ->
            {Return, Req1, State};
        {error, Req1, {ErrCode, ErrIoList}} ->
            ErrMsg = erlang:iolist_to_binary(ErrIoList),
            deliv_web_utils:error_response(ErrCode, ErrMsg, Req1, State)
    end.

do_handle(Req) ->
    {PhaseRunIdBin, Req1} = cowboy_req:binding(run_id, Req),
    case chef_utils:to_int(PhaseRunIdBin) of
        {error, not_an_int} ->
            {error, Req1, {400, "phase run id should be an int"}};
        PhaseRunId ->
            do_handle(Req1, deliv_phase_run:fetch(PhaseRunId))
    end.

do_handle(Req, {error, not_found}) ->
    {error, Req, {404, "phase run not found"}};
do_handle(Req, {ok, PhaseRun}) ->
    StageRunId = deliv_phase_run:getval(stage_run_id, PhaseRun),
    {ok, StageRun} = deliv_stage_run:fetch(StageRunId),
    ChangeId = deliv_stage_run:getval(change_id, StageRun),
    do_handle(cowboy_req:method(Req), PhaseRun, ChangeId).

do_handle({<<"POST">>, Req}, PhaseRun, ChangeId) ->
    handle_parse_request(deliv_web_utils:parse_json_req(Req, phase_run), PhaseRun, ChangeId);
do_handle({<<"GET">>, Req}, PhaseRun, _ChangeId) ->
    Json = chef_json:record_to_json(PhaseRun),
    {ok, Req, chef_json:encode(Json)}.

handle_parse_request({{error, _Why}, Req}, PhaseRun, ChangeId) ->
    PhaseId = deliv_phase_run:getval(id, PhaseRun),
    deliv_event:publish({phase_run, PhaseId}, failed),
    chef_log:failed_call(?MODULE, handle_parse_request, [Req, PhaseRun, ChangeId], invalid_json),
    {error, Req, {400, "invalid json"}};
handle_parse_request({Json, Req}, PhaseRun, ChangeId) ->
    Vals = deliv_hand_phase_runs_named_deserializer:from_json(Json),

    %% TODO: determine if we depend on the value that comes out of
    %% deliv_phase_run:update - add tests here that actually modify
    %% the deliv_phase_run record so that they represent the way the
    %% code works
    UpdatedPhaseRun0 = deliv_phase_run:setvals(Vals, PhaseRun),
    {ok, UpdatedPhaseRun} = deliv_phase_run:update(UpdatedPhaseRun0),

    Finished = deliv_phase_run:getval(finished, UpdatedPhaseRun),
    PhaseId = deliv_phase_run:getval(id, UpdatedPhaseRun),
    Req2 = case Finished of
               false ->
                   deliv_event:publish({phase_run, PhaseId}, updated),
                   Req;
               _ ->
                   case deliv_phase_run:getval(run_success, UpdatedPhaseRun) of
                       false ->
                           deliv_event:publish({phase_run, PhaseId}, failed),
                           chef_log:error("Phase ~p failed.", [PhaseId]);
                       _ ->
                           deliv_event:publish({phase_run, PhaseId}, passed),
                           chef_log:debug("Phase ~p completed successfully.", [PhaseId])
                   end,
                   case revoke_token_if_builder(Req) of
                       {_Error, Req1} ->
                           Req1;
                       Req1 ->
                           Req1
                   end
           end,
    UpdatedPhaseRunEjson = chef_json:record_to_json(UpdatedPhaseRun),
    UpdatedPhaseRunJsonBin = chef_json:encode(UpdatedPhaseRunEjson),
    publish(Req, ChangeId, UpdatedPhaseRunEjson),
    Req3 = cowboy_req:set_resp_body(UpdatedPhaseRunJsonBin, Req2),
    {ok, Req3, true}.

%% @doc If a run is finished, we'll want to revoke the builder user's
%% job-specific token (because why keep it around?). This token will
%% be the same one sent in the headers of this very request.
%%
%% However, since we're also allowing non-builder admins to use this
%% endpoint, we don't want to accidentally revoke their token. We can
%% do this by taking advantage of the fact that currently, all
%% enterprises have a single builder user, and that its name is
%% "builder". If our request is from that user, it's getting its token revoked!
%%
%% Note that this leaves untouched any other tokens the builder user
%% currently has. It would be very bad for the first builder to finish
%% to revoke the tokens of all other in-flight jobs, after all.
revoke_token_if_builder(Req) ->
    {EntName, UserName, Token, Req1} = deliv_token:extract_token_info(Req),
    case UserName of
        ?BUILDER_NAME ->
            Result = deliv_user:revoke_token(EntName, UserName, Token),
            {Result, Req1};
        _ ->
            %% Not a builder, don't revoke!
            Req1
    end.

publish(Req, ChangeId, UpdatedPhaseRunEjson) ->
    PhaseId = ej:get({<<"id">>}, UpdatedPhaseRunEjson),
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),
    {OrgName, Req2} = cowboy_req:binding(org_name, Req1),
    {ProjName, Req3} = cowboy_req:binding(proj_name, Req2),
    {PipeName, _Req4} = cowboy_req:binding(pipe_name, Req3),
    Href = deliv_web_utils:relative_href_for(phase_run, [EntName, OrgName, ProjName, PipeName, PhaseId]),
    UpdatedPhaseRunJsonBin = chef_json:encode(ej:set({<<"href">>}, UpdatedPhaseRunEjson, Href)),
    deliv_event:publish({phase_run_updated, ChangeId}, UpdatedPhaseRunJsonBin).
