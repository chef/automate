%% @doc Module for human consumable communication between delivery and github
%%      It includes setting status as well as communication through comments
-module(deliv_github_status).

-include_lib("delivery/include/deliv_types.hrl").

-export([sync_status/2]).

-spec sync_status(d_common_scope(), binary()) -> ok.
sync_status(Scope, ChangeId) ->
    {ok, Patchset} = deliv_patchset:latest_patchset_for_change(ChangeId),
    PatchsetId = deliv_patchset:getval(id, Patchset),

    [GHPatchset] = deliv_github_patchset:fetch_by_patchset_id(PatchsetId),
    GHPayloadJson = chef_json:decode(deliv_github_patchset:getval(payload, GHPatchset)),
    Sha = deliv_github_pull_request:commit_sha(GHPayloadJson),

    {ok, PhaseRunSummaries} = deliv_change:get_phase_run_summary(ChangeId),
    process_phase_run_summaries(Scope, Sha, PhaseRunSummaries).

%% @private
%% @doc Processes the list of phase run summaries updating the status on the
%%      commit in github.
-spec process_phase_run_summaries(d_common_scope(), binary(), [tuple()]) ->
                                                         ok | {error, binary()}.
process_phase_run_summaries(_, _, []) -> ok;
process_phase_run_summaries(Scope, Sha,
                            [#phase_run_summary{stage = Stage,
                                                phase = Phase,
                                                phase_status = Status} | T]) ->
    [ChangeId, [EntName, OrgName, ProjName, _Pipe]] =
                         deliv_scopes:'#get'([change_id, scoping_names], Scope),
    State = state_for_delivery_status(Status),
    Context = <<"chef_delivery/", Stage/binary, "/", Phase/binary>>,
    ChangeUrl = deliv_web_utils:make_web_url_for_change(EntName, OrgName, ProjName, ChangeId),
    CallbackUrl = <<ChangeUrl/binary, "/status/", Stage/binary>>,
    case deliv_github_api:set_pull_request_status(Scope, Sha, State,
                                                  <<"Delivery phase ", Status/binary>>,
                                                  CallbackUrl, Context) of
        {error, Why} = Err ->
            chef_log:info("set_status on github failed for change_id ~p with error ~p",
                            [ChangeId, Why]),
            Err;
        _ ->
            process_phase_run_summaries(Scope, Sha, T),
            ok
    end.

%% @private
%% @doc returns the correct github status state for given a delivery status.
-spec state_for_delivery_status(binary()) -> atom().
state_for_delivery_status(<<"passed">>) -> success;
state_for_delivery_status(<<"failed">>) -> failure;
state_for_delivery_status(<<"idle">>) -> pending;
state_for_delivery_status(<<"running">>) -> pending;
state_for_delivery_status(<<"skipped">>) -> success;
state_for_delivery_status(<<"skipping">>) -> pending.
