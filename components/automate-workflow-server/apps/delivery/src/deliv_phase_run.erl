-module(deliv_phase_run).
-include("deliv_types.hrl").
-include("deliv_coordinates.hrl").

-compile({parse_transform, sqerl_gobot}).

%% DB operations
-export([
         delete/1,
         fetch/1,
         fetch_by_stage_run_id/1,
         insert/1,
         list_running/0,
         start/2,
         update/1,
         update/2,
         finish_with_log_append/2,
         finish/3
       ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-export([
         build_filename/3
       ]).

-record(deliv_phase_run, {
          id                   :: db_id(),
          stage_run_id         :: db_id(),
          phase                :: binary(),
          status               :: binary(),
          finished = false     :: boolean(),
          run_success          :: boolean(),
          run_log              :: binary(),
          run_status           :: binary(),
          build_node           :: binary(),
          search_query         :: binary(),
          search_description   :: binary(),
          description          :: binary(),
          created_at           :: calendar:datetime(),
          started_at           :: calendar:datetime(),
          finished_at          :: calendar:datetime()
        }).

'#insert_fields'() ->
    [stage_run_id, phase, status, finished, run_success, run_log,
     run_status, build_node, search_query, search_description, description].

'#update_fields'() ->
    [status, finished, finished_at, run_success, run_log, run_status,
     build_node, search_query, search_description, description].

'#statements'() ->
    [default,
     {fetch_by_stage_run_id,
      sqerl_rec:gen_fetch(?MODULE, stage_run_id)},
     {start,
      "UPDATE " ++ '#table_name'()
      ++ " SET build_node = $2, status = $3, started_at = $4 "
      "WHERE id = $1 "
      "RETURNING " ++ '#table_name'() ++ ".*"},
     {list_running,
      "SELECT build_node FROM phase_runs "
      "WHERE status = 'running'"}].

'#table_name'() ->
    "phase_runs".

-spec insert(d_phase_run() | proplist(atom(), any()))
        -> db_op_result(d_phase_run()).
insert(#deliv_phase_run{} = Rec) ->
    deliv_db:insert(Rec);
insert(PropList) when erlang:is_list(PropList)->
    insert(fromlist(PropList)).

-spec fetch(non_neg_integer()) -> db_op_single_result(d_phase_run()).
fetch(PhaseRunId) ->
    deliv_db:fetch_by_id(?MODULE, PhaseRunId).

-spec fetch_by_stage_run_id(non_neg_integer())-> db_op_result(d_phase_run()).
fetch_by_stage_run_id(StageRunId) ->
    deliv_db:qfetch(?MODULE, fetch_by_stage_run_id, [StageRunId]).

-spec list_running() -> [d_phase_run()] | {error, _}.
list_running() ->
    deliv_db:qfetch(?MODULE, list_running, []).

-spec update(d_phase_run()) -> db_op_single_result(d_phase_run()).
update(PhaseRun) ->
    deliv_db:update(PhaseRun).

-spec update(proplist(atom(), any()), d_phase_run()) ->
    db_op_single_result(d_phase_run()).
update(PropList, PhaseRun) ->
    deliv_db:update(PropList, PhaseRun).

-spec finish_with_log_append(binary(), d_phase_run()) ->
    db_op_single_result(d_phase_run()).
finish_with_log_append(Msg, #deliv_phase_run{id = PhaseRunId} = PhaseRun) ->
    case fetch(PhaseRunId) of
        {ok, #deliv_phase_run{run_log = OldLog}} ->
            FullLog = erlang:iolist_to_binary([undefined_or_default(OldLog, <<>>), <<"\n">>, Msg]),
            PropList = [{finished, true},
                        {finished_at, chef_utils:db_now()},
                        {status, <<"failed">>},
                        {run_log, FullLog}],
            deliv_db:update(PropList, PhaseRun);
        {error, Why} = Error ->
            chef_log:error("Unable to fetch deliv_phase_run with: ~p", [Why]),
            Error
    end.

-spec finish(failed | skipped, binary(), d_phase_run()) ->
    db_op_single_result(d_phase_run()).
finish(failed, Msg, PhaseRun) ->
    PropList = [{finished, true},
                {finished_at, chef_utils:db_now()},
                {status, <<"failed">>},
                {run_log, Msg}],
    update(PropList, PhaseRun);
finish(skipped, Msg, PhaseRun) ->
    PropList = [{finished, true},
                {finished_at, chef_utils:db_now()},
                {status, <<"skipped">>},
                {run_log, Msg}],
    update(PropList, PhaseRun).

-spec undefined_or_default(undefined | Type, Type) -> Type when Type :: any().
undefined_or_default(undefined, Default) ->
    Default;
undefined_or_default(Defined, _) ->
    Defined.

%% @doc Once the job has actually started, we only update the 3 fields
%% relevant to the status of the push-job itself, and no other fields,
%% since those might be concurrently updated through the API
-spec start(binary(), d_phase_run())
        -> db_op_result(d_phase_run()).
start(BuildNode, #deliv_phase_run{id = Id}) ->
    Params = [Id,
              BuildNode,
              <<"running">>,
              chef_utils:db_now()],
    deliv_db:qfetch(?MODULE, start, Params).

-spec delete(d_phase_run()) -> ok | {error, any()}.
delete(#deliv_phase_run{} = PhaseRun) ->
    deliv_db:delete(PhaseRun).

build_filename(#proj_coordinates{org_name = Org, proj_name = Proj}, Pipe, #deliv_phase_run{id = PhaseId, phase = Phase, stage_run_id = StageRunId}) ->
    {ok, StageRun} = deliv_stage_run:fetch(StageRunId),
    Stage = deliv_stage_run:getval(stage, StageRun),

    %% orgName-projName-pipeName-stageName-phaseName-phaseId.txt
    chef_utils:to_bin("~s-~s-~s-~s-~s-~p.txt", [Org, Proj, Pipe, Stage, Phase, PhaseId]).
