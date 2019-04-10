%% @doc gen_server to run one push-job
%% useful doc at https://docs.getchef.com/push_jobs.html#jobs-id
-module(deliv_push_job).

-include("deliverance_types.hrl").

-behaviour(gen_server).
-export([
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         start_link/3,
         terminate/2
        ]).

-export([
         start/1,
         status/1,
         stop/1
        ]).

-ifdef(TEST).
-export([
         find_node/1
        ]).
-endif.

%% This is only used for testing at present, but needs to be a real
%% export since otherwise we'll get an unused function warning for
%% non-test compiles. The tests should change to not need access to
%% the record; this was added as a minimal change to port the code and
%% get the tests to pass.
-export([
         example_state/0
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

%% the maximum number of times a 'status' request can fail
%% consecutively for a given push-jobs
-define(MAX_CONSECUTIVE_STATUS_ERRORS, 3).

%% the amount of time to spend waiting for a worker and then waiting for the
%% job to finish (in milliseconds).
-define(TIMEOUT, 1000 * delivery_app:get_env(push_jobs_overall_timeout)).

%% User facing

start(Pid) ->
    gen_server:call(Pid, start, ?TIMEOUT).

status(Pid) ->
    gen_server:call(Pid, status, ?TIMEOUT).

%% @doc Dispatched from a deliv_phase - its job is to fire up
%% deliv_chef_api calls and monitor the results, updating the
%% deliv_phase fsm process that started it. It won't have a name,
%% since we would have to auto-generate one, and we'll get at them
%% through their supervisor.
-spec start_link(pid(), string(), binary()) -> {ok, pid()}.
start_link(PhasePid, NodeSearchQuery, Cmd) ->
    gen_server:start_link(?MODULE,
                          #state{phase_pid=PhasePid,
                                 node_search_query=NodeSearchQuery,
                                 command=Cmd},
                          []).

stop(Pid) ->
    gen_server:call(Pid, stop, ?TIMEOUT).

init(State) -> {ok, State}.

handle_call(start, _From, State) ->
    {ok, NewState} = do_start(State, State#state.node_search_query),
    {reply, ok, NewState};
handle_call(status, _From, State) ->
    case do_status(State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason, NewState} -> {stop, normal, Reason, NewState}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Args, From, State) ->
    chef_log:error("Bad Call!~nArgs: ~p~nFrom: ~p~nState: ~p~n",
                    [Args, From, State]),
    {reply, {error, "unknown call"}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(start, State) ->
    {ok, NewState} = do_start(State, State#state.node_search_query),
    {noreply, NewState};
handle_info(status, State) ->
    case do_status(State) of
        {ok, NewState} -> {noreply, NewState};
        {error, Reason, NewState} -> {stop, Reason, NewState}
    end;
handle_info(Msg, State) ->
    chef_log:error("Unexpected message: ~p~nState was: ~p~n", [Msg, State]),
    {noreply, State}.

terminate(normal, _State) ->
    ok;
terminate(timeout, _State) ->
    chef_log:error("Push job terminated due to timeout."),
    ok;
terminate(Reason, _State) ->
    chef_log:error("Push job terminated due to unknown: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec do_status(#state{}) -> {ok, NewState} | {error, Reason, NewState} when
      NewState :: #state{},
      Reason :: {too_many_consecutive_errors, LastErrorResponse :: tuple()}.
do_status(#state{status=PushStatus} = State) ->
    JobId = PushStatus#push_job_status.id,
    do_status(State, JobId).

do_status(State, JobId) ->
    handle_push_job_status(deliv_chef_api:push_job_status(JobId), State, JobId).

interrogate_quorum_failed(JobId, [{<<"nacked">>, [NodeName]}]) ->
    chef_log:debug("Push job ~p nacked on node ~s. "
                    "Will Retry.",
                    [JobId, NodeName]),
    start;
interrogate_quorum_failed(JobId, [{<<"unavailable">>, [NodeName]}]) ->
    chef_log:debug("Push job ~p unavailable on node ~s. "
                    "Will Retry.",
                    [JobId, NodeName]),
    restart;
interrogate_quorum_failed(JobId, [{UnhandledStatus, [NodeName]}]) ->
    chef_log:error("Push job ~p not nacked on node ~s; unhandled status: ~p."
                    "Will Not Retry.",
                    [JobId, NodeName, UnhandledStatus]),
    finished;
interrogate_quorum_failed(JobId, Other) ->
    chef_log:error("Push job ~p unhandled push jobs status 'nodes': ~p", [JobId, Other]),
    erlang:exit({unhandled_status, Other}).

handle_push_job_status({ok, Status}, State, JobId) ->
    StatusName = Status#push_job_status.status,
    chef_log:debug("Status for push job ~p : ~s",
                    [Status#push_job_status.id, StatusName]),
    NextAction = case StatusName of
                     RunningOrVoting when RunningOrVoting =:= <<"running">>
                                          orelse RunningOrVoting =:= <<"voting">>
                                          orelse RunningOrVoting =:= <<"new">> ->
                         status;
                     <<"quorum_failed">> ->
                         interrogate_quorum_failed(JobId, Status#push_job_status.nodes);
                     ErrorState when ErrorState =:= <<"crashed">>
                                     orelse ErrorState =:= <<"timed_out">>
                                     orelse ErrorState =:= <<"aborted">> ->
                         chef_log:error("Push job ~p has ~s.", [JobId, ErrorState]),
                         finished;
                     <<"complete">> ->
                         chef_log:info("Push job ~p has completed.", [JobId]),
                         finished;
                     UnhandledStatus ->
                         %% If we ever hit this, that means that the Push Jobs API has chagned
                         %% out from underneath us.
                         chef_log:error("Push job ~p has received unhandled status ~p", [UnhandledStatus]),
                         finished
                 end,

    deliv_phase:update(State#state.phase_pid, Status),
    NewState = do_next_action(State, NextAction),

    {ok, NewState#state{status=Status,
                        consecutive_status_errors=0}};
handle_push_job_status({error, ErrorResponse}, #state{consecutive_status_errors=?MAX_CONSECUTIVE_STATUS_ERRORS,
                                                      phase_pid = PhasePid} = State, JobId) ->
    %% we failed to get the job's status too many times in a row, give up
    chef_log:error("Failed to get status for push-job ~p too many times, aborting; error : ~p",
                    [JobId, ErrorResponse]),
    deliv_phase:finished(PhasePid),
    {error, {too_many_consecutive_errors, ErrorResponse}, State#state{finished=true}};
handle_push_job_status({error, _ErrorResponse}, #state{consecutive_status_errors=ConsecutiveErrors} = State, _JobId) ->
    Interval = delivery_app:get_env(push_jobs_retry_interval),
    erlang:send_after(Interval, self(), status),
    {ok, State#state{consecutive_status_errors=ConsecutiveErrors + 1}}.


do_next_action(State, finished) ->
    deliv_phase:finished(State#state.phase_pid),
    State#state{finished=true};
do_next_action(State, NextAction) ->
  MaxRestartTries = delivery_app:get_env(push_jobs_max_retries),
  {Command, IntervalConfigKey, Count} = compute_restart_state(NextAction, State#state.times_started),
  case Count of
    Count when Count < MaxRestartTries ->
      NewState = State#state{times_started=Count},
      Interval = delivery_app:get_env(IntervalConfigKey),
      erlang:send_after(Interval, self(), Command),
      NewState;
    Count ->
      NewState = State#state{finished=true},
      chef_log:error("Phase PID ~p attempted to restart max ~p times, aborting;", [State#state.phase_pid, Count]),
      NewState
  end.


compute_restart_state(status, StartCount) ->
    {status, push_jobs_status_interval, StartCount};
compute_restart_state(start, _StartCount) ->
    {start, push_jobs_retry_interval, 1};
compute_restart_state(restart, StartCount) ->
    {start, push_jobs_retry_interval, StartCount+1}.



%% @doc Start the command via push-jobs
do_start(State, NodeSearchQuery) ->
    case find_node(NodeSearchQuery) of
        no_node_found ->
            NewState = State#state{finished=true},
            deliv_phase:no_worker(NewState#state.phase_pid),
            {ok, NewState};
        {ok, JobNode} ->
            NewState = State#state{node_name=JobNode},
            {ok, JobId} = deliv_chef_api:push_job_start(NewState#state.command,
                                                        [JobNode], 1),
            chef_log:debug("Created push job ~p on node ~s~n", [JobId, JobNode]),
            do_status(NewState, JobId)
    end.

-spec find_node(string()) -> no_node_found | {ok, binary()}.
find_node(NodeSearchQuery) ->
    {ok, SearchResult} = deliv_chef_api:search("node", NodeSearchQuery,
                                               <<"{\"name\":[\"name\"]}">>),
    {ok, StatusResult} = deliv_chef_api:push_node_states(),

    SearchNodes = lists:map(
                    fun({NodePlist}) ->
                            {Node_Data} = proplists:get_value(<<"data">>,
                                                              NodePlist),
                            proplists:get_value(<<"name">>, Node_Data)
                    end,
                    SearchResult#search.rows),
    chef_log:debug("SearchNodes: ~p.", [SearchNodes]),

    StatusNodes = lists:foldl(
                    fun({NodePlist}, Acc) ->
                            case proplists:get_value(<<"availability">>,
                                                     NodePlist) of
                                <<"available">> ->
                                    Name = proplists:get_value(<<"node_name">>,
                                                               NodePlist),
                                    [Name | Acc];
                                _ ->
                                    Acc
                            end
                    end, [], StatusResult),
    chef_log:debug("StatusNodes: ~p.", [StatusNodes]),
    AvailableNodes = sets:to_list(
                       sets:intersection(sets:from_list(SearchNodes),
                                         sets:from_list(StatusNodes))),
    chef_log:debug("AvailableNodes: ~p.", [AvailableNodes]),
    random_elt(AvailableNodes).

%% @private
%% @doc Return a random node from a given list.
%% Returns `no_node_found' if fed an empty list.
-spec random_elt([binary()]) -> no_node_found | {ok, binary()}.
random_elt([]) ->
    no_node_found;
random_elt([Elt]) ->
    {ok, Elt};
random_elt(List) ->
    Idx = crypto:rand_uniform(1, length(List) + 1),
    {ok, lists:nth(Idx, List)}.

example_state() ->
    #state{phase_pid = spawn(fun() -> ok end),
           node_name = <<"alice">>,
           node_search_query = <<"deliverance">>,
           quorum = 1,
           status = undefined,
           finished = false}.
