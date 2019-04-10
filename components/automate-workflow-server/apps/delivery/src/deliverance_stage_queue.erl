%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Adam Jacob <adam@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
%%

-module(deliverance_stage_queue).
-behaviour(gen_server).
-include("deliverance_types.hrl").
-include("deliv_types.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([in/1, queue_status/1]).

-export([do_all/1]).

in(StageData) ->
    gen_server:call(?MODULE, {in, StageData}).

-spec queue_status(binary()) -> list().
queue_status(EntName) ->
    gen_server:call(?MODULE, {queue_status, EntName}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, #deliverance_stage_queue{}, []).

init(State=#deliverance_stage_queue{}) ->
    timer:send_interval(500, all),
    {ok, State}.

handle_call({in, StageData}, _From, State) ->
    NewState = do_in(StageData, State),
    {reply, ok, NewState};
handle_call({queue_status, EntName}, _From, State) ->
    {reply, queue_status(State, EntName), State};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};
handle_call(Args, From, State) ->
    chef_log:error("Bad Call!~nArgs: ~p~nFrom: ~p~nState: ~p~n", [Args, From, State]).

handle_cast(_, State) ->
    {noreply, State}.

handle_info(all, State) ->
    NewState = do_all(State),
    {noreply, NewState};
handle_info(Msg, State) ->
    chef_log:error("Unexpected message: ~p~nState was: ~p~n", [Msg, State]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_in(StageData, State) ->
    QName = q_name(StageData),
    StageName = deliv_stage:get_stage_name(StageData),
    Change = deliv_stage:get_change(StageData),
    ChangeId = deliv_change:getval(id, Change),
    TS = {enqueue_time, ec_date:format("Y-m-d G:i:s", os:timestamp())},
    case find_queue(QName, State) of
        {ok, Queue} ->
            Q2 = queue:in({StageName, StageData, TS}, Queue),
            State#deliverance_stage_queue{queues=orddict:store(QName, Q2, State#deliverance_stage_queue.queues)};
        error ->
            chef_log:debug("Creating new queue for stage: ~p.~n", [StageName]),
            EmptyQ = queue:new(),
            chef_log:debug("Queueing Stage: ~p for ChangeId: ~p.~n", [StageName, ChangeId]),
            Queue = queue:in({StageName, StageData, TS}, EmptyQ),
            chef_log:info("Queued Stage: ~p for ChangeId: ~p.~n", [StageName, ChangeId]),
            State#deliverance_stage_queue{queues=orddict:store(QName, Queue, State#deliverance_stage_queue.queues)}
    end.

do_all(State) ->
    orddict:fold(fun(QName, _Queue, NState) ->
                    {ok, NState2} = do_out(QName, NState),
                    NState2
                 end,
                 State,
                 State#deliverance_stage_queue.queues).

do_out(QName, State) ->
    case queue_is_empty(QName, State) of
        true ->
            case queue_is_running(QName, State) of
                true ->
                    {ok, State};
                false ->
                  %% If the queue is empty and nothing is running we can clean up
                  %% the queue records to prevent a memory leak.
                  State2 = State#deliverance_stage_queue{queues=orddict:erase(QName, State#deliverance_stage_queue.queues),
                                                         running=orddict:erase(QName, State#deliverance_stage_queue.running)},
                  {ok, State2}
            end;
        false ->
            case queue_is_running(QName, State) of
                true ->
                    {ok, State};
                false ->
                    {StageType, StageData, TS, NewState} = queue_out(QName, State),
                    {ok, Pid} = deliv_stage_sup:start_stage(StageData),
                    State2 = NewState#deliverance_stage_queue{running=orddict:store(QName, {StageType, StageData, TS, Pid}, NewState#deliverance_stage_queue.running)},
                    deliv_stage:run(Pid),
                    {ok, State2}
            end
    end.

%% @doc Generate name of queue based on several properties.
q_name(StageData) ->
   StageName = deliv_stage:get_stage_name(StageData),
   Change = deliv_stage:get_change(StageData),
   ChangeId = deliv_change:getval(id, Change),
   [EntName, OrgName, ProjName, PipeName] = deliv_stage:get_scoping_names(StageData),

   BaseName = chef_utils:to_str(EntName) ++ "-" ++ chef_utils:to_str(StageName),

   case chef_utils:to_bin(StageName) of
     <<"verify">> ->
       BaseName ++ "-" ++ chef_utils:to_str(OrgName)
                ++ "-" ++ chef_utils:to_str(ProjName)
                ++ "-" ++ chef_utils:to_str(PipeName)
                ++ "-" ++ chef_utils:to_str(ChangeId);
     <<"build">> ->
       BaseName ++ "-" ++ chef_utils:to_str(OrgName)
                ++ "-" ++ chef_utils:to_str(ProjName)
                ++ "-" ++ chef_utils:to_str(PipeName);
     <<"acceptance">> ->
       BaseName ++ "-" ++ chef_utils:to_str(OrgName)
                ++ "-" ++ chef_utils:to_str(ProjName)
                ++ "-" ++ chef_utils:to_str(PipeName);
     _ ->
       BaseName
   end.

%% @doc Provide queue status information, per enterprise for use on the console
%% or with the :ent/queue_status endpoint.
-spec queue_status(term(), binary()) -> list().
queue_status(State, EntName) ->
    orddict:fold(fun(QName, {_StageType, StageData, {enqueue_time, TS} = _T, _Pid} = _Data, Acc) ->
                     case queue_is_running(QName, State) of
                         false ->
                             [build_status_response(QName, TS, StageData, "queued", EntName) | Acc];
                         true ->
                             [build_status_response(QName, TS, StageData, "running", EntName) | Acc]
                     end
                 end,
                 [],
                 State#deliverance_stage_queue.running).

%% @doc assemble the response if the request enterprise matches the queued item's enterprise
build_status_response(QName, TS, StageData, Status, EntName) ->
    Change = deliv_stage:get_change(StageData),
    ChangeId = deliv_change:getval(id, Change),
    [ChangeEntName, _, _, _] = deliv_change:scoping_names(ChangeId),
    maybe_build_response(EntName =:= ChangeEntName, Change, TS, Status, QName).

maybe_build_response(false, _Change, _TS, _Status, _QName) ->
    [];
maybe_build_response(true, Change, TS, Status, QName) ->
    ChangeId        = deliv_change:getval(id,Change),
    LatestStatus    = deliv_change:getval(latest_patchset_status, Change),
    SubmittedBy     = deliv_change:getval(submitted_by, Change),

    PhaseRunSummary = deliv_change:get_phase_run_summary(ChangeId),

    [{<<"queue_name">>,         chef_utils:to_bin(QName)},
     {<<"stage_queue_status">>, chef_utils:to_bin(Status)},
     {<<"change_id">>,          chef_utils:to_bin(ChangeId)},
     {<<"submitted_by">>,       chef_utils:to_bin(SubmittedBy)},
     {<<"latest_status">>,      chef_utils:to_bin(LatestStatus)},
     {<<"enqueue_time">>,       chef_utils:to_bin(TS)},
     {<<"phase_run_summary">>,  PhaseRunSummary}].

%% @doc determine if a job is running or not but checking it against stored
%% running list in State.
%%
%% Note: Leaving debug statements for now as we get a better understanding
%% of concurrency issues.
queue_is_running(QName, State) ->
    chef_log:debug("QIR Name: ~p, State: ~p", [QName, State]),
    chef_log:debug("QIR looking QName ~p for pid: ~p", [QName, State#deliverance_stage_queue.running]),

    case orddict:find(QName, State#deliverance_stage_queue.running) of
        {ok, {_StageType, _StageData, _TS, Pid} = _Val} ->
            chef_log:debug("QIR QName ~p is pid ~p alive? ~p", [QName, Pid, erlang:is_process_alive(Pid)]),
            erlang:is_process_alive(Pid);
        error ->
            chef_log:debug("QIR QName ~p, did not find pid", [QName]),
            false
    end.

queue_is_empty(QName, State) ->
    case find_queue(QName, State) of
        {ok, Queue} ->
            queue:is_empty(Queue);
        error ->
            false
    end.

queue_out(QName, State) ->
    {ok, Queue} = find_queue(QName, State),
    case queue:out(Queue) of
        {{value, {StageType, StageData, TS}}, Q2} ->
            NewState = State#deliverance_stage_queue{queues=orddict:store(QName, Q2, State#deliverance_stage_queue.queues)},
            {StageType, StageData, TS, NewState};
        {empty, Q2} ->
            State#deliverance_stage_queue{queues=orddict:store(QName, Q2, State#deliverance_stage_queue.queues)}
    end.

find_queue(QName, State) ->
    orddict:find(QName, State#deliverance_stage_queue.queues).
