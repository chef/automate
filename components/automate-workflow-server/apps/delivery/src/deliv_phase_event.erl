%% @doc Manage the lifecycle for phase_events
-module(deliv_phase_event).

-include("deliv_phase.hrl").

-export([
         new/4,
         publish/2,
         subscribe/0,
         subscribe/2
        ]).

-spec new(atom(), atom(), undefined | binary(), #deliv_phase{}) -> #phase_event{}.
new(Status, Reason, JobStatus, #deliv_phase{stage_run = Stage, phase_run = Phase,
                                            change = Change, patchset = Patchset}) ->
    #phase_event{
        phase_name = chef_utils:to_atom(deliv_phase_run:getval(phase, Phase)),
        stage_run = Stage,
        phase_run = Phase,
        change = Change,
        patchset = Patchset,
        status = Status,
        status_reason = Reason,
        job_status = JobStatus
    }.

-spec publish(atom(), #phase_event{}) -> #deliv_phase{}.
publish(Action, #phase_event{phase_name = Phase} = PhaseEvent) ->
    deliv_event:publish({{phase, Action}, Phase}, PhaseEvent).

-spec subscribe() -> boolean().
subscribe() ->
    Subs = lists:flatmap(
        fun(Phase) ->
            [{{phase, waiting_for_worker}, Phase},
             {{phase, finished}, Phase},
             {{phase, updated}, Phase},
             {{phase, started}, Phase}]
        end,
        ?PHASES
    ),
    deliv_event:subscribe(Subs).

-spec subscribe(atom(), atom()) -> boolean().
subscribe(Action, all) ->
    Subs = [{{phase, Action}, Phase} || Phase <- ?PHASES],
    deliv_event:subscribe(Subs);
subscribe(all, Phase) ->
    deliv_event:subscribe([
        {{phase, started}, Phase},
        {{phase, updated}, Phase},
        {{phase, finished}, Phase},
        {{phase, waiting_for_worker}, Phase}
    ]);
subscribe(Action, Phase) ->
    deliv_event:subscribe([{{phase, Action}, Phase}]).
