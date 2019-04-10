%% @doc Listen for phase events
-module(insights_phase_event_listener).

-include_lib("delivery/include/deliv_events.hrl").
-include("insights.hrl").

-export([start_link/0,
         subscribe_to_events/0,
         handle_event/2]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    insights_listener:start_link(?MODULE).

-spec subscribe_to_events() -> boolean().
subscribe_to_events() ->
    deliv_phase:subscribe_phase_events().

-spec handle_event(atom(), term()) -> #insights_event{} | {error, unhandled_event}.
handle_event({{phase, Action}, Phase}, PhaseEvent) ->
    insights_listener:new_event(phase, Action, phase_ejson(Phase, PhaseEvent));
handle_event(_, _) -> {error, unhandled_event}.

phase_ejson(PhaseAtom, #phase_event{phase_run = PhaseRun,
                                    change = Change,
                                    stage_run = StageRun,
                                    status = Status,
                                    status_reason = StatusReason,
                                    job_status = JobStatus}) ->
    ChangeId = deliv_change:getval(id, Change),
    StageName = deliv_stage_run:getval(stage, StageRun),
    BuildNodeName = deliv_phase_run:getval(build_node, PhaseRun),
    [EntName, OrgName, ProjName, PipeName] = deliv_change:scoping_names(ChangeId),
    {[
        {<<"change_id">>, ChangeId},
        {<<"stage">>, StageName},
        {<<"phase">>, chef_utils:to_bin(PhaseAtom)},
        {<<"status">>, chef_utils:to_bin(Status)},
        {<<"status_reason">>, chef_utils:to_bin(StatusReason)},
        {<<"enterprise_name">>, EntName},
        {<<"organization_name">>, OrgName},
        {<<"pipeline_name">>, PipeName},
        {<<"project_name">>, ProjName},
        {<<"job_status">>, JobStatus},
        {<<"build_node">>, BuildNodeName}
    ]}.
