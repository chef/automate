%% @doc An SSE stream of events for a specified pipeline.

-module(deliv_hand_pipe_events).
-behaviour(deliv_web_sse).

-include("deliv_types.hrl").
-include("deliv_events.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([init/3,
         events/2,
         format_event/4]).

-mixin([{deliv_web_sse, [handle/2, info/3, terminate/3]}]).

init(_Transport, Req, State) ->
    deliv_web_sse:init(?MODULE, Req, State).

events(Req, State) ->
    {Req, State,
     lists:flatten([{orgs, crud},
                    phase_events(started),
                    phase_events(updated),
                    phase_events(finished)])}.
format_event({{phase, waiting_for_worker}, _Phase}, PhaseEvent, Req, State) ->
    EventProps = [{<<"event">>, <<"phase_await_worker">>}],
    Msg = {ejson, {lists:flatten([base_phase_msg(PhaseEvent) | EventProps])}},
    format_event(pipeline, Msg, Req, State);
format_event({{phase, started}, _Phase}, #phase_event{phase_run = PhaseRun} = PhaseEvent, Req, State) ->
    Node = deliv_phase_run:getval(build_node, PhaseRun),
    EventProps = [{<<"event">>, <<"phase_started">>},
                  {<<"build_node">>, Node}],
    Msg = {ejson, {lists:flatten([base_phase_msg(PhaseEvent) | EventProps])}},
    format_event(pipeline, Msg, Req, State);
format_event({{phase, finished}, _Phase}, #phase_event{status = skipped} = PhaseEvent, Req, State) ->
    EventProps = [{<<"event">>, <<"phase_completed">>},
                  {<<"status">>, <<"skipped">>}],
    Msg = {ejson, {lists:flatten([base_phase_msg(PhaseEvent) | EventProps])}},
    format_event(pipeline, Msg, Req, State);
format_event({{phase, finished}, _Phase}, #phase_event{status = failed, status_reason = no_workers} = PhaseEvent, Req, State) ->
    EventProps = [{<<"event">>, <<"phase_completed">>},
                  {<<"status">>, <<"failed">>},
                  {<<"reason">>, <<"no available build nodes">>}],
    Msg = {ejson, {lists:flatten([base_phase_msg(PhaseEvent) | EventProps])}},
    format_event(pipeline, Msg, Req, State);
format_event({{phase, finished}, _Phase}, #phase_event{status = failed, status_reason = job_canceled} = PhaseEvent, Req, State) ->
    EventProps = [{<<"event">>, <<"phase_completed">>},
                  {<<"status">>, <<"failed">>},
                  {<<"reason">>, <<"job canceled by user">>}],
    Msg = {ejson, {lists:flatten([base_phase_msg(PhaseEvent) | EventProps])}},
    format_event(pipeline, Msg, Req, State);
format_event({{phase, finished}, _Phase}, #phase_event{status = Status} = PhaseEvent, Req, State) ->
    EventProps = [{<<"event">>, <<"phase_completed">>},
                  {<<"status">>, chef_utils:to_bin(Status)}],
    Msg = {ejson, {lists:flatten([base_phase_msg(PhaseEvent) | EventProps])}},
    format_event(pipeline, Msg, Req, State);
format_event({{phase, updated}, _Phase}, #phase_event{job_status = JobStatus} = PhaseEvent, Req, State) ->
    EventProps = [{<<"event">>, <<"phase_push_update">>},
                  {<<"status">>, JobStatus}],
    Msg = {ejson, {lists:flatten([base_phase_msg(PhaseEvent) | EventProps])}},
    format_event(pipeline, Msg, Req, State);
format_event(EventKey, Msg, Req, State) ->
    %% TODO: in the future, this should be an actual event_id
    %% that clients can use to ask for events that happened
    %% since a given event; for now, we just stub this
    Id = chef_utils:random_hex_string(16),
    IOData = deliv_web_sse:format_event(Id, event_key_to_bin(EventKey), Msg),
    {keep_open, Req, State, IOData}.

event_key_to_bin({orgs, crud}) -> <<"orgs_crud">>;
event_key_to_bin(pipeline) -> <<"pipeline">>.

%% @doc return a list of all the phase event subscription keys for a given action
phase_events(Action) ->
    [{{phase, Action}, unit},
     {{phase, Action}, lint},
     {{phase, Action}, syntax},
     {{phase, Action}, quality},
     {{phase, Action}, security},
     {{phase, Action}, publish},
     {{phase, Action}, provision},
     {{phase, Action}, deploy},
     {{phase, Action}, smoke},
     {{phase, Action}, functional}].

base_phase_msg(#phase_event{phase_run = PhaseRun, stage_run = StageRun}) ->
    ChangeId = deliv_stage_run:getval(change_id, StageRun),
    [EntName, OrgName, ProjName, PipeName] = deliv_change:scoping_names(ChangeId),

    [{<<"event_type">>, <<"pipeline">>},
     {<<"enterprise">>, EntName},
     {<<"organization">>, OrgName},
     {<<"project">>, ProjName},
     {<<"pipeline">>, PipeName},
     {<<"change_id">>, ChangeId},
     {<<"stage">>, deliv_stage_run:getval(stage, StageRun)},
     {<<"stage_run_id">>, deliv_stage_run:getval(id, StageRun)},
     {<<"phase">>, deliv_phase_run:getval(phase, PhaseRun)},
     {<<"phase_run_id">>, deliv_phase_run:getval(id, PhaseRun)},
     %% TODO: this created_at field should either be properly stored in the DB,
     %% or never returned
     {<<"created_at">>, cowboy_clock:rfc1123()}].
