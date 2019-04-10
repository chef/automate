%% @doc Listen for stage events
-module(insights_stage_event_listener).

-behaviour(insights_listener).

-include_lib("delivery/include/deliv_events.hrl").
-include("insights.hrl").


%% insights_listener callbacks
-export([start_link/0,
         subscribe_to_events/0,
         handle_event/2]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    insights_listener:start_link(?MODULE).

-spec subscribe_to_events() -> boolean().
subscribe_to_events() ->
    deliv_stage:subscribe_stage_events().

-spec handle_event(atom(), term()) -> #insights_event{} | {error, unhandled_event}.
handle_event({{stage, Action}, StageName}, StageEvent) ->
    insights_listener:new_event(stage, Action, stage_handle_event(StageName, StageEvent));
handle_event(_, _) -> {error, unhandled_event}.

stage_handle_event(StageName, #stage_event{action = Action,
                                             status = Status,
                                             change = Change,
                                             scope = Scope,
                                             create_time = CreateTime
                                            }) ->
    [Ent, Org, Proj, Pipe] = deliv_scopes:'#get'([ent_name, org_name, proj_name, pipe_name], Scope),
    {[
      {<<"change_id">>,         deliv_change:getval(id, Change)},
      {<<"change_title">>,      deliv_change:getval(title, Change)},
      {<<"status">>,            Status},
      {<<"enterprise_name">>,   Ent},
      {<<"organization_name">>, Org},
      {<<"pipeline_name">>,     Pipe},
      {<<"project_name">>,      Proj},
      {<<"stage_name">>,        chef_utils:to_bin(StageName)},
      {<<"action">>,            chef_utils:to_bin(Action)},
      {<<"create_time">>,       chef_utils:format_timestamp(CreateTime)},
      {<<"submitted_at">>,      chef_utils:format_timestamp(deliv_change:getval(submitted_at, Change))},
      {<<"submitted_by">>,      deliv_change:getval(submitted_by, Change)},
      {<<"approved_by">>,       deliv_change:getval(approved_by, Change)},
      {<<"delivered_by">>,      deliv_change:getval(delivered_by, Change)}
     ]}.
