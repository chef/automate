-module(audit_logger).

-export([log/1]).

-include_lib("delivery/include/deliv_events.hrl").
-include_lib("delivery/include/deliv_types.hrl").
-include("audit_events.hrl").

-spec log(#audit_stage_event{}) -> any().
log(#audit_stage_event{stage_name = StageName, action = Action} = AuditStageEvent) ->
    chef_log:info("audit_log stage_name=~s; action=stage_~s; event=~p; ", [Action, StageName, AuditStageEvent]).
