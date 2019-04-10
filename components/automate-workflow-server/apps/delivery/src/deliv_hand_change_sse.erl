%% @doc An SSE stream of events for a specified change

-module(deliv_hand_change_sse).
-behaviour(deliv_web_sse).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([init/3,
         events/2,
         format_event/4,
         forbidden_callback/0]).

-mixin([{deliv_web_sse, [handle/2, info/3, terminate/3]}]).

%% special authZ callback for change-related endpoints
forbidden_callback() -> forbidden_for_change_action.

init(_Transport, Req, State) ->
    deliv_web_sse:init(?MODULE, Req, State).

events(Req, State) ->
    {ChangeId, Req1} = cowboy_req:binding(change_id, Req),
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),
    PipelineId = deliv_change:getval(pipeline_id, Change),
    {Req1, State, [{build_event_for_change, ChangeId},
                   {change_deleted, ChangeId},
                   {change_updated, ChangeId},
                   {phase_run_updated, ChangeId},
                   {pipeline_blocked, PipelineId},
                   {change_superseded, ChangeId}]}.

format_event({change_updated, ChangeId}, Change, Req,
        #handler{ent_name=EntName, user_name=UserName} = State) ->
    ChangeId = deliv_change:getval(id, Change),
    %% matching `EntName' and `ChangeId' here, binding the 2 others
    {Req1, EntName, OrgName, ProjName, ChangeId} = deliv_web_utils:extract_scoping_names(Req),
    build_event_change_ejson(EntName, OrgName, ProjName, UserName, ChangeId, Change, Req1, State);
format_event({build_event_for_change, ChangeId}, _Msg, Req,
        #handler{ent_name=EntName, user_name=UserName} = State) ->
    %% matching `EntName' and `ChangeId' here, binding the 2 others
    {Req1, EntName, OrgName, ProjName, ChangeId} = deliv_web_utils:extract_scoping_names(Req),
    build_event_change_ejson(EntName, OrgName, ProjName, UserName, ChangeId, ignored, Req1, State);
format_event({phase_run_updated, _ChangeId}, PhaseRun, Req, State) ->
    Id = chef_utils:random_hex_string(16),
    IOData = deliv_web_sse:format_event(Id, <<"phase_run_updated">>, {iodata, PhaseRun}),
    {keep_open, Req, State, IOData};
format_event({change_deleted, _ChangeId}, {deleted_by, Deleter}, Req, State) ->
    DeleterName = deliv_user:getval(name, Deleter),
    Ejson = {[{<<"deleted_by">>, DeleterName}]},
    IOData = do_format_event(<<"change_deleted">>, Ejson),
    {close, Req, State, IOData};
format_event({pipeline_blocked, _PipelineId}, Status, Req, State) ->
    PromotionStatus = case Status of
        false -> approved;
        true -> blocked
    end,
    IOData = promotion_status_event_iodata(PromotionStatus),
    {keep_open, Req, State, IOData};
format_event({change_superseded, _ChangeId}, _Change, Req, State) ->
    IOData = promotion_status_event_iodata(superseded),
    {close, Req, State, IOData}.

do_format_event(EventType, Ejson) ->
    %% same idea as for `deliv_hand_pipe_events': later, this should
    %% be meaningful IDs. For now, we stub
    Id = chef_utils:random_hex_string(16),
    deliv_web_sse:format_event(Id, EventType, {ejson, Ejson}).

promotion_status_event_iodata(Status) ->
    Ejson = {[
        {<<"promotion">>, deliv_change_common:promotion_status_ejson(Status)}
    ]},
    do_format_event(<<"promotion_status_update">>, Ejson).

build_event_change_ejson(EntName, OrgName, ProjName, UserName, ChangeId, Change, Req, State) ->
    %% best effort: we try to get the JSON, but if we fail for whatever reason,
    %% we keep going; might sound crappy, but will avoid not notifying the user
    %% when the change gets deleted right when a new build event gets in
    try
        {ok, Ejson} = deliv_change_common:get_change_as_ejson(EntName, OrgName,
                                                              ProjName, UserName, ChangeId),
        IOData = do_format_event(<<"build_event">>, Ejson),
        {keep_open, Req, State, IOData}
    catch Exception:Reason ->
        chef_log:error("failed_call={~p:format_event, [{change_updated, ~p}]}, reason=~p exception=~p", [?MODULE,Change, Reason, Exception]),
        chef_log:error("Caught an exception while trying to generate change ~s's JSON: ~p:~p",
                        [ChangeId, Exception, Reason]),
        {keep_open, Req, State, no_data}
    end.
