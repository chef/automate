-module(audit_subscriptions).

-behaviour(gen_server).

%% API functions
-export([
         start_link/0,
         audit_log/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_events.hrl").
-include("audit_events.hrl").

-record(state, {cap, events=[]}).

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec audit_log() -> [#audit_stage_event{}].
audit_log() ->
    gen_server:call(?SERVER, get_audit_list).

-spec init([]) -> {ok, #state{}}.
init([]) ->
    deliv_stage:subscribe_stage_events(),
    {ok, MaxEventsInMemory} = application:get_env(audit, max_events_in_memory),
    Events = audit_stage_event_db:fetch_audit_events(MaxEventsInMemory),
    {ok, #state{cap=MaxEventsInMemory, events = Events}}.

handle_call(get_audit_list, _From, #state{events = Events} = State) ->
    {reply, Events, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_Pid, {{stage, _}, StageName}, StageEvent}, #state{events = Events, cap = Cap} = State) ->
    AuditEvent = build_audit_event(StageName, StageEvent),
    audit_stage_event_db:insert(AuditEvent),

    audit_logger:log(AuditEvent),
    {noreply, State#state{events=lists:sublist([ AuditEvent | Events ], 1, Cap)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

build_audit_event(StageName,
                  #stage_event{
                     action = Action,
                     status = Status,
                     change = Change,
                     scope = Scope,
                     create_time = CreateTime
                    }) ->
    [Ent, Org, Proj, Pipe | _ ] = deliv_scopes:'#get'([ent_name, org_name, proj_name, pipe_name], Scope),
    #audit_stage_event{
       action = Action,
       create_time = CreateTime,
       status = Status,
       change_id = deliv_change:getval(id, Change),
       change_title = deliv_change:getval(title, Change),
       ent = Ent,
       org = Org,
       pipe = Pipe,
       proj = Proj,
       stage_name = StageName,
       submitted_at = deliv_change:getval(submitted_at, Change),
       submitted_by = deliv_change:getval(submitted_by, Change),
       approved_by = deliv_change:getval(approved_by, Change),
       delivered_by = deliv_change:getval(delivered_by, Change)
      }.
