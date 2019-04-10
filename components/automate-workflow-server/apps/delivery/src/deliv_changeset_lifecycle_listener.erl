%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-

-module(deliv_changeset_lifecycle_listener).

-behaviour(gen_server).

-include("deliv_types.hrl").
-include("deliv_events.hrl").

%% Public API
-export([
         start_link/0
        ]).

%% Behaviour callbacks
-export([
         init/1,

         handle_call/3,
         handle_cast/2,
         handle_info/2,

         terminate/2,
         code_change/3
        ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Events = [{{stage,started}, acceptance}],
    true = deliv_event:subscribe(Events),
    {ok, stateless}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({_Pid, {{stage,started}, acceptance}, #stage_event{change=Change, scope=Scope}}, State) ->
    {ok, Dependencies} = deliv_proj_config:get_dependency_ids(Scope, Change),
    {ok, _Changeset} = deliv_changeset:add_to_changeset(Change, Dependencies),
    noreply(State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-ifdef(TEST).
noreply(State) ->
    deliv_event:publish(deliv_changeset_lifecycle_listener, stage_started_processed),
    {noreply, State}.
-else.
noreply(State) ->
    {noreply, State}.
-endif.
