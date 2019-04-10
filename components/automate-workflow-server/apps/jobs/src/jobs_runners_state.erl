-module(jobs_runners_state).

-include("jobs_types.hrl").

-behaviour(gen_server).

% Public API
-export([
         start_link/0,
         fetch/2,
         list/0
        ]).

% gen_server callbacks
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

-spec init([]) -> {ok, map()}.
init([]) ->
    jobs_runner:subscribe_runner_events(),
    {ok, maps:new()}.

-spec list() -> [jobs_runner()].
list() ->
    gen_server:call(?MODULE, list).

-spec fetch(binary(), binary()) -> [jobs_runner()].
fetch(EntName, Hostname) ->
    gen_server:call(?MODULE, {fetch, EntName, Hostname}).

handle_call(list, _From, State) ->
    {reply, to_list(State), State};
handle_call({fetch, EntName, Hostname}, _From, State) ->
    Result = case maps:find({EntName, Hostname}, State) of
                 error -> [];
                 {ok, Runner} -> [Runner]
             end,
    {reply, Result, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_Pid, runner_state_changed, #runner{enterprise_name = EntName, hostname = Hostname} = Runner}, State) ->
    NewState = State#{{EntName, Hostname} => Runner},
    deliv_event:publish(runners_state_updated, to_list(NewState)),
    {noreply, NewState};
handle_info({_Pid, runner_stopped, #runner{enterprise_name = EntName, hostname = Hostname}}, State) ->
    NewState = maps:remove({EntName, Hostname}, State),
    deliv_event:publish(runners_state_updated, to_list(NewState)),
    {noreply, NewState};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVersion, State, _Extras) ->
    {ok, State}.

to_list(State) ->
    Fun = fun(_Key, Runner, AccIn) -> [Runner | AccIn] end,
    maps:fold(Fun, [], State).

