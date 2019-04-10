-module(jobs_runner_registry).

-include("jobs_types.hrl").

-behaviour(gen_server).

% Public API
-export([
         start_link/0,
         register/6,
         register_get_public_key/6,
         health/1,
         delete/1
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

init([]) ->
    {ok, [], 0}.

-spec register(binary(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, jobs_runner()} | {error, any()}.
register(EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion) ->
    gen_server:call(?MODULE, {create, EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion}).

-spec register_get_public_key(binary(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, any()}.
register_get_public_key(EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion) ->
    gen_server:call(?MODULE, {create_get_public_key, EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion}).

-spec health(jobs_runner()) -> jobs_runner().
health(Runner) ->
    gen_server:call(?MODULE, {health, Runner}).

-spec delete(jobs_runner()) -> ok | {error, any()}.
delete(#runner{} = Runner) ->
    gen_server:call(?MODULE, {delete, Runner}).

handle_call({create, EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion}, _From, State) ->
    {{ok, Runner}, NewState} = create_if_absent(proplists:get_value({EntName, Hostname}, State),
                                                EntName, Hostname, Os, PlatformFamily, Platform,
                                                PlatformVersion, State),
    {reply, {ok, Runner}, NewState};
handle_call({create_get_public_key, EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion}, _From, State) ->
    {{ok, Runner}, NewState} = create_if_absent(proplists:get_value({EntName, Hostname}, State),
                                                EntName, Hostname, Os, PlatformFamily, Platform,
                                                PlatformVersion, State),
    {reply, {ok, Runner#runner.public_key}, NewState};
handle_call({delete, #runner{enterprise_name = EntName, hostname = Hostname, pid = Pid}}, _From, State) ->
    %% First, delete the runner from the database.
    %% Match on ok so we crash in the case of db failure
    %% and hopefully come back up in a nicer state.
    ok = jobs_runner:delete(Pid),
    jobs_runners_sup:delete(Pid),
    {reply, ok, proplists:delete({EntName, Hostname}, State)};
handle_call({health, #runner{ pid = Pid }}, _From, State) ->
    NewRunner = jobs_runner:health(Pid),
    {reply, NewRunner, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, []) ->
    State = populate_initial_state(jobs_runner_db:fetch_all(), []),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVersion, State, _Extras) ->
    {ok, State}.

populate_initial_state([], State) ->
    State;
populate_initial_state([#runner{
                           enterprise_name = EntName,
                           hostname = Hostname,
                           os = Os,
                           platform_family = PlatformFamily,
                           platform = Platform,
                           platform_version = PlatformVersion
                          }|Tail], State) ->
    {{ok, _}, NewState} = create_if_absent(undefined, EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion, State),
    populate_initial_state(Tail, NewState).

create_if_absent(undefined, EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion, State) ->
    {ok, Pid} = jobs_runners_sup:create(EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion),
    Runner = jobs_runner:fetch_state(Pid),
    {{ok, Runner}, [{{EntName, Hostname}, Pid} | State]};
create_if_absent(Pid, _EntName, _Hostname, _Os, _PlatformFamily, _Platform, _PlatformVersion, State) ->
    Runner = jobs_runner:fetch_state(Pid),
    {{ok, Runner}, State}.
