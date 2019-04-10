-module(jobs_runners_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         create/6,
         delete/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec create(binary(), binary(), binary(), binary(), binary(), binary()) -> {ok, pid()}.
create(EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion) ->
    ExistingRunners = supervisor:which_children(?MODULE),
    RunnerPid = [Pid || {_, Pid, _, _} <- ExistingRunners, {EntName, Hostname} == jobs_runner:fetch_ent_hostname(Pid)],
    create_or_return(RunnerPid, EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion).

- spec create_or_return(list(), binary(), binary(), binary(), binary(), binary(), binary()) -> {ok, pid()}.
create_or_return([Pid], _EntName, _Hostname, _Os, _PlatformFamily, _Platform, _PlatformVersion) ->
    {ok, Pid};
create_or_return([], EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion) ->
    supervisor:start_child(?MODULE, [jobs_runner, [EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion]]).

-spec delete(pid()) -> ok.
delete(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(jobs_runner, worker)]} }.
