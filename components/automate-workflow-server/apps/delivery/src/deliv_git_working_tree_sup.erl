%% @doc A common supervisor for all deliv_git_working_tree workers
-module(deliv_git_working_tree_sup).
-behaviour(supervisor).

%% API
-export([get_worker/1]).

%% meant for deliv_git_working_tree workers
-export([register_worker/1,
         notify_starting_process/2]).

%% callbacks
-export([init/1,
         start_link/0]).

-include("deliv_types.hrl").

%% how long we wait for the worker to start if needed (in ms)
-define(START_GIT_WORKING_TREE_TIMEOUT, 1000).

%% @doc Retrieves the current deliv_git_working_tree worker
%% for a given project; starts it beforehand if needed
-spec get_worker(d_project()) -> {ok, pid()} | {error, timeout}.
get_worker(Proj) ->
    %% wish we could use `gproc:reg_or_locate/3', but said fun
    %% uses only `erlang:spawn/1' to start the process
    %% TODO: maybe later, get a PR merged in gproc to allow starting
    %% processes with `proc_lib:spawn' instead, that would allow us
    %% to do things more cleanly here
    GprocKey = gproc_key(Proj),
    case gproc:where(GprocKey) of
        undefined ->
            %% hasn't been started yet!
            ProjGuid = deliv_project:getval(guid, Proj),
            chef_log:debug("Starting a deliv_git_working_tree worker "
                            "for project ~p", [ProjGuid]),
            supervisor:start_child(?MODULE, [erlang:self(), Proj]),
            %% wait for the child to say it's started
            receive
                {deliv_git_working_tree_worker_started, ProjGuid} ->
                    WorkerPid = gproc:lookup_pid(GprocKey),
                    {ok, WorkerPid}
            after ?START_GIT_WORKING_TREE_TIMEOUT ->
                chef_log:error("Failed to start a deliv_git_working_tree worker "
                                "in time for project ~p", [ProjGuid]),
                {error, timeout}
            end;
        Pid when erlang:is_pid(Pid) ->
            {ok, Pid}
    end.

%% @doc Meant to be called by deliv_git_working_tree workers when they've
%% successfully registered
-spec notify_starting_process(pid(), d_project()) -> ok.
notify_starting_process(StartingProcess, Proj) ->
    ProjGuid = deliv_project:getval(guid, Proj),
    StartingProcess ! {deliv_git_working_tree_worker_started, ProjGuid},
    ok.

%% @doc Also meant to be called by workers, to register themselves
%% Returns ok iff it successfully registered
-spec register_worker(d_project()) -> ok | {error, already_registered}.
register_worker(Proj) ->
    try gproc:reg(gproc_key(Proj)), ok
    catch error:badarg ->
        {error, already_registered}
    end.

%% @private
%% @doc Returns the gproc key associated with a project's
%% deliv_git_working_tree worker
-spec gproc_key(d_project()) -> {n, l, {deliv_git_working_tree, binary()}}.
gproc_key(Proj) ->
    ProjGuid = deliv_project:getval(guid, Proj),
    {n, l, {deliv_git_working_tree, ProjGuid}}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{deliv_git_working_tree,
            {deliv_git_working_tree, start_link, []},
            transient,
            infinity,
            worker,
            [deliv_git_working_tree]}]}}.
