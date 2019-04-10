%% ===================================================================
%% Bitbucket async git client
%% ===================================================================
-module(scm_async_git_worker_sup).

-behavior(supervisor).

-include_lib("delivery/include/deliv_coordinates.hrl").

%% ===================================================================
%% Our API
%% ===================================================================
-export([start_link/0,
         delete_branch/2,
         force_push/3]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-export([init/1]).

%% ===================================================================
%% Our API
%% ===================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec force_push(binary(), binary(), #proj_coordinates{}) -> ok.
force_push(SourceBranch, DestBranch, Coords) ->
    supervisor:start_child(?MODULE, [force_push, [SourceBranch, DestBranch, Coords]]),
    ok.

-spec delete_branch(binary(), #proj_coordinates{}) -> ok.
delete_branch(Branch, Coords) ->
    supervisor:start_child(?MODULE, [delete_branch, [Branch, Coords]]),
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init(_) ->
    Procs = [{scm_async_git_worker,
              {scm_async_git_worker, start_link, []},
              temporary, 5000, worker, [scm_async_git_worker]}],
    {ok, {{simple_one_for_one, 5, 10}, Procs}}.
