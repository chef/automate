%% ===================================================================
%% Bitbucket git client
%% ===================================================================
-module(scm_git_worker_sup).

-behavior(supervisor).

-include_lib("delivery/include/deliv_coordinates.hrl").

%% ===================================================================
%% Our API
%% ===================================================================
-export([start_link/0]).

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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init(_) ->
    Procs = [{scm_git_worker,
              {scm_git_worker, start_link, []},
              permanent, 5000, worker, [scm_git_worker]}],
    {ok, {{one_for_one, 5, 10}, Procs}}.
