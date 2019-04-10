%% ===================================================================
%% Bitbucket application supervisor
%% ===================================================================
-module(scm_sup).

-behavior(supervisor).

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
    Procs = [
             {scm_async_git_worker_sup,
              {scm_async_git_worker_sup, start_link, []},
              permanent, 5000, supervisor, [scm_async_git_worker_sup]},
             {scm_git_worker_sup,
              {scm_git_worker_sup, start_link, []},
              permanent, 5000, supervisor, [scm_git_worker_sup]}
            ],
    {ok, {{one_for_one, 5, 10}, Procs}}.
