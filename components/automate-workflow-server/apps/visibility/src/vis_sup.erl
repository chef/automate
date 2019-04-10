-module(vis_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    Enabled = application:get_env(visibility, enabled),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Enabled).

init({ok, true}) ->
    %% Initialize schemas for validation
    vis_json:init_schemas(),
    Procs = [],
    {ok, {{one_for_one, 10, 10}, Procs}};
init(_) ->
    chef_log:info("Visibility disabled"),
    {ok, {{one_for_one, 10, 10}, []}}.
