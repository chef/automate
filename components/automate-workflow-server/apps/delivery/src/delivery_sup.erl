-module(delivery_sup).

-behaviour(supervisor).

-export([
         init/1,
         start_link/0,
         update_dispatch/0
        ]).

-include("deliv_types.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%% cowboy supports named listeners. We could, for example, create a
%% second listener on an alternate port for diagnostics and metrics.
-define(API_LISTENER, http).

-spec start_link() -> {ok, pid()} | any().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

update_dispatch() ->
    cowboy:set_env(?API_LISTENER, dispatch, dispatch()).

dispatch() ->
   cowboy_router:compile([{'_', gather_routes()}]).

gather_routes() ->
   Routes = lists:flatten([Mod:routes() || Mod <- [
                                          audit_routes,
                                          scm_routes,
                                          deliv_github_routes,
                                          notification_routes,
                                          auth_routes,
                                          jobs_routes,
                                          vis_routes,
                                          deliv_routes %% THIS MUST ALWAYS BE AT THE END
                                         ]]),
   log_routes(Routes),
   Routes.

log_routes(Routes) ->
    chef_log:info("configuring dispatch routes"),
    [ chef_log:info("route: ~s => ~s", [Path, Mod])
      || {Path, Mod, _} <- Routes ].

init([]) ->
    %% Initialize schemas for validation
    deliv_json:init_schemas(),

    %% Start the SSH server for git
    %% TODO I'm not clear whether `ssh:daemon' starts a supervised
    %% child or if we need to handle supervision ourselves?
    {ok, _Sshd} = deliv_ssh_git:start(),

    Opts =  [
             {ip, get_ip()},
             {port, get_port()}
            ],
    {ok, _Pid} = cowboy:start_http(?API_LISTENER, 100, Opts,
                                   [{env, [{dispatch, dispatch()}]}]),

    chef_log:info("starting cowboy with ~p", [Opts]),

    %% Doing this in a function so we can exercise it later.
    DeliveranceProcs = build_deliverance_procs(),

    %% we want sync enabled on dev boxes
    case delivery_app:get_env(is_dev_box, false) of
        true -> delivery_app:start_app_with_deps(sync);
        false -> ok
    end,

    %% We have this A2mode configuration available but since we are
    %% mergint the repository we no longer need to worry about the
    %% code here supporting both modes.
    %%
    %% A2mode = delivery_app:get_env(a2_mode, false),

    %% Doing the list construction here to be really clear the
    %% deliverance cruft is bolted on the end.
    Procs = [{deliv_git_working_tree_sup,
              {deliv_git_working_tree_sup, start_link, []},
              permanent, 5000, supervisor, [deliv_git_working_tree_sup]},
             {deliv_hipchat_sup,
              {deliv_hipchat_sup, start_link, []},
              permanent, 5000, supervisor, [deliv_hipchat_sup]},
             {deliv_stage_event_listeners_sup,
              {deliv_stage_event_listeners_sup, start_link, []},
              permanent, 5000, supervisor, [deliv_stage_event_listeners_sup]},
             {deliv_dependency_failures,
              {deliv_dependency_failures, start_link, []},
               permanent, 5000, worker, [deliv_dependency_failures]},
             {deliv_change_lifecycle_listener,
              {deliv_change_lifecycle_listener, start_link, []},
               permanent, 5000, worker, [deliv_change_lifecycle_listener]}
             | DeliveranceProcs],
    {ok, {{one_for_one, 5, 10}, Procs}}.

get_port() ->
    deliv_routes:get_non_neg_integer(listen_port).

get_ip() ->
    case inet:parse_address(delivery_app:get_env(listen_ip)) of
        {ok, IP} ->
            IP;
        {error, einval} ->
            erlang:error({invalid_app_config,
                          {delivery, listen_ip, delivery_app:get_env(listen_ip)}})
    end.

%% Build out the process we need running for deliverance stage logic.
%% Doing this here so it is easier to remove later.
build_deliverance_procs() ->
    StageQueue = {deliverance_stage_queue,
                  {deliverance_stage_queue, start_link, []},
                  permanent, 5000, worker, [deliverance_stage_queue]},

    StageSup = {deliv_stage_sup,
                {deliv_stage_sup, start_link, []},
                permanent, 5000, supervisor, [deliv_stage_sup]},

    Processes = [StageQueue,
                 StageSup],

    chef_log:info("Defined Chef Delivery processes: ~p~n", [Processes]),
    Processes.
