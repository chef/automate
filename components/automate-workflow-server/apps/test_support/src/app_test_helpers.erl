%% Copyright 2014 Opscode, Inc. All Rights Reserved.

%% @doc Provides a few helper functions to use in tests to start,
%% stop & configure the apps.

-module(app_test_helpers).

-include_lib("delivery/include/deliv_types.hrl").

-export([ensure_app_running/1,
         start_fresh/0,
         setup_app_env/0,
         stop_app/1,
         host/0,
         listen_port/0,
         proto/0,
         project_root/1,
         project_path/2,
         cd_to_root/0]).

-define(TEST_DB_NAME, "delivery_test_ct_db").
-define(DB_HOST, "localhost").
-define(DB_PORT, 5432).
-define(DB_PASS, "inchains").

-define(TEST_APP_LISTEN_PORT, 9618).

%% These two make it possible to override the port and host
%% that get hit by tests, and the protocol used; see `host/0',
%% `listen_port/0' and `proto/0' for more details
-define(TEST_APP_PORT_ENV_VAR, "DELIV_CT_PORT").
-define(TEST_APP_HOST_ENV_VAR, "DELIV_CT_HOST").
-define(TEST_APP_PROTO_ENV_VAR, "DELIV_CT_PROTO").

%% @doc Starts the delivery app with all the dependencies, with the right DB config
start_fresh() ->
    cd_to_root(),
    setup_app_env(),
    delivery_app:start().

%% @doc Needed before starting tests so that the dependencies' are found
-spec cd_to_root() -> ok.
cd_to_root() ->
    ok = file:set_cwd(project_root(delivery_app)).

%% @doc Makes sure an OTP app (or a list of such) is up and running
%% Also makes sure dependencies are started
-spec ensure_app_running(atom() | list(atom())) -> _.
ensure_app_running(App) ->
    delivery_app:start_app_with_deps(App).

%% @doc Same, to stop the apps
-spec stop_app(atom() | list(atom())) -> _.
stop_app(App) when erlang:is_atom(App) ->
    error_logger:tty(false),
    application:stop(App),
    error_logger:tty(true);
stop_app(List) when erlang:is_list(List) ->
    [ stop_app(App) || App <- List ].

setup_app_env() ->
    SqerlEnv = [{db_host, ?DB_HOST},
                {db_port, ?DB_PORT},
                {db_user, "alice"},
                {db_pass, ?DB_PASS},
                {db_name, ?TEST_DB_NAME},
                {idle_check, 10000},
                {prepared_statements,
                 {sqerl_rec, statements,
                  [[{app, delivery},{app,github}, {app, audit}]]}},
                {column_transforms, []}],
    [ ok = application:set_env(sqerl, Key, Val) || {Key, Val} <- SqerlEnv ],

    PoolConfig = [{name, sqerl},
                  {max_count, 100},
                  {init_count, 20},
                  {start_mfa, {sqerl_client, start_link, []}}],
    ok = application:set_env(pooler, pools, [PoolConfig]),

    DelivEnv = [{listen_ip, "127.0.0.1"},
                {listen_port, ?TEST_APP_LISTEN_PORT},
                {hostname, "127.0.0.1"},
                {api_proto, "http"},
                {read_ttl_secs, 3600},
                {write_ttl_secs, 3600},

                %% FIXME: obsolete - delete at the same time as deliv_authzed_keys_server
                {deliv_git_ssh_authorized_keys_path, "/tmp/authorized_keys"},
                {deliv_git_ssh_base_command, <<"/opt/delivery/embedded/bin/delivery-git">>},

                {deliv_git_repos_root, <<"/tmp/test_git_repos">>},
                {deliv_ssh_git_hostname, "127.0.0.1"},
                {deliv_ssh_git_port, 8990},
                {deliv_ssh_git_server_keys_path, "/var/opt/delivery/delivery/etc/ssh_git_server_keys"},
                {deliv_git_repo_template, project_path(delivery_app, "priv/git_repo_template")},
                {deliv_chef_config, "/var/opt/delivery/delivery/etc/erlang.cfg"},
                {git_executable, "/opt/delivery/embedded/bin/git"},
                {deliv_git_working_tree_dir, <<"/tmp/git_workspace">>},

                {deliv_default_search, "recipes:delivery_builder"},
                {push_jobs_status_interval, 0},
                {push_jobs_retry_interval, 0},
                {push_jobs_overall_timeout, 7200},
                {push_jobs_run_timeout, 4500},

                {trusted_certificates_file, "/etc/ssl/certs/ca-certificates.crt"},

                %% our tests create a zillion changes that never evolve past the first stages;
                %% if we were to restart the changes created by previous stages on restart, that
                %% would result in using up all available threads from the pooler app, and
                %% things would be sad
                {restart_stages_on_boot, false}
               ],
    [ ok = application:set_env(delivery, Key, Val)
      || {Key, Val} <- DelivEnv ],
    ok.

%% @doc Returns the port on which the app is running when running CT tests;
%% can be overridden using the `?TEST_APP_PORT_ENV_VAR' env var
%% this is mainly to allow for acceptance-time CT tests
-spec listen_port() -> integer().
listen_port() ->
    case os:getenv(?TEST_APP_PORT_ENV_VAR) of
        false -> ?TEST_APP_LISTEN_PORT;
        Port -> erlang:list_to_integer(Port)
    end.

%% @doc Same as `listen_port/0' above, but for the host
%% All the same comments apply (can be overriden using the
%% `?TEST_APP_HOST_ENV_VAR' env var)
-spec host() -> string().
host() ->
    case os:getenv(?TEST_APP_HOST_ENV_VAR) of
        false -> "localhost";
        Host -> Host
    end.

%% @doc Still the same idea as `listen_port/0' and `host/0' above:
%% we want to be able to set the protocol (http|https) with
%% the `TEST_APP_PROTO_ENV_VAR?' env var
-spec proto() -> string().
proto() ->
    case os:getenv(?TEST_APP_PROTO_ENV_VAR) of
        false -> deliv_web_utils:protocol();
        Proto -> Proto
    end.

%% @doc Returns the absolute path to the project root
%% We detect the project's root as the one that contains a 'src' dir
%% Somewhat yucky, but better than relying on some .beam's location
%% (such as given by `code:which(blah)' since that location will change)
%% depending on whether it's CT or enuit calling it
-spec project_root(atom() | string()) -> string().
project_root(Module) when is_atom(Module) ->
    project_root(filename:absname(code:which(Module)));
project_root("/") -> error(root_not_found);
project_root("/" ++ _Rest = AbsPath) ->
    SrcPath = filename:join(AbsPath, "src"),
    case filelib:is_dir(SrcPath) of
        true -> AbsPath;
        false -> project_root(filename:dirname(AbsPath))
    end.

%% @doc Turns a project relative path into an absolute path
-spec project_path(atom(), str_or_binary()) -> str_or_binary().
project_path(Module, Path) ->
    filename:join(project_root(Module), Path).
