%% Copyright 2014 Opscode, Inc. All Rights Reserved.

%% @doc Provides a few helper functions to use in tests to start,
%% stop & configure the apps.

-module(eu_application).

-include_lib("delivery/include/deliv_types.hrl").

-export([setup/0,
         teardown/0]).

-define(GIT_WORK_TREE, "/tmp/git_workspace").

application_environment_setup() ->
  DelivEnv = [{listen_ip, "127.0.0.1"},
              {listen_port, 9618},
              {hostname, "127.0.0.1"},
              {api_proto, "http"},
              {deliv_ssh_git_hostname, "127.0.0.1"},
              {deliv_ssh_git_port, 8989},
              {deliv_git_working_tree_dir, ?GIT_WORK_TREE},
              {deliv_git_repos_root, <<"/tmp/test_git_repos">>},
              {deliv_git_repo_template, project_path(delivery_app, "priv/git_repo_template")},
              {git_executable, string:strip(os:cmd("which git"), right, $\n)},

              %% our tests create a zillion changes that never evolve past the first stages;
              %% if we were to restart the changes created by previous stages on restart, that
              %% would result in using up all available threads from the pooler app, and
              %% things would be sad
              {restart_stages_on_boot, false}
             ],
  [ ok = application:set_env(delivery, Key, Val) || {Key, Val} <- DelivEnv ],
  ok.

setup() ->
    application_environment_setup(),
    application:start(delivery).

teardown() ->
    os:cmd("rm -Rf " ++ ?GIT_WORK_TREE),
    application:stop(delivery).

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
