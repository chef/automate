%% @doc Helper funs to make it more delightful to write eunit tests with git
-module(eu_git).

-include_lib("delivery/include/deliv_types.hrl").

-export([
        with_empty_bare_repo/1,
        with_work_repo_and_master_branch/1,
        with_test_env/1
       ]).

-export([
        get_sha/2
       ]).

-type test_fun(ConfigData) :: fun((ConfigData) -> []).
-type test_with_repo_fixture(ConfigData) :: {setup, fun(() -> ConfigData), fun((ConfigData) -> ok), test_fun(ConfigData)}.

%% @doc Helps run a test with the delivery app config needed for git tests
-spec with_test_env(test_fun(_)) -> test_with_repo_fixture(_).
with_test_env(Test) ->
    {setup,
     fun setup_env/0,
     fun(_) -> clean_env() end,
     Test}.

%% @doc The env should always get cleaned up afterwards with `clean_env/0'
-spec setup_env() -> ok.
setup_env() ->
    [ application:set_env(delivery, Key, Value)
      || {Key, Value} <- eu_git_env() ],
    ok.

%% @doc Removes the delivery app config set by {@link setup_env/0}.
-spec clean_env() -> ok.
clean_env() ->
    [ ok = application:unset_env(delivery, Key) || {Key, _} <- eu_git_env() ],
    ok.

%% Return a proplist for setting/clearing the delivery application
%% config.
eu_git_env() ->
    TemplateDir =  app_test_helpers:project_path(?MODULE, "priv/git_repo_template"),
    [{git_executable, find_git()},
     {deliv_ssh_git_port, <<"1234">>},
     {deliv_fips_stunnel_proxy_port, <<"11111">>},
     {deliv_ssh_git_hostname, "my-delivery.chef.io"},
     {deliv_git_repo_template, TemplateDir}].

find_git() ->
    DelivGit = "/opt/delivery/embedded/bin/git",
    case filelib:is_file(DelivGit) of
        true ->
            DelivGit;
        false ->
            case os:find_executable("git") of
                false ->
                    error("can't find git executable");
                PathGit ->
                    PathGit
            end
    end.

%% @doc Helps run a test with a empty bare repo set up
%% Meant to be used as follows:
%% my_test_() ->
%%     eu_git:with_empty_bare_repo(fun(RepoPath) ->
%%         your tests...
%%         return a nested list of ?_* eunit macros
%%     end).
%% WARNING: keep in mind that the repo created here
%% does not have the normal delivery hooks! so don't
%% use this to test our hooks (that should be done
%% in CT tests anyway)
-spec with_empty_bare_repo(test_fun(string())) -> test_with_repo_fixture(string()).
with_empty_bare_repo(Test) ->
    {setup,
     fun() ->
         setup_env(),
         create_bare_repo()
     end,
     fun cleanup_dirs_and_env/1,
     Test}.

%% @doc Helps run a test with both bare and work repos set up,
%% and a 'master' branch created on both (with an empty commit)
%% Kinda ugly, but it's impossible to create a valid git
%% ref in an empty bare repo, and a lot of tests do require
%% an existing ref
%% Meant to be called the same way as `with_empty_bare_repo/1' above
%% WARNING: the same warning as for `with_empty_bare_repo/1' applies!
-spec with_work_repo_and_master_branch(test_fun({string(), string()}))
    -> test_with_repo_fixture({string(), string()}).
with_work_repo_and_master_branch(Test) ->
    {setup,
     fun() ->
         setup_env(),

         BareRepoPath = create_bare_repo(),

         %% now let's clone the repo
         WorkRepoPath = clone_repo(BareRepoPath),

         %% and now let's create a master branch with
         %% an empty commit, and push it to the bare repo
         [git_cmd(Cmd, WorkRepoPath)
          || Cmd <- [["commit", "--allow-empty", "-m", "'Root commit'"],
                     ["push", "origin", "HEAD:master"]]],

         {BareRepoPath, WorkRepoPath}
     end,
     fun cleanup_dirs_and_env/1,
     Test}.

%% @private
cleanup_dirs_and_env(Dirs) ->
    clean_env(),
    rm_dirs(Dirs).

%% @private
%% @doc We cannot use `deliv_git:create_repo/1' because we do _not_
%% want to use our repo template here, since we want to be able to
%% run these tests without having the HTTP service up, nor the DB around
-spec create_bare_repo() -> string().
create_bare_repo() ->
    BareRepoPath = ec_file:insecure_mkdtemp(),
    git_cmd(["init", "--bare"], BareRepoPath),

    BareRepoPath.

%% @private
-spec clone_repo(string()) -> string().
clone_repo(BareRepoPath) ->
    WorkRepoRoot = ec_file:insecure_mkdtemp(),
    WorkRepoName = "repo",
    WorkRepoPath = filename:join(WorkRepoRoot, "repo"),

    git_cmd(["clone", BareRepoPath, WorkRepoName], WorkRepoRoot),

    WorkRepoPath.

%% @private
-spec git_cmd([string()], string()) -> binary().
git_cmd(CmdList, Dir) ->
    {ok, GitExe} = application:get_env(delivery, git_executable),
    GitCmd = [GitExe | CmdList],
    {0, Output} = chef_utils:run_exe(GitCmd, Dir),
    Output.

%% @private
-spec rm_dirs(string() | tuple()) -> ok.
rm_dirs(Dir) when erlang:is_list(Dir) ->
    ok = ec_file:remove(Dir, [recursive]),
    ok;
rm_dirs(Dirs) when is_tuple(Dirs) ->
    lists:foreach(fun rm_dirs/1, erlang:tuple_to_list(Dirs)),
    ok.

%% @doc Returns the SHA for the given `Ref'
-spec get_sha(string(), str_or_binary()) -> binary().
get_sha(RepoPath, Ref) ->
    Out = git_cmd(["rev-parse", chef_utils:to_str(Ref)], RepoPath),
    [Sha, <<>>] = binary:split(Out, <<"\n">>),
    Sha.
