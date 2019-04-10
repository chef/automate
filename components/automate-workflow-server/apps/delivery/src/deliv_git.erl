-module(deliv_git).

-include("deliv_types.hrl").

-export([
         authorized_git_action/5,
         authorized_git_action/3,
         create_repo/1,
         delete_repo/1,
         file_diff/4,
         file_diff/5,
         file_at_sha/3,
         files_at_sha/3,
         run_git/3,
         run_git/4,
         run_git/5,
         rev_parse/2
        ]).

%% Branch-related functions
-export([
         branch_exists/2,
         create_branch/3,
         move_branch/3,
         branch_type/1,
         force_delete_branch/2
        ]).

-export([
         log/3,
         merge_base/3,
         patchset_branch/3,
         feature_branch/2,
         patchset_changed_files/7,
         patchset_commit_log/7,
         patchset_diffstats/7
        ]).

%% utility functions
-export([
         ensure_hooks/1,
         uri/4,
         fips_uri/4
        ]).

-type git_action() :: read | write | review | deliver | approve.

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(BETWEEN, <<"-|||||">>).
-define(AFTER,   <<"_|||||">>).

-spec file_at_sha(iodata(), str_or_binary(), str_or_binary()) ->
                         {ok, binary()} |
                         {error, tuple()}.
file_at_sha(Repo, SHA, File) ->
    case files_at_sha(Repo, SHA, [File]) of
        [Contents] ->
            {ok, Contents};
        [{error, Why} = Error] ->
            chef_log:error("Could not get file ~s at SHA ~s in ~s : ~p",
                            [File, SHA, Repo, Why]),
            Error
    end.

-spec files_at_sha(iodata(), str_or_binary(), [str_or_binary()]) ->
                          [binary() | {error, tuple()}].
files_at_sha(Repo, SHA, Files) ->
    lists:map(
      fun (File) ->
              %% try-catch to appease dialyzer
              try
                  run_git(files_at_sha,
                          Repo,
                          ["show",
                           chef_utils:to_str(SHA) ++ ":" ++
                               chef_utils:to_str(File)],
                          fun({0, FileBin}) ->
                                  FileBin;
                             %% this can mean that the file isn't there at
                             %% the SHA requested, i.e. that it's been added.
                             %% not 100% that this is all it ever means
                             %% FIXME: we should return something else that would
                             %% allow us to make a difference between an empty file
                             %% and a file that's not there!
                             ({128, _ErrMsg}) ->
                                  <<>>;
                             ({error, timeout} = E)->
                                  E;
                             ({Status, Out}) ->
                                  {error, {files_at_sha, git_fail, Status, Out}}
                          end)
              catch _:Exp ->
                      {error, {files_at_sha, map_failure, Exp, erlang:get_stacktrace()}}
              end
      end,
      Files).

%% @doc file_diff/4,5 returns the git diff between two SHAs in the repo for
%% the named file. If "Context" is specfied, git is called with the
%% "--unified <N>" option, which gives N lines of context, instead of the
%% usual 3.
-spec file_diff(iodata(), str_or_binary(), str_or_binary(), [str_or_binary()]) ->
                          {ok, str_or_binary()} | {error, tuple()}.
file_diff(Repo, StartSHA, EndSHA, FilePath) ->
    file_diff(Repo, StartSHA, EndSHA, FilePath, context_unspecified).

-spec file_diff(iodata(), str_or_binary(), str_or_binary(), [str_or_binary()], atom()) ->
                          {ok, binary()} | {error, tuple()}.
file_diff(Repo, StartSHA, EndSHA, FilePath, context_unspecified) ->
    run_git(diff,
        Repo,
        ["diff",
         chef_utils:to_str(StartSHA),
             chef_utils:to_str(EndSHA),
             "--",
             FilePath]);
%% @doc this clause handles the case where the change has merged, and the
%% diff SHAs are identical; here we compare the end SHA with the preceding SHA
%% by re-writing the Start SHA as [SHA}^.
file_diff(Repo, StartSHA, EndSHA, FilePath, Context) when StartSHA =:= EndSHA ->
    NewStartSHA = <<StartSHA/binary, <<"^">>/binary>>,
    file_diff(Repo, NewStartSHA, EndSHA, FilePath, Context);
file_diff(Repo, StartSHA, EndSHA, FilePath, Context) ->
    run_git(diff,
        Repo,
        ["diff", "--unified=" ++ chef_utils:to_str(Context),
         chef_utils:to_str(StartSHA),
             chef_utils:to_str(EndSHA),
             "--",
             FilePath]).

-spec rev_parse(str_or_binary(), str_or_binary()) -> binary().
rev_parse(Ref, RepoPath) ->
    {ok, Rev} = run_git(rev_parse, chef_utils:to_str(RepoPath),
                        ["rev-parse",
                         chef_utils:to_str(Ref)]),
    binary:replace(Rev, <<"\n">>, <<>>).

%% @doc Creates a bare git repo on the given path
%% and installs git hooks
%% TODO OS ownership of said repo??
-spec create_repo(str_or_binary()) -> {ok, binary()} |
                                      {error, binary() | atom()}.
create_repo(RepoPath) ->
    TemplateDir = delivery_app:get_env(deliv_git_repo_template),
    InitCmd = ["init", "--bare",
               "--template=" ++ TemplateDir],
    run_git(create_repo, RepoPath, InitCmd).

%% @doc Deletes a git repo on the given path
-spec delete_repo(str_or_binary()) -> ok.
delete_repo(RepoPath) ->
    ec_file:remove(chef_utils:to_str(RepoPath)).

ensure_repo_path(RepoPath) ->
    %% we add a trailing slash to make sure `filelib:ensure_dir'
    %% will do what we want it to - won't hurt anyway even if it's
    %% already there
    BinRepoPath = chef_utils:to_bin(RepoPath),
    PathWithTrailingSlash = <<BinRepoPath/binary, "/">>,
    filelib:ensure_dir(PathWithTrailingSlash).

%% @doc Makes sure the repo's hooks dir is setup as a symbolic
%% link to the template directory instead of having the files
%% inside the repository. This makes it easier to manage them
%% centrally.
-spec ensure_hooks(binary())
        -> ok | {error, binary() | atom()}.
ensure_hooks(RepoPath) ->
    TemplateHooks = delivery_app:get_env(deliv_git_repo_template)
                    ++ "/" ++ "hooks",
    RepoHooks = chef_utils:to_str(RepoPath) ++ "/" ++ "hooks",

    case file:read_link(RepoHooks) of
      {ok, TemplateHooks} ->
        %% The link is created and correct.
        ok;
      {ok, _} ->
        %% There is a link but it doesn't point to correct spot delete
        %% the current one and re-create.
        update_hooks(TemplateHooks, RepoHooks);
      {error, Err} when Err =:= einval; Err =:= enoent ->
        %% einval - RepoHooks is not a link.
        %% enoent - RepoHooks does not exist.
        update_hooks(TemplateHooks, RepoHooks);
      Error ->
        Error
    end.

%% @private
%% @doc Given a template hooks dir and a repo hooks make sure
%% repo hooks links to template hooks.
-spec update_hooks(string(), string()) -> ok | {error, _Why}.
update_hooks(TemplateHooks, RepoHooks) ->
    case ec_file:remove(RepoHooks, [recursive]) of
      ok ->
        file:make_symlink(TemplateHooks, RepoHooks);
      {error,enotdir} ->
        %% RepoHooks is not a directory, delete as a file.
        case ec_file:remove(RepoHooks) of
          ok ->
            file:make_symlink(TemplateHooks, RepoHooks);
          Error ->
            Error
        end;
      {error, enoent} ->
        %% RepoHooks does not exist.
        file:make_symlink(TemplateHooks, RepoHooks);
      Error ->
        Error
    end.

handle_run_git({error, _Why} = Error) -> Error;
handle_run_git({0, Output}) -> {ok, Output};
handle_run_git(CmdFailed) -> {error, {git_failed, CmdFailed}}.

-spec run_git(atom(), iodata(), [iodata()]) -> term().
run_git(Caller, RepoPath, Cmd) ->
    run_git(Caller,
            RepoPath,
            Cmd,
            fun handle_run_git/1,
            []).

-spec run_git(atom(), iodata(), [iodata()],
              fun(({integer(), binary()}) -> term()) | proplist(string(), string()) | false)
                  -> term().
run_git(Caller, RepoPath, Cmd, Callback) when erlang:is_function(Callback) ->
    run_git(Caller,
            RepoPath,
            Cmd,
            Callback,
            []);
run_git(Caller, RepoPath, Cmd, Env) when erlang:is_list(Env) orelse Env =:= false ->
    run_git(Caller,
            RepoPath,
            Cmd,
            fun handle_run_git/1,
            Env).

%% @doc Common logic for shelling out to git.
%%
%% `Caller' is simply an atom indicating the name of the function this
%% is being called from. It's only used to create error tuples to make
%% it a little easier to see where a failure is coming from.
%%
%% `RepoPath' is the absolute path to a git repository on the server's
%% file system. This is where the command will be executed.
%%
%% `Args0' is a list of strings constituting the arguments to the git
%% command to be run.
%%
%% `Callback' is a function that will be called with the results of
%% the operation, which is a `{ExitCode::integer(), Output::binary()}'
%% tuple.  It should have a head for each case it needs to handle. If
%% the result is not handled, a default handler kicks in, logging the
%% error, and returning an error tuple.
%%
%% `Env' is the list of additional env vars to set
-spec run_git(atom(), iodata(), [iodata()], Callback, Env) -> term() when
    Callback :: fun(({integer(), binary()}) -> term()),
    Env :: proplist(string(), string()) | false.
run_git(Caller, RepoPath, Args0, Callback, Env) ->
    Args = chef_utils:iodata_strify(Args0),
    case ensure_git_and_repo_path(application:get_env(delivery, git_executable), ensure_repo_path(RepoPath)) of
        {ok, Git} ->
            Cmd = [Git | Args],
            case chef_utils:run_exe(Cmd, chef_utils:to_str(RepoPath), Env) of
                {Status, _Output} = Result when erlang:is_integer(Status) ->
                    try Callback(Result)
                    catch
                        error:function_clause ->
                            chef_log:log(error, "Unhandled exit status for git command: ~p, ~p, ~p", [RepoPath, Cmd, Result]),
                            {error, {git_cmd_failed, Caller, RepoPath, Result}}
                    end;
                {error, Why} ->
                    chef_log:log(error, "Git command error: ~p, ~p, ~p", [RepoPath, Cmd, Why]),
                    {error, {git_cmd_error, Caller, RepoPath, Why}}
            end;
        {error, Why} ->
            chef_log:log(error, "Git failure: ~p, ~p, ~p", [RepoPath, Args, Why]),
            {error, {git_cmd_error, Caller, RepoPath, Why}}
    end.

ensure_git_and_repo_path({ok, _Git} = OkGit, ok) ->
    OkGit;
ensure_git_and_repo_path(undefined, RepoPathReturn) ->
    ensure_git_and_repo_path(find_git(chef_utils:find_executable("git")), RepoPathReturn);
ensure_git_and_repo_path(_, {error, Why}) ->
    {error, Why};
ensure_git_and_repo_path(_, _) ->
    {error, no_git_or_repo_path}.

find_git(false) ->
    {error, no_git};
find_git(PathGit) ->
    {ok, PathGit}.

%% @doc Create a new branch `BranchName' in a git repository
%% (`RepoPath') based at `BaseRef'.
%%
%% Assumptions:
%%   - `RepoPath' exists and is, in fact, a repository
%%   - `BranchName' doesn't already exist
%%   - `BaseRef' is a valid Git reference
-spec create_branch(str_or_binary(), str_or_binary(), str_or_binary()) -> ok | {error, _Why}.
create_branch(RepoPath, BranchName, BaseRef) ->
    CreateBranchCmd = ["branch",
                       chef_utils:to_str(BranchName),
                       %% We're not going to explicitly check that
                       %% BaseRef is a valid Git ref (SHA, branch,
                       %% tag); we'll just rely on Git to tell us that
                       chef_utils:to_str(BaseRef)],
    run_git(create_branch,
            RepoPath,
            CreateBranchCmd,
            fun({0, <<>>}) -> ok end).

%% @doc Moves a branch
-spec move_branch(str_or_binary(), str_or_binary(), str_or_binary())
        -> ok | {error, branch_exists | {integer(), binary()}}.
move_branch(RepoPath, OldBranch, NewBranch) ->
    MoveBranchCmd = ["branch", "--move",
                     chef_utils:to_str(OldBranch),
                     chef_utils:to_str(NewBranch)],
    run_git(move_branch,
            RepoPath,
            MoveBranchCmd,
            fun({0, _}) ->
                ok;
               ({128, _} = E) ->
                case branch_exists(RepoPath, NewBranch) of
                    branch_exists ->
                        {error, branch_exists};
                    no_branch_exists ->
                        {error, E}
                end
            end).

-spec force_delete_branch(str_or_binary(), str_or_binary()) -> ok | {error, _Why}.
force_delete_branch(RepoPath, BranchName) ->
    Cmd = ["branch",
           "-D",
           chef_utils:to_str(BranchName)],
    run_git(force_delete_branch,
            RepoPath,
            Cmd,
            fun({0, _}) -> ok; %% Branch deleted
               %% FIXME: this command also returns 1 if we try to delete the branch
               %% we're on (e.g. "master")!!
               ({1, _}) -> ok; %% Branch didn't exist in the first place
               (Error) -> Error
            end).

%% @doc Determine if a branch exists in a given repository.
-spec branch_exists(str_or_binary(), str_or_binary()) -> branch_exists |
                                                  no_branch_exists |
                                                  {error, term()}.
branch_exists(RepoPath, BranchName) ->
    Cmd = ["show-ref", "--verify", "--quiet",
           "refs/heads/" ++ chef_utils:to_str(BranchName)],
    run_git(branch_exists,
            RepoPath,
            Cmd,
            fun({0, _}) -> branch_exists;
               ({1, _}) -> no_branch_exists
           end).

%% @doc
-spec authorized_git_action(d_common_scope(), binary(), git_action()) -> allow | forbid.
authorized_git_action(Scope, UserName, Action) ->
    [EntName, OrgName, ProjName, _Pipe] = deliv_scopes:'#get'(scoping_names, Scope),
    authorized_git_action(EntName, OrgName, ProjName, UserName, Action).

-spec authorized_git_action(binary(), binary(), binary(), binary(), git_action())
                           -> allow | forbid.
authorized_git_action(EntName, OrgName, ProjName, UserName, Action) ->
    Params = [EntName, UserName, OrgName, ProjName],
    case deliv_user:effective_roles(project, Params) of
        {ok, Roles} ->
            deliv_authz:roles_match(Roles, roles_for_action(Action));
        {error, _Why} ->
            forbid
    end.

-spec fips_uri(iodata(), iodata(), iodata(), iodata()) -> binary().
fips_uri(UserName, EntName, OrgName, ProjName) ->
    Port = delivery_app:get_env(deliv_fips_stunnel_proxy_port),
    construct_uri(UserName, EntName, OrgName, ProjName, "localhost", Port).

-spec uri(iodata(), iodata(), iodata(), iodata()) -> binary().
uri(UserName, EntName, OrgName, ProjName) ->
    Hostname = delivery_app:get_env(deliv_ssh_git_hostname),
    Port = delivery_app:get_env(deliv_ssh_git_port),
    construct_uri(UserName, EntName, OrgName, ProjName, Hostname, Port).

%% @private
construct_uri(UserName, EntName, OrgName, ProjName, Hostname, Port) ->
    %% we may need to worry about special shell chars here at some point in
    %% the future but so far it hasn't been an issue
    erlang:iolist_to_binary(["ssh://", deliv_encode:encode(UserName), "@",
                             deliv_encode:encode(EntName), "@", Hostname, ":",
                             chef_utils:to_str(Port), "/",
                             deliv_project:compose_repo_name(EntName, OrgName,
                                                             ProjName)]).

-spec roles_for_action(git_action()) -> [deliv_role()].
roles_for_action(read) -> [<<"admin">>, <<"committer">>, <<"reviewer">>];
roles_for_action(write) -> [<<"admin">>, <<"committer">>];
roles_for_action(review) -> [<<"admin">>, <<"committer">>, <<"reviewer">>];
roles_for_action(deliver) -> [<<"admin">>, <<"shipper">>];
roles_for_action(approve) -> [<<"admin">>, <<"reviewer">>].

%% @doc
branch_type(<<"_for/", _/binary>>) -> reserved_branch;
branch_type(_)                     -> normal_branch.

%% Git Log Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Return a list of commits from `EndSha' back to `StartSha'
%% (i.e., latest is first). Currently the data returned for each
%% commit includes:
%%
%% - Commit SHA
%% - Commit Subject (i.e., the first line of the commit message)
%% - Commit Body (i.e., the rest of the commit message)
-spec log(iodata(), binary(), binary())
        -> {ok, [d_patchset_commit()]} | {error, _Why}.
log(Repo, StartSha, EndSha) ->
    log(Repo, StartSha, EndSha, undefined).

%% @private
-spec log(iodata(), binary(), binary(), non_neg_integer() | undefined)
        -> {ok, [d_patchset_commit()]} | {error, term}.
log(Repo, StartSha, EndSha, PatchsetId) ->
    run_git(log,
            Repo,
            ["log", "--format=sha: %H%nsubject: %s%nbody: %b%n" ++ chef_utils:to_str(?AFTER),
             range(StartSha, EndSha)],
            fun({0, Data}) ->
                Lines = binary:split(Data, <<"\n">>, [global, trim]),
                {ok, process_log_lines(Lines, [], PatchsetId)}
            end).

%% @doc Wrapper for `log/3' to retrieve the commit messages of all
%% commits on a particular patchset branch, from the tip of the branch
%% to the common ancestor with the feature branch.
-spec patchset_commit_log(binary(), binary(), binary(), binary(), binary(), binary(), d_patchset())
                            -> {ok, [d_patchset_commit()]} | {error, term()}.
patchset_commit_log(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) ->
    PatchsetSeqNum = deliv_patchset:getval(sequence_number, Patchset),
    PatchsetId = deliv_patchset:getval(id, Patchset),
    {RepoPath, MergeBase, PatchsetBranch} = patchset_coordinates(EntName,
                                                                 OrgName,
                                                                 ProjName,
                                                                 ChangeId,
                                                                 PipeName,
                                                                 FeatureBranch,
                                                                 PatchsetSeqNum),
    log(RepoPath, MergeBase, PatchsetBranch, PatchsetId).

%% @doc Given a list of binary strings representing the output of a
%% `git log' invocation, process it into discrete `d_patchset_commit()'
%% records.
%%
%% See `log/3' for details of the format being parsed here.
-spec process_log_lines([binary()], [d_patchset_commit()], non_neg_integer() | undefined)
        -> [d_patchset_commit()].
process_log_lines([], Acc, _PatchsetId) ->
    lists:reverse(Acc);
process_log_lines([<<"sha: ", Sha/binary>> | Rest], Acc, PatchsetId) ->
    process_log_lines(Rest, [deliv_patchset_commit:fromlist([{patchset_id, PatchsetId},
                                                              {sha, Sha}]) | Acc], PatchsetId);
process_log_lines([<<"subject: ", Subject/binary>> | Rest], [Current | Acc], PatchsetId) ->
    process_log_lines(Rest, [deliv_patchset_commit:setvals([{subject, Subject}], Current) | Acc], PatchsetId);
process_log_lines([<<"body: ", FirstBodyLine/binary>> | Rest], [Current | Acc], PatchsetId) ->
    {Body, RestOfOutput} = consume_log_body([FirstBodyLine], Rest),
    process_log_lines(RestOfOutput, [deliv_patchset_commit:setvals([{body, Body}], Current) | Acc], PatchsetId).

%% @doc Commit bodies can span several lines; this accumulates all the
%% lines of a single commit body and merges them back into a single
%% binary string.
%%
%% Returns the remainder of the output beyond the end of the current
%% commit body for further processing (see `process_log_lines/2').
-spec consume_log_body([binary()], [binary()]) -> {binary(), [binary()]}.
%% this is to remove the trailing new line from the body
consume_log_body([<<>>, <<"\n">> | BodWithoutTrailingNewLine], [?AFTER | _Rest] = R) ->
    consume_log_body(BodWithoutTrailingNewLine, R);
consume_log_body(Acc, [?AFTER | Rest]) ->
    {erlang:list_to_binary(lists:reverse(Acc)), Rest};
consume_log_body(Acc, [Next | Rest]) ->
    %% Add a newline in, because we're gonna squash it all back down
    %% to a single binary in the end
    consume_log_body([Next, <<"\n">> | Acc], Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Git Diffstats
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec diffstats(iodata(), binary(), binary(), non_neg_integer() | undefined)
        -> {ok, d_patchset_diffstat()} | {error, term()}.
diffstats(Repo, Ref1, Ref2, PatchsetId) ->
    run_git(diffstats,
            Repo,
            ["diff", "--shortstat", range(Ref1, Ref2)],
            fun(CmdResult) -> process_diffstats(CmdResult, PatchsetId) end).

%% @doc Wrapper for `diffstats/3' to retrieve diffstats between the
%% tip of a patchset branch and its most recent ancestor with its
%% target branch.
-spec patchset_diffstats(binary(), binary(), binary(), binary(), binary(), binary(), d_patchset())
        -> {ok, d_patchset_diffstat()} | {error, term()}.
patchset_diffstats(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) ->
    PatchsetSeqNum = deliv_patchset:getval(sequence_number, Patchset),
    PatchsetId = deliv_patchset:getval(id, Patchset),
    {RepoPath, MergeBase, PatchsetBranch} = patchset_coordinates(EntName,
                                                                 OrgName,
                                                                 ProjName,
                                                                 ChangeId,
                                                                 PipeName,
                                                                 FeatureBranch,
                                                                 PatchsetSeqNum),
    diffstats(RepoPath, MergeBase, PatchsetBranch, PatchsetId).

%% Handler function for a `run_git/4` call. Extracted so we can unit
%% test it more easily!
process_diffstats({0, <<>>}, PatchsetId) ->
    %% This happens with empty commits
    {ok, deliv_patchset_diffstat:fromlist([{id, PatchsetId},
                                           {files_changed, 0},
                                           {insertions, 0},
                                           {deletions, 0}])};
process_diffstats({0, Result}, PatchsetId) ->
    %% Generates, e.g.: <<" 19 files changed, 1079 insertions(+), 24
    %% deletions(-)\n">> Depending on the specifics, though, various
    %% parts of that can be present or not (Pretty sure the "files
    %% changed" bit is always present, though).
    %%
    %% Because of this, we're going to use named capture groups to
    %% identify the bits that we pull out (otherwise, we won't know
    %% what's what!)

    %% This is the actual code that compiles the regex, but I've gone
    %% ahead and done this in a shell and pasted the literal output
    %% here for efficiency / expediency. In real life, we'll probably
    %% want to do this via a parse transform of some flavor.

    %% {ok, RE} = re:compile("((?<FILE>\\d+) files? changed)(, )?((?<INS>\\d+) insertions?\\(\\+\\))?(, )?((?<DEL>\\d+) deletions?\\(\\-\\))?"),

    %% TODO: parse transform it!
    RE = {re_pattern,8,0,0,
            <<69,82,67,80,0,1,0,0,0,0,0,0,65,0,0,0,255,255,255,255,255,255,
              255,255,0,0,100,0,0,0,8,0,0,0,64,0,7,0,3,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,68,69,76,0,0,0,2,70,73,76,69,0,
              0,5,73,78,83,0,27,125,0,167,127,0,43,0,1,127,0,7,0,2,87,7,114,0,
              7,29,32,29,102,29,105,29,108,29,101,37,115,29,32,29,99,29,104,
              29,97,29,110,29,103,29,101,29,100,114,0,43,140,127,0,9,0,3,29,
              44,29,32,114,0,9,140,127,0,43,0,4,127,0,7,0,5,87,7,114,0,7,29,
              32,29,105,29,110,29,115,29,101,29,114,29,116,29,105,29,111,29,
              110,37,115,29,40,29,43,29,41,114,0,43,140,127,0,9,0,6,29,44,29,
              32,114,0,9,140,127,0,41,0,7,127,0,7,0,8,87,7,114,0,7,29,32,29,
              100,29,101,29,108,29,101,29,116,29,105,29,111,29,110,37,115,29,
              40,29,45,29,41,114,0,41,114,0,167,0>>},

    %% Matches are returned in the specified capture group ordering;
    %% capture groups that failed to match will return as empty
    %% binaries (<<>>).
    {match, [FilesChanged, Insertions, Deletions]} =
        re:run(Result, RE, [{capture, ['FILE', 'INS', 'DEL'], binary}]),

    %% Helper to convert our binary numbers to actual integers; didn't
    %% feel this merited its own top-level function in the module
    Convert = fun(<<>>) -> 0;
                 (Bin) -> erlang:binary_to_integer(Bin)
              end,

    %% Wrap it all up and ship it out!
    {ok, deliv_patchset_diffstat:fromlist([{id, PatchsetId},
                                           {files_changed, Convert(FilesChanged)},
                                           {insertions, Convert(Insertions)},
                                           {deletions, Convert(Deletions)}])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% File Listing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec changed_files(iodata(), binary(), binary(), non_neg_integer() | undefined)
                   -> {ok, [d_patchset_changed_file()]} | {error, term()}.
changed_files(Repo, Ref1, Ref2, PatchsetId) ->
    NameStatus = run_git(changed_files,
            Repo,
            ["diff", "--name-status", chef_utils:to_str(Ref1), chef_utils:to_str(Ref2)],
            fun(CmdResult) -> CmdResult end),
    NumStat = run_git(changed_files,
            Repo,
            ["diff", "--numstat", chef_utils:to_str(Ref1), chef_utils:to_str(Ref2)],
            fun(CmdResult) -> CmdResult end),
    process_changed_files(NameStatus, NumStat, PatchsetId).

%% @doc Wrapper for `changed_files/4' to retrieve changed files between the
%% tip of a patchset branch and its most recent ancestor with its
%% target branch.
-spec patchset_changed_files(binary(), binary(), binary(), binary(), binary(), binary(), d_patchset())
                            -> {ok, [d_patchset_changed_file()]} | {error, term()}.
patchset_changed_files(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) ->
    PatchsetSeqNum = deliv_patchset:getval(sequence_number, Patchset),
    PatchsetId = deliv_patchset:getval(id, Patchset),
    {RepoPath, MergeBase, PatchsetBranch} = patchset_coordinates(EntName,
                                                                 OrgName,
                                                                 ProjName,
                                                                 ChangeId,
                                                                 PipeName,
                                                                 FeatureBranch,
                                                                 PatchsetSeqNum),
    changed_files(RepoPath, MergeBase, PatchsetBranch, PatchsetId).

%% @private
%% @doc Callback function for `run_git/4' to convert a succesful call
%% to
%%
%%   git diff --name-status $REF1 $REF2
%%
%% into a list of Erlang tuples.
-spec process_changed_files({0, binary()}, {0, binary()}, non_neg_integer() | undefined)
                           -> {ok, [d_patchset_changed_file()]}.
process_changed_files({0, NameStatus}, {0, NumStat},  PatchsetId) ->
    NameStatusLines = binary:split(NameStatus, <<"\n">>, [trim, global]),
    NumStatLines = binary:split(NumStat, <<"\n">>, [trim, global]),
    {ok, [changed_files_from_line(Line, PatchsetId) || Line <-
                                                       lists:zip(NameStatusLines,
                                                                NumStatLines)]}.

changed_files_from_line({NameStatusLine, NumStatLine}, PatchsetId) ->
    SplitNumStatLine = [Val || Val <- binary:split(NumStatLine, <<"\t">>, [global]), Val /= <<>>],
    {Status, Filename, Inserts, Deletes} = file_status(NameStatusLine, SplitNumStatLine),
    deliv_patchset_changed_file:fromlist([{patchset_id, PatchsetId},
                                           {status, Status},
                                           {file, Filename},
                                           {inserts, Inserts},
                                           {deletes, Deletes}]).

%% @doc Parses an output from, e.g., `git diff --name-status ...` into
%% a more Erlangy representation.
file_status(<<"M\t", Filename/binary>>, [Inserts, Deletes, Filename]) ->
    {<<"modified">>, Filename, Inserts, Deletes};
file_status(<<"A\t", Filename/binary>>, [Inserts, Deletes, Filename]) ->
    {<<"added">>, Filename, Inserts, Deletes};
file_status(<<"D\t", Filename/binary>>, [Inserts, Deletes, Filename]) ->
    {<<"deleted">>, Filename, Inserts, Deletes}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: Might want to capture this when a patchset is first
%% committed; otherwise, the target branch can move on (ideally, it
%% should be the same as when first submitted, but that might
%% introduce ordering dependencies between Ref1 and Ref2... need to
%% check into that)

%% @doc Find the most recent common ancestor between `Ref1' and `Ref2'.
%% Arguments are symmetric, no order
-spec merge_base(str_or_binary(), str_or_binary(), str_or_binary())
        -> binary() | {error, term()}.
merge_base(Repo, Ref1, Ref2) ->
    run_git(merge_base,
            Repo,
            ["merge-base",
             chef_utils:to_str(Ref1), chef_utils:to_str(Ref2)],
            fun({0, Result}) ->
                    %% This chops off the trailing newline that git likes to add.
                    binary:replace(Result, <<"\n">>, <<>>)
            end).

%% @doc Helper to create the name of an internal patchset's review branch.
-spec patchset_branch(binary(), binary(), integer()) -> string().
patchset_branch(PipeName, FeatureBranch, PatchsetSeqNumber) ->
    PatchsetSeqNumBin = erlang:integer_to_binary(PatchsetSeqNumber),
    chef_utils:to_str(<<"_reviews/", PipeName/binary,
                         "/", FeatureBranch/binary,
                         "/", PatchsetSeqNumBin/binary>>).

%% @doc Helper to create the name of an internal features's review branch.
-spec feature_branch(binary(), binary()) -> string().
feature_branch(PipeName, FeatureBranch) ->
    chef_utils:to_str(<<"_reviews/", PipeName/binary,
                         "/", FeatureBranch/binary, "/latest">>).

%% @doc A few git commands require us operate on a range of commits,
%% from one ref to another. An example would be "deadbeef..baddecaf".
-spec range(binary(), binary()) -> string().
range(Ref1, Ref2) ->
    chef_utils:to_str(Ref1) ++ ".." ++ chef_utils:to_str(Ref2).

%% @doc For several patchset-related git interactions, we will always
%% need three bits of information, which I am referring to as
%% "coordinates":
%%
%% - The path to a project's repository on disk
%% - The common ancestor of a pipeline target branch and a patchset's branch
%% - The patchset's branch
%%
%% This simply bundles all that up into one convenient function.
-spec patchset_coordinates(binary(), binary(), binary(), binary(), binary(), binary(), integer()) -> {binary(), binary(), binary()}. %% Lawd, that's ugly
patchset_coordinates(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, PatchsetSeqNum) ->
    {ok, RepoPath} = deliv_project:repo_path(EntName, OrgName, ProjName),
    PatchsetBranch = patchset_branch(PipeName, FeatureBranch, PatchsetSeqNum),

    %% If PatchsetBranch no longer exists, then that's a sign that our
    %% change has already merged. We need to sort out which change
    %% this patchset was for and see what its "merge" commit was (I
    %% say that with quotes since we don't actually have real merge
    %% commits at the moment, just fast-forwards).
    %%
    %% We'll use a heuristic of pretending we have one-commit
    %% patchsets (even though we don't currently enforce that!) so we
    %% can go to the parent of the "merge" commit and use that as a
    %% merge base. This also means that we need to use the 'merge_sha'
    %% from the patchset's change as the reference instead of the
    %% PatchsetBranch name.
    %%
    %% In the future, we should either strictly enforce a 1-commit
    %% rule for patchsets, or figure out how to set up proper merge
    %% commits (which itself will require the use of non-bare
    %% repositories, or intermediate clones to use as work
    %% directories).
    %%
    %% This use-case is currently the only reason we pass in
    %% `ChangeId`, though we're probably going to incorporate that
    %% into the patchset branch naming scheme soon, so it's kind of
    %% nice that we have access to it already :)
    {MergeBase, RealPatchsetRef} =
        case branch_exists(RepoPath, PatchsetBranch) of
            branch_exists ->
                %% This is the standard flow when working with changes
                %% that are still open
                {merge_base(RepoPath, PipeName, PatchsetBranch), PatchsetBranch};
            no_branch_exists ->
                %% Here, we have a change that's merged, so we need to
                %% fiddle with the standard merge base and patchset ref
                handle_merged_patchset_branch(RepoPath, ChangeId)
        end,
    %% TODO: Consider just converting everything to strings here
    {RepoPath, MergeBase, RealPatchsetRef}.


%% @doc Find a quasi-suitable substitute for a merge base for a merged
%% change. IF ONLY WE HAD REAL MERGE COMMITS!
-spec handle_merged_patchset_branch(binary(), binary()) -> {MergeBase :: binary(),
                                                            PatchsetRef :: binary()}.
handle_merged_patchset_branch(RepoPath, ChangeId) ->
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),
    case deliv_change:getval(merge_sha, Change) of
        undefined ->
            %% WAT: This means that a merged change hasn't recorded a merge SHA!
            %% We should never get here!
            chef_log:error("Change Id with unrecorded merge SHA: ~s", [ChangeId]),
            erlang:error(unrecorded_merge_sha);
        MergeSha ->
            %% Grab the parent of this commit
            %%
            %% Note that this could also potentially fail if a
            %% user has force-pushed to the repository in such
            %% a way that the recorded MergeSha no longer exists in
            %% the repository. Naughty user!
            {ok, ParentSha} = parent_sha(RepoPath, MergeSha),
            {ParentSha, MergeSha}
    end.

%% @doc Return the SHA of the first parent of `Ref`.
%% @end
%%
%% Note: Could be easily modified to return multiple parents when we
%% actually have merge commits. Not dealing with that for now, though.
-spec parent_sha(binary(), binary()) -> {ok, ParentSha :: binary()} | {error, term()}.
parent_sha(RepoPath, Ref) ->
    Cmd = ["rev-list", "--parents", "-n", "1", chef_utils:to_str(Ref)],
    run_git(parent_sha,
            RepoPath,
            Cmd,
            fun({0, Output}) ->
                    %% This command returns the SHA of `Ref`, followed
                    %% by all parent SHAs. Here, we are only concerned
                    %% with the first one; `_Rest` could be empty.
                    %%
                    %% We could explicitly match on `Ref` instead of
                    %% `_RefAsSha`, but only if we enforce the rule
                    %% that `Ref` is only ever a complete SHA1 sum.

                    %% Ditch that pesky newline
                    Stripped = binary:replace(Output, <<"\n">>, <<>>),

                    %% All the SHAs are on a single line, separated by spaces
                    [_RefAsSha, Parent1 | _Rest] = binary:split(Stripped, <<" ">>, [global, trim]),
                    {ok, Parent1}
            end).
