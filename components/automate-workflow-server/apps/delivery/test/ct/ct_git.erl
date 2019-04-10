%% @doc Helpers for interacting with Chef Delivery Git repositories in
%% a Common Test environment
-module(ct_git).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("test_support/include/test_support.hrl").

%% git actions
-export([
         add_branch/4,
         push_dummy_commit/2,
         push_dummy_commit/3,
         push_dummy_commit/5,
         push_dummy_commit_failure/2,
         push_dummy_commit_failure/3,
         push_dummy_commit_failure/5,
         push_head_to/3,
         push_for_review/4,
         clone_empty/3,
         clone_not_empty/3,
         sync/2,
         checkout_latest/3,
         get_head_local_sha/1
        ]).

%% utils
-export([
         cmd/2,
         ssh_cmd/3,
         trigger_branch_name/2
        ]).

%% asserts
-export([
         assert_local_git_cmd_success/2,
         assert_ssh_git_cmd_success/3,
         assert_ssh_git_cmd_failure/3,
         assert_unauthorized_git_action/4,

         assert_branch_exists/2,
         assert_branches_are/2,
         assert_no_branch_exists/2,
         assert_no_remote_branch_exists/2,
         assert_remote_branch_exists/2,
         assert_remote_branches_are/2
        ]).

-export([
         init_repo/2,
         create_change/3,
         create_changes/3,
         create_commit/2,
         create_commits/2,
         add_file_change/2,
         add_file_changes/2
        ]).

-type git_ssh() :: string().

%% Set FileContent to 'delete' if you want to, you know... delete the
%% file
-type file_change() :: {FilePath :: string(),
                        FileContent :: binary() | delete}.
-type commit() :: {CommitSubject :: binary(),
                   CommitBody :: binary(),
                   [file_change()]}.
-type change() :: {FeatureBranch :: binary(),
                   Pipeline :: binary(),
                   [commit()]}.

%% Exporting these types for use in specs in other of our CT helper
%% modules
-export_type([
              change/0,
              commit/0,
              file_change/0
             ]).

-spec clone_empty(str_or_binary(), git_ssh(), proplist(atom(), any()))
        -> binary().
clone_empty(Url, GitSsh, Config) ->
    clone(Url, GitSsh, Config, true).

-spec clone_not_empty(str_or_binary(), git_ssh(), proplist(atom(), any()))
        -> binary().
clone_not_empty(Url, GitSsh, Config) ->
    clone(Url, GitSsh, Config, false).

%% @private
%% @doc Clone a Delivery Git repository
%%
%% `Url' is the (SSH) URL for a repository. (It's assumed to be
%% correctly formatted for Delivery, but nothing requires that to be
%% true).
%%
%% `GitSsh' is the absolute path to a "git ssh" script, which will
%% point to a valid SSH key for a Chef Delivery user (i.e., the key
%% has been registered with that user in the Delivery database
%% already). The key should belong to the user embedded in the `Url'
%% (obviously).
%%
%% `Config' is a Common Test configuration object. It is used to
%% enforce the fact that the cloning should take place in the
%% testcase's `priv_dir', since this is the proper place for such
%% things to happen. This is where you will find your fresh checkout
%% (but don't worry, 'cuz we're gonna return the full path at the
%% end).
%%
%% Note that this currently assumes (because it affirmitively tests
%% for it) that you are cloning the Delivery repository as an empty
%% repository (i.e., this operation is the first thing that anybody
%% has done with this repository). When / if we need to start cloning
%% repos with content, this will obviously need to change.
%%
%% Returns the absolute value of the path to the directory the repo
%% was checked out to. Note that the directory will have the same name
%% as the Git project (i.e., default Git behavior).
-spec clone(str_or_binary(), git_ssh(), proplist(atom(), any()), boolean())
        -> binary().
clone(Url, GitSsh, Config, ShouldBeEmpty) ->
    %% CT should always set us up a priv dir, but it never hurts to
    %% verify!
    PrivDir = ?config(priv_dir, Config),
    ?assertNotEqual(undefined, PrivDir),

    %% to avoid conflicting paths when we checkout the same repo twice
    RepoParentPath = filename:join(PrivDir, ct_utils:unique_string()),
    ok = file:make_dir(RepoParentPath),
    ClonedDirName = dirname_from_git_url(Url),

    Out = assert_ssh_git_cmd_success(["clone",
                                      chef_utils:to_str(Url),
                                      ClonedDirName],
                                     RepoParentPath,
                                     GitSsh),

    case ShouldBeEmpty of
        true ->
            ?assertStringContains(Out, "You appear to have cloned an empty repository");
        false ->
            ?assertStringNotContains(Out, "You appear to have cloned an empty repository")
    end,

    CheckoutPath = filename:join(RepoParentPath, ClonedDirName),
    ?assert(filelib:is_dir(filename:join(CheckoutPath, ".git"))),

    CheckoutPath.

%% @private
%% @doc Extract a directory name from a Git url (really, any binary /
%% string that's remotely filepath-like). This will be the name of the
%% directory that a `git clone' operation will create in the current
%% working directory.
-spec dirname_from_git_url(str_or_binary()) -> string().
dirname_from_git_url(Url) ->
    UrlStr = chef_utils:to_str(Url),
    filename:basename(UrlStr).

%% @doc Initially, our git repositories are completely empty; they'll
%% need some actual commits before they have branches for us to do
%% anything interesting with. This simply creates a commit and pushes
%% it back upstream.
-spec push_dummy_commit(str_or_binary(), git_ssh()) -> binary().
push_dummy_commit(RepoDir, GitSsh) ->
    push_dummy_commit(RepoDir, GitSsh, "master").

-spec push_dummy_commit(str_or_binary(), git_ssh(), str_or_binary()) -> binary().
push_dummy_commit(RepoDir, GitSsh, TargetBranch) ->
    push_dummy_commit(RepoDir, GitSsh, TargetBranch,
                      "hello_world", <<"Hello world!\n">>).

%% @doc returns the output of the push command
-spec push_dummy_commit(str_or_binary(), git_ssh(), str_or_binary(),
        str_or_binary(), str_or_binary()) -> binary().
push_dummy_commit(RepoDir, GitSsh, TargetBranch, FileName, FileContent) ->
    ct:pal("Pushing dummy commit FileName ~p to TargetBranch ~p in RepoDir ~p",
           [FileName, TargetBranch, RepoDir]),
    RepoDirStr = chef_utils:to_str(RepoDir),
    FileNameStr = chef_utils:to_str(FileName),
    TargetBranchStr = chef_utils:to_str(TargetBranch),
    FilePath = filename:join(RepoDirStr, FileNameStr),
    ok = filelib:ensure_dir(FilePath),
    ok = file:write_file(FilePath, FileContent),

    [assert_local_git_cmd_success(GitCmd, RepoDir)
     || GitCmd <- [["add '" ++ FileNameStr ++ "'"],
                   ["commit", "-am", "'Adding new file " ++ FileNameStr ++ "\n\n"
                    ++ "This file was added automatically from ct_git:push_dummy_commit/5"
                    ++ "'"]]],

    push_head_to(RepoDir, GitSsh, TargetBranchStr).

%% @doc This simply creates a commit, pushes it upstream and verifies it fails.
-spec push_dummy_commit_failure(str_or_binary(), git_ssh()) -> binary().
push_dummy_commit_failure(RepoDir, GitSsh) ->
    push_dummy_commit_failure(RepoDir, GitSsh, "master").

-spec push_dummy_commit_failure(str_or_binary(), git_ssh(), str_or_binary()) -> binary().
push_dummy_commit_failure(RepoDir, GitSsh, TargetBranch) ->
    push_dummy_commit_failure(RepoDir, GitSsh, TargetBranch,
                      "hello_world", <<"Hello world!\n">>).

%% @doc returns the output of the push command
-spec push_dummy_commit_failure(str_or_binary(), git_ssh(), str_or_binary(),
        str_or_binary(), str_or_binary()) -> binary().
push_dummy_commit_failure(RepoDir, GitSsh, TargetBranch, FileName, FileContent) ->
    ct:pal("Pushing dummy commit FileName ~p to TargetBranch ~p in RepoDir ~p",
           [FileName, TargetBranch, RepoDir]),
    RepoDirStr = chef_utils:to_str(RepoDir),
    FileNameStr = chef_utils:to_str(FileName),
    TargetBranchStr = chef_utils:to_str(TargetBranch),
    FilePath = filename:join(RepoDirStr, FileNameStr),
    ok = filelib:ensure_dir(FilePath),
    ok = file:write_file(FilePath, FileContent),

    push_head_to_fail(RepoDir, GitSsh, TargetBranchStr).

-spec push_head_to(str_or_binary(), git_ssh(), str_or_binary()) -> binary().
push_head_to(RepoDir, GitSsh, TargetBranch) ->
    PushCmd = ["push", "origin", "HEAD:" ++ chef_utils:to_str(TargetBranch)],
    assert_ssh_git_cmd_success(PushCmd, RepoDir, GitSsh).

-spec push_head_to_fail(str_or_binary(), git_ssh(), str_or_binary()) -> binary().
push_head_to_fail(RepoDir, GitSsh, TargetBranch) ->
    PushCmd = ["push", "origin", "HEAD:" ++ chef_utils:to_str(TargetBranch)],
    assert_ssh_git_cmd_failure(PushCmd, RepoDir, GitSsh).

%% @doc Push a feature branch to pipeline, which triggers review creation
%% TODO in the future, this should use `delivery review`
-spec push_for_review(str_or_binary(), git_ssh(),
                      str_or_binary(), str_or_binary()) -> binary().
push_for_review(RepoDir, GitSsh, Pipeline, FeatureBranch) ->
    FeatureBranchL = chef_utils:to_str(FeatureBranch),
    PushCmd = ["push", "origin",
               FeatureBranchL ++ ":_for/" ++ chef_utils:to_str(Pipeline) ++ "/" ++ FeatureBranchL],
    assert_ssh_git_cmd_success(PushCmd, chef_utils:to_str(RepoDir), GitSsh).

-spec add_branch(string(), git_ssh(),
        str_or_binary(), str_or_binary()) -> _.
add_branch(RepoDir, GitSsh, Branch, Base) ->
    ct:pal("Adding Branch ~p to repo ~p from Base ~p", [Branch, RepoDir, Base]),
    [assert_ssh_git_cmd_success(GitCmd, RepoDir, GitSsh)
     || GitCmd <- [["checkout -b",
                    chef_utils:to_str(Branch),
                    chef_utils:to_str(Base)],
                   ["push", "--set-upstream", "origin",
                    chef_utils:to_str(Branch)]]].

%% @doc An alias dear to CM!
-spec sync(string(), git_ssh()) -> _.
sync(RepoDir, GitSsh) ->
    assert_ssh_git_cmd_success(["fetch", "--all", "--prune"],
                               RepoDir,
                               GitSsh).

%% @doc Checkouts and pulls `Branch'
-spec checkout_latest(string(), git_ssh(),
        str_or_binary()) -> _.
checkout_latest(RepoDir, GitSsh, Branch) ->
    [assert_ssh_git_cmd_success(GitCmd, RepoDir, GitSsh)
     || GitCmd <- [["checkout",
                    chef_utils:to_str(Branch)],
                   ["pull"]]].

%% @doc Returns `HEAD''s local SHA
-spec get_head_local_sha(string()) -> binary().
get_head_local_sha(RepoDir) ->
    Out = assert_local_git_cmd_success(["rev-parse", "HEAD"],
                                       RepoDir),
    [Sha, <<>>] = binary:split(Out, <<"\n">>),
    Sha.

-spec cmd([string()], string()) ->
        {integer(), binary()} | {error, timeout}.
cmd(Cmd, Dir) ->
    cmd(Cmd, Dir, "").

-spec ssh_cmd([string()], string(), git_ssh()) ->
        {integer(), binary()} | {error, timeout}.
ssh_cmd(Cmd, Dir, GitSsh) ->
    cmd(Cmd, Dir, GitSsh).

%% @private
-spec cmd(string(), string(), git_ssh()) ->
        {integer(), binary()} | {error, timeout}.
cmd(Cmd, Dir, GitSsh) ->
    GitCmd = ["git" | Cmd],
    JoinedCmd = string:join(GitCmd, " "),
    Env = case GitSsh of
        "" ->
            ct:pal("Attempting to execute git command in ~p: ~p",
                   [Dir, JoinedCmd]),
            [];
        Path ->
            ct:pal("Attempting to execute git command in ~p: ~p",
                   [Dir,
                    "GIT_SSH='" ++ Path ++ "' " ++ JoinedCmd]),
            [{"GIT_SSH", Path}]
    end,
    chef_utils:run_cmd(GitCmd, Dir, Env).

-spec assert_branches_are(binary(), [binary()]) -> _.
assert_branches_are(RepoDir, Branches) ->
    Existing = deliv_git:list_branches(RepoDir),
    ?assertEqual([],
                 ordsets:subtract(ordsets:from_list(Existing),
                                  ordsets:from_list(Branches))).

-spec assert_branch_exists(str_or_binary(), binary()) -> _.
assert_branch_exists(RepoDir, Branch) ->
    ?assertEqual(branch_exists,
                deliv_git:branch_exists(RepoDir, Branch)).

-spec assert_no_branch_exists(str_or_binary(), binary()) -> _.
assert_no_branch_exists(RepoDir, MissingBranch) ->
    ?assertEqual(no_branch_exists,
                deliv_git:branch_exists(RepoDir, MissingBranch)).

-spec assert_remote_branches_are(string(), [binary()]) -> _.
assert_remote_branches_are(RepoDir, Branches) ->
    Remotes = remote_branches(RepoDir),
    ?assertEqual([],
                 ordsets:subtract(ordsets:from_list(Remotes),
                                  ordsets:from_list(Branches))).

-spec assert_remote_branch_exists(string(), binary()) -> _.
assert_remote_branch_exists(RepoDir, Branch) ->
    ?assert(lists:member(Branch,
                         remote_branches(RepoDir))).

-spec assert_no_remote_branch_exists(string(), binary()) -> _.
assert_no_remote_branch_exists(RepoDir, Branch) ->
    ?assertNot(lists:member(Branch,
                           remote_branches(RepoDir))).

%% @doc Asserts that an SSH git command fails because of authZ
-spec assert_unauthorized_git_action([string()], string(), string(), integer()) -> _.
assert_unauthorized_git_action(GitCmd, RepoDir, GitSsh, ExpectedCode) ->
    {Code, Out} = ssh_cmd(GitCmd, RepoDir, GitSsh),
    ?assertEqual(ExpectedCode, Code),
    %% note that this also checks that we did pass authN!
    ?assertStringContains(Out, "DELIVERY: ERROR: Unauthorized action").

-spec assert_local_git_cmd_success([string()], string()) -> binary().
assert_local_git_cmd_success(GitCmd, Dir) ->
    {0, Out} = cmd(GitCmd, Dir),
    ct:pal("Output of git command is: ~p", [Out]),
    Out.

-spec assert_ssh_git_cmd_success([string()], string(), git_ssh()) -> binary().
assert_ssh_git_cmd_success(GitCmd, Dir, GitSsh) ->
    {0, Out} = ssh_cmd(GitCmd, Dir, GitSsh),
    ct:pal("Output of git command is: ~p", [Out]),
    Out.

-spec assert_ssh_git_cmd_failure([string()], string(), git_ssh()) -> binary().
assert_ssh_git_cmd_failure(GitCmd, Dir, GitSsh) ->
    {Err, Out} = ssh_cmd(GitCmd, Dir, GitSsh),
    ct:pal("Output of git command code:~p, output:~p", [Err, Out]),
    ?assertNotEqual(0, Err).

%% Change Creation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Assumes `Repo' is a checked-out empty repository, with HEAD
%% referring to (a non-existent) master. Adds an empty commit and
%% pushes it upstream to populate the repository.
-spec init_repo(Repo :: string(), GitSsh :: git_ssh()) -> ok.
init_repo(Repo, GitSsh) ->
    assert_local_git_cmd_success(["commit", "--allow-empty", "-m", "'Initial commit'"], Repo),
    assert_ssh_git_cmd_success(["push", "--set-upstream", "origin", "master"], Repo, GitSsh),
    ok.

%% @doc Add one or more changes to a delivery repository. Operates
%% within a local checked-out copy, pushes all commits to the remote,
%% and triggers a submission of a patchset to be merged into a target
%% branch.
-spec create_changes(Repo :: string(), GitSsh :: git_ssh(),
                     [change()]) -> [ChangeId :: binary()].
create_changes(Repo, GitSsh, Changes) ->
    [ create_change(Repo, GitSsh, Change) || Change <- Changes ].

-spec create_change(Repo :: string(), GitSsh :: git_ssh(),
                    change()) -> binary().
create_change(Repo, GitSsh, {FeatureBranch, Pipeline, Commits}) ->
    %% Create a feature branch off of Pipeline
    %%
    %% We'll allow for a feature branch to already exist; otherwise,
    %% we'd never be able to create multiple patchsets!
    case deliv_git:branch_exists(Repo, FeatureBranch) of
        branch_exists ->
            ok;
        no_branch_exists ->
            add_branch(Repo, GitSsh, FeatureBranch, Pipeline)
    end,
    create_commits(Repo, Commits),
    %% TODO: We don't want to push if FeatureBranch and Pipeline point
    %% to the same SHA (i.e., if Commits = []) (this should be added
    %% to delivery review)
    Out = push_for_review(Repo, GitSsh, Pipeline, FeatureBranch),
    ct_change:extract_change_id(Out).

%% @doc Adds one or more commit to a repository. Operates completely
%% locally; does not push anything upstream. Makes no assumptions
%% about what branch is currently HEAD (that's up to you).
-spec create_commits(Repo :: string(), [commit()]) -> ok.
create_commits(Repo, Commits) ->
    lists:foreach(fun(Commit) ->
                          create_commit(Repo, Commit)
                  end,
                  Commits).

-spec create_commit(Repo :: string(), commit()) -> ok.
create_commit(Repo, {Subject, Body, []}) ->
    commit_with_subject_and_body(Repo, Subject, Body, ["--allow-empty"]);
create_commit(Repo, {Subject, Body, FileChanges}) ->
    add_file_changes(Repo, FileChanges),
    commit_with_subject_and_body(Repo, Subject, Body, []).

%% @private
commit_with_subject_and_body(Repo, Subject, Body, Options) ->
    assert_local_git_cmd_success(["commit",
                                  %% Repeated '-m' flags become separate paragraphs!
                                  "-m", "\"" ++ chef_utils:to_str(Subject) ++ "\"",
                                  "-m", "\"" ++ chef_utils:to_str(Body) ++ "\""
                                  | Options],
                                 Repo),
    ok.

%% @doc Given a series of file changes, makes each change and adds the
%% change to the index. No assumptions about the current value of HEAD
%% are made, and no commits are made.
%%
%% Note also that here a "file change" amounts to setting the content
%% of a file to a given content; no diffing or incremental changes are
%% assumed. The change is implicit in the difference between what you
%% specify and what was there before.
%%
%% If the FileContent is the atom `delete' however, this deletes the
%% file.
-spec add_file_changes(Repo :: string(), [file_change()]) -> ok.
add_file_changes(Repo, FileChanges) ->
    lists:foreach(fun(FileChange) ->
                          add_file_change(Repo, FileChange)
                  end,
                  FileChanges).

-spec add_file_change(Repo :: string(), change()) -> ok.
add_file_change(Repo, {FilePath, delete}) ->
    FileNameStr = chef_utils:to_str(FilePath),
    assert_local_git_cmd_success(["rm", FileNameStr], Repo),
    ok;
add_file_change(Repo, {FilePath, FileContent}) ->
    %% Write FileContent to FilePath in Repo
    RepoDirStr = chef_utils:to_str(Repo),
    FileNameStr = chef_utils:to_str(FilePath),

    %% Need to create directories recursively
    DirName = filename:dirname(FileNameStr),
    ok = ec_file:mkdir_p(filename:join(RepoDirStr, DirName)),

    ok = file:write_file(filename:join(RepoDirStr, FileNameStr), FileContent),

    %% 'git add' the file
    assert_local_git_cmd_success(["add", FileNameStr], Repo),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% @doc I realize that some of this code duplicates internal code from
%% deliv_git (particularly normalize_branch/2 and
%% list_branches/1). However, since that code doesn't currently need
%% to be exported for normal application use, I'm hesitant to extract
%% and export them just for this CT helper, since I'd rather not muddy
%% the module's API.
-spec remote_branches(string()) -> [binary()].
remote_branches(RepoDir) ->
    Out = assert_local_git_cmd_success(["branch", "--remote"], RepoDir),
    Lines = binary:split(Out, <<"\n">>, [global, trim]),
    Branches = [begin
                    case(L) of
                        <<"  ", B/binary>> -> B;
                        <<"* ", B/binary>> -> B
                    end
                end || L <- Lines],
    ct:pal("Remote branches of '~p' are: ~p", [RepoDir, Branches]),
    Branches.

%% @doc Builds the branch name to which to push to trigger a new
%% patchset
-spec trigger_branch_name(iodata(), iodata()) -> string().
trigger_branch_name(PipeName, FeatBranch) ->
    chef_utils:to_str(erlang:iolist_to_binary(["_for/", PipeName, "/", FeatBranch])).
