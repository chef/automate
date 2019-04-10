%% @doc Workers to perform sequential git operations in the
%% git working trees we keep on the server
-module(deliv_git_working_tree).
-behaviour(gen_server).

%% Delivery API
-export([
         merge_change/5
         ]).

%% Behaviour API
-export([
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         start_link/2,
         terminate/2
         ]).

-include("deliv_types.hrl").

-record(state, {
            proj :: d_project(),
            dir :: binary()
        }).

%% @doc Merges `ReviewBranch' into `TargetBranch'
-spec merge_change(pid(), ReviewBranch, TargetBranch, ChangeId, Merger)
        -> {ok, MergeSha} | {error, term()} when
    ReviewBranch :: binary(),
    TargetBranch :: binary(),
    ChangeId :: binary(),
    Merger :: d_user(),
    MergeSha :: binary().
merge_change(Pid, ReviewBranch, TargetBranch, ChangedId, Merger) ->
    gen_server:call(Pid, {merge_change, ReviewBranch, TargetBranch, ChangedId, Merger}).

%% Internals

%% @doc Start individual process for project to handle working tree
%% updates and merges in the DefaultDir defined by the delivery-sys.config.erb
%% parameter 'deliv_git_working_tree_dir' for the delivery environment.
-spec start_link(pid(), d_project()) -> {ok, pid()} | ignore.
start_link(StartingProcess, Proj) ->
    gen_server:start_link(?MODULE, {StartingProcess, Proj}, []).

-spec init({pid(), d_project()}) -> {ok, #state{}} | ignore.
init({StartingProcess, Proj}) ->
    %% try to register with gproc
    Result = case deliv_git_working_tree_sup:register_worker(Proj) of
        ok ->
            Dir = deliv_project:working_tree_path(Proj),
            State = #state{proj=Proj, dir=Dir},
            {ok, State};
        {error, already_registered} ->
            %% we've been beaten to it, just abort
            ignore
    end,
    %% no matter what, at this point, there should be a registered
    %% gen_server for that project
    deliv_git_working_tree_sup:notify_starting_process(StartingProcess, Proj),
    Result.

handle_call(clone_repo, _From, #state{proj=Proj, dir=Dir} = State) ->
    ok = clone_repo_i(Proj, Dir),
    {reply, ok, State};
handle_call({update_repo, Branch}, _From, #state{proj=Proj, dir=Dir} = State) ->
    Reply = update_repo_i(Proj, Branch, Dir),
    {reply, Reply, State};
handle_call({merge_change, ReviewBranch, TargetBranch, ChangeId, Merger}, _From,
            #state{proj=Proj, dir=Dir} = State) ->
    Reply = merge_change_i(ReviewBranch, TargetBranch, Proj, Dir, ChangeId, Merger),
    {reply, Reply, State};
handle_call(Msg, _From, #state{} = State) ->
    chef_log:error("Unexpected call in ~s: ~p",
                    [?MODULE, Msg]),
    {noreply, State}.

handle_cast(Msg, #state{} = State) ->
    chef_log:error("Unexpected cast in ~s: ~p",
                    [?MODULE, Msg]),
    {noreply, State}.

handle_info(Msg, #state{} = State) ->
    chef_log:error("Unexpected info in ~s: ~p",
                    [?MODULE, Msg]),
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.

%% Utils

-spec clone_repo_i(Proj, Dir) -> ok when
      Proj :: d_project(),
      Dir :: binary().
clone_repo_i(Proj, Dir) ->
    %% Get Repo path from Proj
    Repo = deliv_project:repo_path(Proj),

    chef_log:info("Checking out Repo: ~s to Dir: ~s", [Repo, Dir]),

    %% Clone Dir from Repo if it does not already exist
    case already_cloned(Dir) of
        true ->
            ok;
        false ->
            PathWithTrailingSlash = <<Dir/binary, "/">>,
            filelib:ensure_dir(PathWithTrailingSlash),
            %% git clone Repo Dir
            CloneCmd = ["clone", Repo, Dir],
            %% TODO determine error handling and response
            {ok, _} = deliv_git:run_git(clone_repo_i, Dir, CloneCmd),
            ok
    end.

%% @doc Returns true iff the repo has already been cloned in said dir
%% TODO: do we want to do that more robust?
-spec already_cloned(binary()) -> boolean().
already_cloned(Dir) ->
    filelib:is_dir(Dir).

-spec update_repo_i(Proj, Branch, Dir) -> ok | {error, term()} when
      Proj :: d_project(),
      Branch :: binary(),
      Dir :: binary().
update_repo_i(Proj, Branch, Dir) ->
    Repo = deliv_project:repo_path(Proj),

    chef_log:info("Updating Repo: ~s on Branch: ~s in Dir: ~s",
                   [Repo, Branch, Dir]),

    %% Make sure repo is already checked out
    ok = clone_repo_i(Proj, Dir),

    %% and make sure it's clean
    clean_repo(Dir),

    %% git fetch origin Branch
    FetchCmd = ["fetch", "origin", Branch],
    {ok, _} = deliv_git:run_git(handle_call_update_repo_fetch, Dir, FetchCmd),

    %% git checkout Branch
    CheckoutCmd = ["checkout", Branch],
    {ok, _} = deliv_git:run_git(handle_call_update_repo_checkout, Dir, CheckoutCmd),

    %% git reset --hard origin/Branch
    ResetCmd = ["reset", "--hard", <<"origin/", Branch/binary>>],
    {ok, _} = deliv_git:run_git(handle_call_update_repo_reset, Dir, ResetCmd),

    ok.

-spec merge_change_i(binary(), binary(), d_project(), binary(), binary(), d_user())
        -> {ok, MergeSha :: binary()}.
merge_change_i(ReviewBranch, TargetBranch, Proj, Dir, ChangeId, Merger) ->
    Repo = deliv_project:repo_path(Proj),

    %% TODO? add an event generator here, and add a listener that logs
    %% the message below after the event is sent, and also make sure it is
    %% part of wherever our audit log is going to be.
    chef_log:info("Merge ReviewBranch: ~s of Repo: ~s into TargetBranch: ~s in Dir: ~s",
                   [ReviewBranch, Repo, TargetBranch, Dir]),

    %% Make sure repo is already updated (and checked out) and
    %% switched to proper Branch
    ok = update_repo_i(Proj, TargetBranch, Dir),

    FetchCmd = ["fetch", "origin", ReviewBranch],
    {ok, _} = deliv_git:run_git(handle_call_merge_change_fetch, Dir, FetchCmd),

    Msg = generate_commit_msg(ReviewBranch, TargetBranch, ChangeId, Merger),
    MergeCmd = ["merge", "--no-ff", "-m", Msg, "FETCH_HEAD"],
    case deliv_git:run_git(handle_call_merge_change_merge,
                           Dir,
                           MergeCmd,
                           git_author_env()) of
        {ok, _} ->
            PushCmd = ["push", "origin", TargetBranch],
            %% this is to deactivate the update hook
            %% TODO: this is a ugly hack!
            %% see the comments in the update hook's script for more details
            Env = [{"DELIV_MERGE_NO_HOOK", "1"}],
            {ok, _} = deliv_git:run_git(handle_call_merge_change_push, Dir, PushCmd, Env),

            %% all good, now we retrieve the actual merge SHA
            MergeSha = deliv_git:rev_parse("HEAD", Dir),

            {ok, MergeSha};
        {error, _Why} -> {error, feature_branch_merge_failed}
    end.

git_author_env() ->
    %% For now, the Delivery Server is both author and committer of
    %% our merge commits. Eventually, we may want to have the person
    %% that clicked the merge button be the author, but we'll need to
    %% have sensible fallbacks in case names and / or emails are not
    %% properly set (we currently don't require these).
    %%
    %% Alternatively, we could specify the committer information as
    %% part of Delivery server setup (i.e., using Chef).
    %%
    %% See
    %% http://git-scm.com/book/en/v2/Git-Internals-Environment-Variables#Committing
    %% and also http://git-scm.com/docs/git-commit (scroll to --author
    %% documentation; this can either be a name or a pattern, in which
    %% the first matching name from the history is used)
    [
     {"GIT_COMMITTER_NAME",  "Delivery Server"},
     {"GIT_COMMITTER_EMAIL", "delivery@delivery.local"},
     {"GIT_AUTHOR_NAME",     "Delivery Server"},
     {"GIT_AUTHOR_EMAIL",    "delivery@delivery.local"}
    ].

%% @doc Generate the merge commit message
%% TODO: Finalize the format and decide who the merge commit author is (Delivery
%% or who signed off on it (hit accept))
-spec generate_commit_msg(binary(), binary(), binary(), d_user()) -> binary().
generate_commit_msg(ReviewBranch, TargetBranch, ChangeId, Merger) ->
    IOList = ["Merged change ", ChangeId, "\n\n",
              "From review branch ", ReviewBranch, " into ", TargetBranch, "\n\n",
              "Signed-off-by: ", user_to_iodata(Merger), "\n\n",
              get_reviewers(ChangeId)],
    erlang:iolist_to_binary(IOList).

-spec get_reviewers(binary()) -> iodata().
get_reviewers(ChangeId) ->
    %% TODO what if deliv_user:commenters_for_change fails?
    lists:map(
        fun(Reviewer) -> ["Reviewed-by: ", user_to_iodata(Reviewer), "\n"] end,
        deliv_user:commenters_for_change(ChangeId)
    ).

-spec user_to_iodata(d_user()) -> iodata().
user_to_iodata(User) ->
    UserName = deliv_user:getval(name, User),
    Email = case deliv_user:getval(email, User) of
        undefined -> <<"(no email given)">>;
        E -> E
    end,
    [UserName, " <", Email, ">"].

-spec clean_repo(binary()) -> ok.
clean_repo(Dir) ->
    ResetCmd = ["reset", "--hard"],
    {ok, _} = deliv_git:run_git(clean_repo_reset, Dir, ResetCmd),
    CleanCmd = ["clean", "-fxd"],
    {ok, _} = deliv_git:run_git(clean_repo_clean, Dir, CleanCmd),
    ok.
