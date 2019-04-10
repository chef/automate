%% @doc SCM Module: Local Delivery Git Server
%% @see deliv_scm
-module(deliv_scm_local).
-behaviour(deliv_scm).

-include("deliv_types.hrl").
-include("deliv_coordinates.hrl").

-export([
         patchset_metadata_ejson/2,
         clone_url/3,
         clone_url/4,
         load_config_for_patchset/2,
         patchset_branch/3,
         merge_feature_branch/3,
         delete_feature_branch/2,
         process_new_patchset/6,
         pull_request_description/1
        ]).

%% @see deliv_scm:patchset_metadata_ejson/2
-spec patchset_metadata_ejson(d_common_scope(), d_patchset()) -> {ok, {json(), json(), json()}} |
                                                                 {error, term()}.
patchset_metadata_ejson(Scope, Patchset) ->
    [EntName, OrgName, ProjName,
     PipeName, ChangeId] = deliv_scopes:'#get'([ent_name, org_name, proj_name,
                                                pipe_name, change_id], Scope),
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),
    FeatureBranch = deliv_change:getval(feature_branch, Change),

    CommitsResults = deliv_patchset_commit:fetch(EntName, OrgName, ProjName,
                                                 ChangeId, PipeName, FeatureBranch, Patchset),
    DiffstatsResults = deliv_patchset_diffstat:fetch(EntName, OrgName, ProjName,
                                                     ChangeId, PipeName, FeatureBranch, Patchset),
    ChangedFilesResults = deliv_patchset_changed_file:fetch(EntName, OrgName,
                                                            ProjName, ChangeId, PipeName, FeatureBranch, Patchset),

    case {CommitsResults, DiffstatsResults, ChangedFilesResults} of
        {{ok, Commits}, {ok, Diffstats}, {ok, ChangedFiles}} ->
            CommitJson = [deliv_patchset_commit:to_json(Commit) || Commit <- Commits],
            DiffstatJson = deliv_patchset_diffstat:to_json(Diffstats),
            ChangedFilesJson = [deliv_patchset_changed_file:to_json(FileStatus) || FileStatus <- ChangedFiles],
            {ok, {CommitJson, DiffstatJson, ChangedFilesJson}};
        {{error, _CommitError} = Error, _Diffstats, _ChangedFiles} ->
            Error;
        {_Commits, {error, _DiffstatsError} = Error, _ChangedFiles} ->
            Error;
        {_Commits, _Diffstats, {error, _ChangedFilesError} = Error} ->
            Error
    end.


%% @see deliv_scm:clone_url/3
-spec clone_url(iodata(), d_patchset(), d_common_scope()) -> binary().
clone_url(Username, Patchset, Scope) ->
    clone_url(Username, Patchset, Scope, false).

-spec clone_url(iodata(), d_patchset(), d_common_scope(), boolean()) -> binary().
clone_url(Username, _Patchset, Scope, false) ->
    [EntName, OrgName, ProjName, _PipeName] = deliv_scopes:'#get'(scoping_names, Scope),
    deliv_git:uri(Username, EntName, OrgName, ProjName);
clone_url(Username, _Patchset, Scope, true) ->
    [EntName, OrgName, ProjName, _PipeName] = deliv_scopes:'#get'(scoping_names, Scope),
    deliv_git:fips_uri(Username, EntName, OrgName, ProjName).

%% @see deliv_scm:load_config_for_patchset/2
-spec load_config_for_patchset(d_patchset(), d_common_scope()) -> {ok, tuple()} |
                                                                  {error, invalid_config | atom() | tuple()}.
load_config_for_patchset(Patchset, Scope) ->
    [Ent, Org, Proj, Pipe] = deliv_scopes:'#get'(scoping_names, Scope),
    {ok, RepoPath} = deliv_project:repo_path(Ent, Org, Proj),
    CommitSha = deliv_patchset:getval(sha, Patchset),

    case deliv_git:file_at_sha(RepoPath, CommitSha, deliv_proj_config:project_config_file()) of
        {ok, FileBin} ->
            deliv_proj_config:validate(FileBin);
        {error, Why} = Err ->
            chef_log:error("Could not load the project's config for "
                            "~s/~s/~s/~s at ~s and sha ~s : ~p",
                            [Ent, Org, Proj, Pipe, RepoPath, CommitSha, Why]),
            Err
    end.

%% @see deliv_scm:patchset_branch/3
-spec patchset_branch(d_change(), d_patchset(), d_common_scope()) -> binary().
patchset_branch(Change, Patchset, Scope) ->
    FeatBranchName = deliv_change:getval(feature_branch, Change),
    [PipelineName] = deliv_scopes:'#get'([pipe_name], Scope),
    PSBranch = deliv_git:patchset_branch(PipelineName, FeatBranchName,
                                         deliv_patchset:getval(sequence_number, Patchset)),
    chef_utils:to_bin(PSBranch).

%% @see deliv_scm:delete_feature_branch/1
-spec delete_feature_branch(d_common_scope(), d_patchset()) -> {ok, term()} |
                                                               {error, delete_branch_errors()}.
delete_feature_branch(Scope, Patchset) ->
    [ChangeId, ProjectId] =
        deliv_scopes:'#get'([change_id, proj_id], Scope),

    {ok, Project} = deliv_project:fetch_by_id(ProjectId),
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),

    ProjDir = deliv_project:repo_path(Project),
    PatchsetBranch = patchset_branch(Change, Patchset, Scope),
    PatchsetBranchWithoutPatchNum = filename:join(lists:droplast(filename:split(PatchsetBranch))),
    ReviewBranch = filename:join(["refs", "heads", PatchsetBranch]),
    ReviewPrefix = filename:join(["refs", "heads", PatchsetBranchWithoutPatchNum]),

    deliv_notify:format("Change ~s deleted (~s).", [ChangeId, ReviewBranch]),

    {ok, BranchesBin} = deliv_git:run_git(delete_review_branch, ProjDir,
                                          ["for-each-ref",
                                           "--format=%(refname:short)",
                                           ReviewPrefix]),
    Branches = binary:split(BranchesBin, <<"\n">>, [global, trim]),
    DelCmd = ["branch", "-D"] ++ Branches,

    deliv_git:run_git(delete_review_branch2, ProjDir, DelCmd).

%% @see deliv_scm:merge_feature_branch/3
-spec merge_feature_branch(d_common_scope(), d_patchset(), d_user()) -> {ok, term()} |
                                                                        {error, _Why}.
merge_feature_branch(Scope, Patchset, Approver) ->
    [ChangeId, PipelineName, ProjectId]
        = deliv_scopes:'#get'([change_id, pipe_name, proj_id], Scope),
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),
    {ok, Project} = deliv_project:fetch_by_id(ProjectId),

    PatchsetBranch = patchset_branch(Change, Patchset, Scope),
    RepoPath = deliv_project:repo_path(Project),

    %% Check to make sure the pipeline branch exists, or we will
    %% be unable to merge
    case deliv_git:branch_exists(RepoPath, PipelineName) of
        branch_exists ->
            {ok, Pid} = deliv_git_working_tree_sup:get_worker(Project),
            deliv_git_working_tree:merge_change(Pid,
                                                PatchsetBranch,
                                                PipelineName,
                                                ChangeId,
                                                Approver);
        no_branch_exists ->
            {error, pipeline_branch_missing}
    end.

-spec process_new_patchset(d_patchset(), binary(), binary(), binary(), binary(), #proj_coordinates{}) -> {ok, binary()}.
process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName}) ->
    PatchsetNumber = deliv_patchset:getval(sequence_number, Patchset),
    ChangeId = deliv_patchset:getval(change_id, Patchset),
    ReviewBranch = deliv_git:patchset_branch(PipeName, FeatBranchName, PatchsetNumber),
    FeatureBranch = deliv_git:feature_branch(PipeName, FeatBranchName),

    MvCmd = ["branch", "--move", RawBranchName, ReviewBranch],
    {ok, _} = deliv_git:run_git(handle_new_patchset, ProjDir, MvCmd),

    ForceCmd = ["branch", "--force", FeatureBranch, ReviewBranch],
    {ok, _} = deliv_git:run_git(handle_new_patchset, ProjDir, ForceCmd),

    %% Get the git commit message
    %% git log --format=%B -1 ReviewBranch
    LogCmd = ["log", "--format=%B", "-1", ReviewBranch],
    {ok, CommitMsg} = deliv_git:run_git(handle_new_patchset, ProjDir, LogCmd),

    %% Set title and description from commit message. We use the same assumption
    %% as the current WebUI, which is to assume the 1st line of the 1st commit
    %% message is the title, and the rest of the 1st commit message is the
    %% description.

    %% We try to first parse the accepted standard of having the title followed
    %% by two newlines, then the description. If that is not followed, we simply
    %% assume the first line is the title, and the rest is the description.

    [Title, Description] = case binary:split(CommitMsg, <<"\n\n">>) of
                               [H, T] -> [H, T];
                               [CommitMsg] -> binary:split(CommitMsg, <<"\n">>)
                           end,

    {ok, Change} = deliv_change:fetch_by_id(ChangeId),

    NewChange = set_if_new(title, Title, Change),
    NewChange2 = set_if_new(description, Description, NewChange),

    {ok, NewChange2} = deliv_change:update(NewChange2),

    %% Trigger Verify Run. Revisit the way we are passing the sequence number.
    deliv_change:verify_patchset(ChangeId),

    URL = deliv_web_utils:make_web_url_for_change(EntName, OrgName,
                                                  ProjName, ChangeId),

    {ok, <<"Created new patchset\n", URL/binary, "\n">>}.

%% @private
%% @doc Sets the given field in change if it has not yet been set (undefined).
-spec set_if_new(atom(), any(), d_change()) -> d_change().
set_if_new(Field, Value, Change) ->
    case deliv_change:getval(Field, Change) of
        undefined ->
            deliv_change:setvals([{Field, Value}], Change);
        _ ->
           Change
    end.

%% This should never be called on this module, but it is required by the behaviour.
pull_request_description(_) -> <<"">>.
