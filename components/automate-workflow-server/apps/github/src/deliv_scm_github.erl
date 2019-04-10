%% @doc SCM Module: Github
%% @see deliv_scm
-module(deliv_scm_github).
-behaviour(deliv_scm).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

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
patchset_metadata_ejson(_Scope, _Patchset) ->
    {ok, {[], {[]}, []}}.

%% @see deliv_scm:clone_url/3
-spec clone_url(iodata(), d_patchset(), d_common_scope()) -> binary().
clone_url(_Username, Patchset, _Scope) ->
    deliv_github_patchset:clone_url(Patchset).

%% @see deliv_scm:clone_url/4
-spec clone_url(iodata(), d_patchset(), d_common_scope(), boolean()) -> binary().
clone_url(Username, Patchset, Scope, false) ->
    clone_url(Username, Patchset, Scope);
clone_url(_Username, _Patchset, _Scope, true) ->
    erlang:throw(<<"Github V1 intergration is not supported in FIPS mode. Please upgrade to Github V2 intergration if you wish to run with FIPS enabled on the Automate Server kernal.">>).

%% @see deliv_scm:load_config_for_patchset/2
-spec load_config_for_patchset(d_patchset(), d_common_scope()) -> {ok, tuple()} |
                                                                  {error, invalid_config | atom() | tuple()}.
load_config_for_patchset(Patchset, Scope) ->
    FilePath = deliv_proj_config:project_config_file(),
    CommitSha = deliv_patchset:getval(sha, Patchset),

    case deliv_github_api:get_file_contents(Scope, FilePath, CommitSha) of
        {ok, ProjectConfig} ->
            deliv_proj_config:validate(ProjectConfig);
        {error, Why} = Err ->
            [Ent, Org, Proj, Pipe] = deliv_scopes:'#get'(scoping_names, Scope),
            chef_log:error("Could not load the project's config for "
                            "~s/~s/~s/~s at sha ~s : ~p",
                            [Ent, Org, Proj, Pipe, CommitSha, Why]),
            Err
    end.

%% @see deliv_scm:patchset_branch/3
%% @doc Return the branch name that builders should use
-spec patchset_branch(Change :: d_change(), Patchset :: d_patchset(), Scope :: d_common_scope()) -> binary().
patchset_branch(Change, _Patchset, _Scope) ->
    deliv_change:getval(feature_branch, Change).

%% @see deliv_scm:merge_feature_branch/3
%% @doc Merge an existing feature branch on github and return the merge sha
-spec merge_feature_branch(d_common_scope(), d_patchset(), d_user()) -> {ok, string()} | {error, _Why}.
merge_feature_branch(Scope, Patchset, Approver) ->
    PatchsetId = deliv_patchset:getval(id, Patchset),
    case deliv_github_patchset:fetch_by_patchset_id(PatchsetId) of
        [GithubPatchset] ->
            Json = chef_json:decode(deliv_github_patchset:getval(payload, GithubPatchset)),
            PullRequestNumber = deliv_github_pull_request:pull_request_number(Json),

            FeatureBranch = deliv_github_pull_request:feature_branch(Json),
            TargetBranch = deliv_github_pull_request:pipe_name(Json),
            ChangeId = deliv_patchset:getval(change_id, Patchset),
            Sha = deliv_patchset:getval(sha, Patchset),
            CommitMessage = generate_commit_msg(FeatureBranch, TargetBranch, ChangeId, Approver),

            deliv_github_api:merge_pull_request(Scope, PullRequestNumber, Sha, CommitMessage);
        [] -> {error, github_patchset_not_found}
     end.

%% private
%% @doc This is a temporary function until we clean out the code path in deliv_git_working_tree
%% It returns a reasonable merge message
generate_commit_msg(ReviewBranch, TargetBranch, ChangeId, Merger) ->
    IOList = ["Merged change ", ChangeId, "\n\n",
              "From review branch ", ReviewBranch, " into ", TargetBranch, "\n\n",
              "Signed-off-by: ", user_to_iodata(Merger)],
    erlang:iolist_to_binary(IOList).

%% private
%% TODO(jmink) This is a copy paste from deliv_git_working_tree which can be deleted once we clean up
%% generate_commit_msg
-spec user_to_iodata(d_user()) -> iodata().
user_to_iodata(User) ->
    UserName = deliv_user:getval(name, User),
    Email = case deliv_user:getval(email, User) of
        undefined -> <<"(no email given)">>;
        E -> E
    end,
    [UserName, " <", Email, ">"].

%% @see deliv_scm:delete_feature_branch/2
-spec delete_feature_branch(d_common_scope(), d_patchset()) -> {ok, binary()} |
                                                               {error, delete_branch_errors() }.
delete_feature_branch(Scope, Patchset) ->
    PatchsetId = deliv_patchset:getval(id, Patchset),
    case deliv_github_patchset:fetch_by_patchset_id(PatchsetId) of
        [GithubPatchset] ->
            Json = chef_json:decode(deliv_github_patchset:getval(payload, GithubPatchset)),
            Branch = deliv_github_pull_request:feature_branch(Json),

            case deliv_github_api:delete_branch(Scope, Branch) of
                ok -> {ok, Branch};
                {error, _Why} -> {error, feature_branch_delete_failed}
            end;
        [] -> {error, github_patchset_not_found}
     end.

-spec process_new_patchset(d_patchset(), binary(), binary(), binary(), binary(), #proj_coordinates{}) -> {error, binary()}.
process_new_patchset(_Patchset, _ProjDir, _PipeName, _FeatBranchName, _RawBranchName, _Coords) ->
    {error, <<"not implemented">>}.

%% This module is the GitHubV1 implementation. It will be removed when V2
%% is complete.
%% This should never be called on this module, but it is required by the behaviour.
pull_request_description(_) -> <<"">>.
