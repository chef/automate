%% @doc SCM Module: GitHub
%% This module uses the delegation patern around deliv_scm_local to add syncing
%% with github and creating prs in github.
%% @see deliv_scm
-module(github_scm).
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
patchset_metadata_ejson(Scope, Patchset) ->
    deliv_scm_local:patchset_metadata_ejson(Scope, Patchset).

%% @see deliv_scm:clone_url/3
-spec clone_url(iodata(), d_patchset(), d_common_scope()) -> binary().
clone_url(Username, Patchset, Scope) ->
    deliv_scm_local:clone_url(Username, Patchset, Scope).

%% @see deliv_scm:clone_url/4
-spec clone_url(iodata(), d_patchset(), d_common_scope(), boolean()) -> binary().
clone_url(Username, Patchset, Scope, FipsMode) ->
    deliv_scm_local:clone_url(Username, Patchset, Scope, FipsMode).

%% @see deliv_scm:load_config_for_patchset/2
-spec load_config_for_patchset(d_patchset(), d_common_scope()) -> {ok, tuple()} |
                                                                  {error, invalid_config | atom() | tuple()}.
load_config_for_patchset(Patchset, Scope) ->
    deliv_scm_local:load_config_for_patchset(Patchset, Scope).

%% @see deliv_scm:patchset_branch/3
%% @doc Return the branch name that builders should use
-spec patchset_branch(Change :: d_change(), Patchset :: d_patchset(), Scope :: d_common_scope()) -> binary().
patchset_branch(Change, Patchset, Scope) ->
    deliv_scm_local:patchset_branch(Change, Patchset, Scope).

%% @see deliv_scm:merge_feature_branch/3
%% @doc Merge an existing feature branch on github and return the merge sha
-spec merge_feature_branch(d_common_scope(), d_patchset(), d_user()) -> {ok, binary()}.
merge_feature_branch(Scope, Patchset, Approver) ->
    case deliv_scm_local:merge_feature_branch(Scope, Patchset, Approver) of
        {ok, MergeSha} ->
            [EntName, OrgName, ProjName, PipeName]
                  = deliv_scopes:'#get'([ent_name, org_name, proj_name, pipe_name], Scope),
            Coords = #proj_coordinates{ ent_name = EntName, org_name = OrgName, proj_name = ProjName},
            chef_log:info("Pushing pipeline ~s of project ~s/~s/~s to GitHub asynchronously...", [PipeName, EntName, OrgName, ProjName]),
            scm_git_client:async_force_push(PipeName, PipeName, Coords),
            {ok, MergeSha};
        {error, _Why} = Error -> Error
    end.

%% @see deliv_scm:delete_feature_branch/2
-spec delete_feature_branch(d_common_scope(), d_patchset()) -> {ok, binary()} |
                                                               {error, delete_branch_errors() }.
delete_feature_branch(Scope, Patchset) ->
    [EntName, OrgName, ProjName]
      = deliv_scopes:'#get'([ent_name, org_name, proj_name], Scope),
    Coords = #proj_coordinates{ ent_name = EntName, org_name = OrgName, proj_name = ProjName},

    ChangeId = deliv_patchset:getval(change_id, Patchset),
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),

    RemoteBranch = deliv_change:getval(feature_branch, Change),

    chef_log:info("Deleting branch ~s of project ~s/~s/~s on GitHub asynchronously...",
                   [RemoteBranch, EntName, OrgName, ProjName]),

    scm_git_client:async_delete_branch(RemoteBranch, Coords),

    deliv_scm_local:delete_feature_branch(Scope, Patchset).

%% @see deliv_scm:process_new_patchset/6
-spec process_new_patchset(d_patchset(), binary(), binary(), binary(), binary(), #proj_coordinates{}) -> {ok, binary()} | {error, binary()}.
process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords) ->
    {ok, Msg} = deliv_scm_local:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords),

    LatestBranch = chef_utils:to_bin(deliv_git:feature_branch(PipeName, FeatBranchName)),
    case scm_git_client:force_push(LatestBranch, FeatBranchName, Coords) of
        {ok, _} ->
            case scm_github_rest_api:ensure_pull_request(Patchset, FeatBranchName, PipeName, Coords) of
                {ok, BodyResponse} ->
                    PRUrl = ej:get({<<"html_url">>}, BodyResponse),
                    PRId = ej:get({<<"number">>}, BodyResponse),
                    scm_change:save(deliv_patchset:getval(change_id, Patchset), PRId, PRUrl),
                    {ok, <<Msg/binary, " GitHub Pull Request: ", PRUrl/binary>>};
                {exists, PRMsg} ->
                    {ok, <<Msg/binary, " " , PRMsg/binary>>};
                {error, Why} ->
                    {ok, <<Msg/binary, " " , Why/binary>>}
            end;
        {error, {git_failed, {_, Why}}} ->
            {ok, <<Msg/binary, " Failed to push branch to GitHub: ", Why/binary>>};
        {error, {git_cmd_failed, {_, _, Why}}} ->
            {ok, <<Msg/binary, " Failed to push branch to GitHub: ", Why/binary>>};
        {error, {git_cmd_error, {_, _, Why}}} ->
            {ok, <<Msg/binary, " Failed to push branch to GitHub: ", Why/binary>>};
        {error, _} ->
            {ok, <<Msg/binary, " Failed to push branch to GitHub.">>}
    end.

%% @see deliv_scm:pull_request_description/1
pull_request_description(PRID) ->
    <<"GitHub Pull Request (#", (integer_to_binary(PRID))/binary, ")">>.
