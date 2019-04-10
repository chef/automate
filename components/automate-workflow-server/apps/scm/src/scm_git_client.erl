%% @doc Client module for Bitbucket git operations. It is assumed that all
%% of the operations described in this module will be interacting with a
%% Bitbucket remote git repository.
-module(scm_git_client).

-include_lib("delivery/include/deliv_coordinates.hrl").

-export([
         async_delete_branch/2,
         async_force_push/3,
         delete_branch/2,
         force_push/3
        ]).

%% @doc Asynchronously pushes `SourceBranch` as `DestBranch` on the configured
%% Bitbucket instance for the Enterprise in the `ProjCoords`. Does not re-try.
-spec async_force_push(binary(), binary(), #proj_coordinates{}) -> ok.
async_force_push(SourceBranch, DestBranch, Coords) ->
    scm_git_worker:force_push(SourceBranch, DestBranch, Coords).

%% @doc Asynchronously deletes `DestBranch` on the configured Bitbucket instance
%% for the Enterprise in the `ProjCoords`.
-spec async_delete_branch(binary(), #proj_coordinates{}) -> ok.
async_delete_branch(DestBranch, Coords) ->
    scm_git_worker:delete_branch(DestBranch, Coords).

%% @doc Pushes `SourceBranch` as `DestBranch` on the configured Bitbucket instance
%% for the Enterprise in the `ProjCoords`.
-spec force_push(binary(), binary(), #proj_coordinates{}) -> {ok, term()} | {error, term()}.
force_push(SourceBranch, DestBranch, Coords = #proj_coordinates{ent_name = EntName,
                                                                org_name = OrgName,
                                                                proj_name = ProjName}) ->
    {ok, GitUrl} = scm_repo:url(Coords),
    GitCmd = [<<"push">>, <<"--force">>, GitUrl, <<SourceBranch/binary, ":", DestBranch/binary>>],

    TrustedCertsFile = delivery_app:get_env(trusted_certificates_file),

    {ok, RepoPath} = deliv_project:repo_path(EntName, OrgName, ProjName),
    deliv_git:run_git(bitbucket_scm, RepoPath, GitCmd, [{"GIT_SSL_CAINFO", TrustedCertsFile}]).

%% @doc Deletes `DestBranch` on the configured Bitbucket instance
%% for the Enterprise in the `ProjCoords`.
-spec delete_branch(binary(), #proj_coordinates{}) -> {ok, term()} | {error, term()}.
delete_branch(DestBranch, Coords) ->
    force_push(<<>>, DestBranch, Coords).
