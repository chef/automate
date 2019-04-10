%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et

%% @doc EUnit helper functions for concisely creating test data for
%% database-dependent tests.
%%
%% Example Usage:
%%
%%   eu_data:with_enterprise(<<"deliv_changeset_test_enterprise">>,
%%     eu_data:with_organization(<<"deliv_changeset_test_organization">>,
%%       eu_data:with_project(<<"deliv_changeset_test_project">>,
%%         eu_data:with_pipeline(<<"master">>,
%%           fun(Enterprise, Organization, Project, Pipeline) ->
%%             %% Assert something fun in here
%%             %% Or return data as part of a Hoax parameterized fixture
%%           end)))).
%%
-module(eu_data).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

with_enterprise(EntName, Fun) ->
    Fun(fetch_or_create_enterprise(EntName)).

fetch_or_create_enterprise(EntName) ->
    [Enterprise] = case deliv_enterprise:fetch(EntName) of
        {error, not_found} ->
            deliv_enterprise:insert(EntName);
        {ok, Ent} ->
            [Ent]
    end,
    Enterprise.

with_organization(OrgName, Fun) ->
    fun(Enterprise) ->
            Fun(Enterprise, fetch_or_create_organization(Enterprise, OrgName))
    end.

fetch_or_create_organization(Enterprise, OrgName) ->
   EntName = deliv_enterprise:getval(name, Enterprise),
   [Organization] = case deliv_organization:fetch(EntName, OrgName) of
                        {error, not_found} ->
                            deliv_organization:insert(EntName, OrgName);
                        {ok, Org} ->
                            [Org]
                    end,
   Organization.

with_project(ProjName, Fun) ->
    fun(Enterprise, Organization) ->
            Fun(Enterprise, Organization, fetch_or_create_project(Enterprise, Organization, ProjName))
    end.

fetch_or_create_project(Enterprise, Organization, ProjName) ->
   EntName = deliv_enterprise:getval(name, Enterprise),
   OrgName = deliv_organization:getval(name, Organization),
   [Project] = case deliv_project:fetch(EntName, OrgName, ProjName) of
                   {error, not_found} ->
                       deliv_project:insert(EntName, OrgName, ProjName);
                   {ok, Proj} ->
                       [Proj]
               end,
   Project.

with_bitbucket_project(ProjName, PipeName, BitbucketKey, RepoName, Fun) ->
   fun(Enterprise, Organization) ->
           Fun(Enterprise, Organization, fetch_or_create_bitbucket_project(Enterprise, Organization, ProjName, PipeName, BitbucketKey, RepoName))
   end.

fetch_or_create_bitbucket_project(Enterprise, Organization, ProjName, PipeName, BitbucketKey, RepoName) ->
  EntName = deliv_enterprise:getval(name, Enterprise),
  OrgName = deliv_organization:getval(name, Organization),
  [Project] = case deliv_project:fetch(EntName, OrgName, ProjName) of
                  {error, not_found} ->
                      Params = [EntName, OrgName, ProjName, PipeName, bitbucket_scm, BitbucketKey, RepoName],
                      deliv_db:qfetch(deliv_project, create_bitbucket_scm_project, Params);
                  {ok, Proj} ->
                      [Proj]
              end,
  Project.

with_githubV2_project(ProjName, PipeName, GHOwner, RepoName, Fun) ->
    fun(Enterprise, Organization) ->
            Fun(Enterprise, Organization, fetch_or_create_githubV2_project(Enterprise, Organization, ProjName, PipeName, GHOwner, RepoName))
    end.

fetch_or_create_githubV2_project(Enterprise, Organization, ProjName, PipeName, GHOwner, RepoName) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    OrgName = deliv_organization:getval(name, Organization),
    [Project] = case deliv_project:fetch(EntName, OrgName, ProjName) of
                  {error, not_found} ->
                      Params = [EntName, OrgName, ProjName, PipeName, github_scm, GHOwner, RepoName],
                      deliv_db:qfetch(deliv_project, create_scm_project, Params);
                  {ok, Proj} ->
                      [Proj]
              end,
    Project.

with_github_project(ProjName, PipeName, GHOwner, RepoName, Fun) ->
    fun(Enterprise, Organization) ->
            Fun(Enterprise, Organization, fetch_or_create_github_project(Enterprise, Organization, ProjName, PipeName, GHOwner, RepoName))
    end.

fetch_or_create_github_project(Enterprise, Organization, ProjName, PipeName, GHOwner, RepoName) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    OrgName = deliv_organization:getval(name, Organization),
    [Project] = case deliv_project:fetch(EntName, OrgName, ProjName) of
                  {error, not_found} ->
                      Params = [EntName, OrgName, ProjName, PipeName, deliv_scm_github, GHOwner, RepoName],
                      deliv_db:qfetch(deliv_project, create_scm_project, Params);
                  {ok, Proj} ->
                      [Proj]
              end,
    Project.

with_pipeline(PipeName, Fun) ->
    fun(Enterprise, Organization, Project) ->
            Fun(Enterprise, Organization, Project, fetch_or_create_pipeline(Enterprise, Organization, Project, PipeName))
    end.

fetch_or_create_pipeline(Enterprise, Organization, Project, PipeName) ->
   EntName = deliv_enterprise:getval(name, Enterprise),
   OrgName = deliv_organization:getval(name, Organization),
   ProjName = deliv_project:getval(name, Project),
   [Pipeline] = case deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName) of
                    {error, not_found} ->
                        deliv_pipeline:insert(EntName, OrgName, ProjName, PipeName);
                    {ok, Pipe} ->
                        [Pipe]
                end,
   Pipeline.

with_change(UserName, PipelineName, FeatureBranch, Fun) ->
    fun(Enterprise, Organization, Project) ->
            Pipeline = fetch_or_create_pipeline(Enterprise, Organization, Project, PipelineName),
            User = fetch_or_create_user(Enterprise, UserName),
            Patchset = create_patchset(Enterprise, User, Organization, Project,
                                       Pipeline, FeatureBranch),
            Change = change_from_patchset(Patchset),
            Fun(Enterprise, Organization, Project, Change)
    end.

create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    UserName = deliv_user:getval(name, User),
    OrgName = deliv_organization:getval(name, Organization),
    ProjName = deliv_project:getval(name, Project),
    PipeName = deliv_pipeline:getval(name, Pipeline),

    %% Create a random 40-character string to serve as a SHA for this
    %% patchset; at the moment, this is an implementation detail that
    %% we don't really care about for the tests
    Sha = chef_utils:random_hex_string(20),

    [Patchset] = deliv_patchset:new(EntName,
                                    UserName,
                                    OrgName,
                                    ProjName,
                                    PipeName,
                                    FeatureBranch,
                                    Sha),
    Patchset.

change_from_patchset(Patchset) ->
    ChangeId = deliv_patchset:getval(change_id, Patchset),
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),
    Change.

change_with_closed_changeset_from_patchset(Patchset, User, Deps) ->
    Change = change_from_patchset(Patchset),
    ChangeId = deliv_change:getval(id, Change),
    deliv_changeset:add_to_changeset(Change, Deps),
    deliv_changeset:close_changeset(ChangeId, User),
    Change.

create_patchset_changed_file(Patchset, Status, FileName, Inserts, Deletes) ->
    PatchsetId = deliv_patchset:getval(id, Patchset),
    Record = deliv_patchset_changed_file:'#new'(),
    ChangedFile = deliv_patchset_changed_file:setvals([
                                                       {patchset_id, PatchsetId},
                                                       {status, Status},
                                                       {file, FileName},
                                                       {inserts, Inserts},
                                                       {deletes, Deletes}
                                                      ], Record),
    deliv_db:insert(ChangedFile),
    ChangedFile.

change_on_consumer_of_pipeline(Enterprise, Organization, DepPipeline, User, ProjName) ->
    Project = fetch_or_create_project(Enterprise, Organization, ProjName),
    Pipeline = fetch_or_create_pipeline(Enterprise, Organization, Project, <<"master">>),

    FeatureBranch = chef_utils:join_binaries([<<"branch_on">>, ProjName], <<"_">>),
    Patchset = create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
    DepId = deliv_pipeline:getval(id, DepPipeline),
    change_with_closed_changeset_from_patchset(Patchset, User, [DepId]).

fetch_or_create_user(Enterprise, UserName) ->
  fetch_or_create_user(Enterprise, UserName, <<"internal">>).
fetch_or_create_user(Enterprise, UserName, UserType) ->
   EntName = deliv_enterprise:getval(name, Enterprise),
    [User] = case deliv_user:fetch(EntName, UserName) of
                 {error, not_found} ->
                     deliv_user:insert(EntName,
                                       [{name, UserName},
                                        {first_name, <<"Deliv">>},
                                        {last_name, <<"Changeset">>},
                                        {email, <<"changeset@chef.io">>},
                                        {telemetry_enabled, true},
                                        {user_type, UserType}]);
                 {ok, FoundUser} ->
                     [FoundUser]
             end,
   User.

proj_coordinates(Enterprise, Organization, Project) ->
   EntName = deliv_enterprise:getval(name, Enterprise),
   OrgName = deliv_organization:getval(name, Organization),
   ProjName = deliv_project:getval(name, Project),
   #proj_coordinates{ent_name = EntName,
                     org_name = OrgName,
                     proj_name = ProjName}.
