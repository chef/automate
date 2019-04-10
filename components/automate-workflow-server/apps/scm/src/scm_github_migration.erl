-module(scm_github_migration).

-include_lib("delivery/include/deliv_coordinates.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-export([migrate/1]).

-spec migrate(list()) -> {ok | error, binary()}.
%% Migrate enterprise (migrates all orgs with ghv1 projects)
migrate([EntNameStr]) ->
    EntName = chef_utils:to_bin(EntNameStr),
    MigratedOrgs = [ migrate([EntName, OrgName]) || OrgName <- scm_github_migration_helpers:list_orgs(EntName) ],
    Success = chef_utils:to_bin([ X || {ok, X} <- MigratedOrgs]),
    Failure = chef_utils:to_bin([ X || {error, X} <- MigratedOrgs]),
    case Failure of
        <<>> -> {ok, <<"No errors encountered attempting to migrate ", EntName/binary, " to GitHubV2.\n",
                Success/binary>>};
        _ -> {error, <<"Migration failed for ", EntName/binary, ".\n",
                Failure/binary, Success/binary>>}
    end;
%% Migrate org (includes all ghv1 projects in that org)
migrate([_EntName, _OrgName] = Args) ->
    [EntName, OrgName] = [ chef_utils:to_bin(A) || A <- Args ],
      MigratedProjects = [ migrate([EntName, OrgName, ProjectName]) || ProjectName <- scm_github_migration_helpers:list_ghv1_projects(EntName, OrgName) ],
      Success = chef_utils:to_bin([ X || {ok, X} <- MigratedProjects]),
      Failure = chef_utils:to_bin([ X || {error, X} <- MigratedProjects]),
      case {Success, Failure} of
          {<<>>, <<>>} -> {ok, <<"No GitHubV1 projects found for ", EntName/binary, "/", OrgName/binary, ". Nothing to do.\n">>};
          {_, <<>>} -> {ok, <<"Successfully migrated ", EntName/binary, "/", OrgName/binary, " to GitHubV2.\n",
                                 Success/binary>>};
          {_, _} -> {error, <<"Migration failed for ", EntName/binary, "/", OrgName/binary, ".\n",
                                 Failure/binary, Success/binary>>}
    end;
%% Migrate project
migrate([_EntName, _OrgName, _ProjName] = Args) ->
    [EntName, OrgName, ProjName] = [ chef_utils:to_bin(A) || A <- Args ],
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    FetchEnt = deliv_enterprise:fetch(EntName),
    FetchOrg = deliv_organization:fetch(EntName, OrgName),
    FetchProj = deliv_project:fetch(EntName, OrgName, ProjName),
    try
      DProj = scm_github_migration_helpers:validate_input(FetchEnt, FetchOrg, FetchProj),
      scm_github_migration_helpers:validate_project_scm_module(DProj),
      scm_github_migration_helpers:validate_no_open_changes(EntName, OrgName, ProjName),
      scm_github_migration_helpers:validate_github_scm_configured(EntName),
      scm_github_migration_helpers:validate_automate_has_push_permissions(Coords),
      WebhookIds = scm_github_migration_helpers:list_project_webhooks(Coords),
      scm_github_migration_helpers:delete_webhooks(Coords, WebhookIds),
      scm_github_migration_helpers:validate_no_local_git_repo(Coords),
      scm_github_migration_helpers:initialize_local_git_repo(Coords),
      scm_github_migration_helpers:set_scm_module_to_new_github(DProj),
      SuccessMessage = <<"Successfully migrated ", EntName/binary, "/", OrgName/binary, "/",
              ProjName/binary, " to GitHubV2.\n">>,
      Message = case WebhookIds of
          [] -> <<"WARNING: No webhooks were deleted. ",
                  "Please ensure webhooks are removed from the GitHub repository.\n",
                  SuccessMessage/binary>>;
          _ -> SuccessMessage
     end,
    {ok, Message}
    catch
        throw:Reason ->
            Why = chef_utils:to_bin(Reason),
            ErrorMessage = <<"Migration failed for ", EntName/binary, "/", OrgName/binary, "/",
                        ProjName/binary, ": ", Why/binary, ".\n">>,
            {error, ErrorMessage}
    end.
