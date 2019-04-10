-module(scm_github_migration_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

scm_github_migration_helpers_test_() ->
    [
     hoax:fixture(?MODULE, 'migrate_when_'),
     hoax:fixture(?MODULE, 'migrate_org_'),
     hoax:fixture(?MODULE, 'migrate_ent_')
    ].

migrate_when_project_does_not_exist_returns_error_not_found() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    ExpectedResponse = {error, <<"Migration failed for ent_name/org_name/proj_name: not_found.\n">>},

    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj) -> {error, not_found};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {error, not_found}) -> erlang:throw(not_found)
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org, Proj])),
    ?verifyAll.

migrate_when_project_scm_module_is_not_deliv_github_scm_throws_error() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    ExpectedResponse = {error, <<"Migration failed for ent_name/org_name/proj_name: else.\n">>},

    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj) -> {ok, proj_record};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record}) -> proj_record;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record) -> erlang:throw(else)
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org, Proj])),
    ?verifyAll.

migrate_when_no_open_changes_returns_changes_throws_error() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    ExpectedResponse = {error, <<"Migration failed for ent_name/org_name/proj_name: Open changes in the system.\n">>},

    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj) -> {ok, proj_record};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record}) -> proj_record;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj) -> erlang:throw(<<"Open changes in the system">>)
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org, Proj])),
    ?verifyAll.

migrate_when_no_scm_configured_throws_error() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    ExpectedResponse = {error, <<"Migration failed for ent_name/org_name/proj_name: No scm configured.\n">>},

    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj) -> {ok, proj_record};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record}) -> proj_record;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj) -> true;
                    scm_github_migration_helpers:validate_github_scm_configured(Ent) -> erlang:throw(<<"No scm configured">>)
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org, Proj])),
    ?verifyAll.

migrate_when_list_project_webhooks_fails_throws_error() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    Coords = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj},
    ExpectedResponse = {error, <<"Migration failed for ent_name/org_name/proj_name: responsebody.\n">>},

    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj) -> {ok, proj_record};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record}) -> proj_record;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj) -> true;
                    scm_github_migration_helpers:validate_github_scm_configured(Ent) -> true;
                    scm_github_migration_helpers:validate_automate_has_push_permissions(Coords) -> true;
                    scm_github_migration_helpers:list_project_webhooks(Coords) ->  erlang:throw(responsebody)
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org, Proj])),
    ?verifyAll.

migrate_when_list_project_webhooks_succeeds_but_no_webhooks_found_prints_warning() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    Coords = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj},
    ExpectedResponse = {ok, <<"WARNING: No webhooks were deleted. ",
                              "Please ensure webhooks are removed from the GitHub repository.\n",
                              "Successfully migrated ent_name/org_name/proj_name to ",
                              "GitHubV2.\n">>},

    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj) -> {ok, proj_record};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record}) -> proj_record;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj) -> true;
                    scm_github_migration_helpers:validate_github_scm_configured(Ent) -> true;
                    scm_github_migration_helpers:list_project_webhooks(Coords) ->  [];
                    scm_github_migration_helpers:delete_webhooks(Coords, []) -> ok;
                    scm_github_migration_helpers:validate_no_local_git_repo(Coords) -> ok;
                    scm_github_migration_helpers:initialize_local_git_repo(Coords) -> ok;
                    scm_github_migration_helpers:set_scm_module_to_new_github(proj_record) -> ok;
                    scm_github_migration_helpers:validate_automate_has_push_permissions(Coords) -> true
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org, Proj])),
    ?verifyAll.

migrate_validate_automate_throws_error_prints_error() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    Coords = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj},
    ExpectedResponse = {error, <<"Migration failed for ent_name/org_name/proj_name: why.\n">>},

    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj) -> {ok, proj_record};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record}) -> proj_record;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj) -> true;
                    scm_github_migration_helpers:validate_github_scm_configured(Ent) -> true;
                    scm_github_migration_helpers:validate_automate_has_push_permissions(Coords) -> erlang:throw(why)
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org, Proj])),
    ?verifyAll.

migrate_when_local_git_repo_exists_throws_error() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    Coords = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj},
    ExpectedResponse = {error, <<"Migration failed for ent_name/org_name/proj_name: why.\n">>},

    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj) -> {ok, proj_record};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record}) -> proj_record;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj) -> true;
                    scm_github_migration_helpers:validate_github_scm_configured(Ent) -> true;
                    scm_github_migration_helpers:validate_automate_has_push_permissions(Coords) -> true;
                    scm_github_migration_helpers:list_project_webhooks(Coords) ->  webhookids;
                    scm_github_migration_helpers:delete_webhooks(Coords, webhookids) -> ok;
                    scm_github_migration_helpers:validate_no_local_git_repo(Coords) -> erlang:throw(why)
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org, Proj])),
    ?verifyAll.

migrate_when_initialize_local_git_repo_fails_throws_error() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    Coords = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj},
    ExpectedResponse = {error, <<"Migration failed for ent_name/org_name/proj_name: why.\n">>},

    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj) -> {ok, proj_record};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record}) -> proj_record;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj) -> true;
                    scm_github_migration_helpers:validate_github_scm_configured(Ent) -> true;
                    scm_github_migration_helpers:validate_automate_has_push_permissions(Coords) -> true;
                    scm_github_migration_helpers:list_project_webhooks(Coords) ->  webhookids;
                    scm_github_migration_helpers:delete_webhooks(Coords, webhookids) -> ok;
                    scm_github_migration_helpers:validate_no_local_git_repo(Coords) -> ok;
                    scm_github_migration_helpers:initialize_local_git_repo(Coords) -> erlang:throw(why)
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org, Proj])),
    ?verifyAll.

migrate_when_project_exists_returns_the_project() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    Coords = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj},
    ExpectedResponse = {ok, <<"Successfully migrated ent_name/org_name/proj_name to GitHubV2.\n">>},

    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj) -> {ok, proj_record};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record}) -> proj_record;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj) -> true;
                    scm_github_migration_helpers:validate_github_scm_configured(Ent) -> true;
                    scm_github_migration_helpers:list_project_webhooks(Coords) ->  webhookids;
                    scm_github_migration_helpers:delete_webhooks(Coords, webhookids) -> ok;
                    scm_github_migration_helpers:validate_no_local_git_repo(Coords) -> ok;
                    scm_github_migration_helpers:initialize_local_git_repo(Coords) -> ok;
                    scm_github_migration_helpers:set_scm_module_to_new_github(proj_record) -> ok;
                    scm_github_migration_helpers:validate_automate_has_push_permissions(Coords) -> true
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org, Proj])),
    ?verifyAll.

migrate_when_set_scm_module_fails_throws_error() ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    Coords = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj},
    ExpectedResponse = <<"Migration failed for ", Ent/binary, "/", Org/binary, "/",
                        Proj/binary, ": why.\n">>,
    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj) -> {ok, proj_record};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record}) -> proj_record;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj) -> true;
                    scm_github_migration_helpers:validate_github_scm_configured(Ent) -> true;
                    scm_github_migration_helpers:validate_automate_has_push_permissions(Coords) -> true;
                    scm_github_migration_helpers:list_project_webhooks(Coords) ->  webhookids;
                    scm_github_migration_helpers:delete_webhooks(Coords, webhookids) -> ok;
                    scm_github_migration_helpers:validate_no_local_git_repo(Coords) -> ok;
                    scm_github_migration_helpers:initialize_local_git_repo(Coords) -> ok;
                    scm_github_migration_helpers:set_scm_module_to_new_github(proj_record) -> erlang:throw(why)
                end),

    ?assertEqual({error, ExpectedResponse}, scm_github_migration:migrate([Ent, Org, Proj])),
    ?verifyAll.

%% migrate_org => 2 args to the function(ent, org)
migrate_org_successfully_migrates_all_projects() ->
    Ent = <<"ent">>,
    Org = <<"org">>,
    ProjList = [Proj1, Proj2, Proj3] = [<<"proj1">>, <<"proj2">>, <<"proj3">>],
    Coords1 = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj1},
    Coords2 = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj2},
    Coords3 = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj3},
    ExpectedResponse = {ok, <<"Successfully migrated ent/org to GitHubV2.\n",
                              "Successfully migrated ent/org/proj1 to GitHubV2.\n",
                              "Successfully migrated ent/org/proj2 to GitHubV2.\n",
                              "Successfully migrated ent/org/proj3 to GitHubV2.\n">>},

    hoax:expect(receive
                    scm_github_migration_helpers:list_ghv1_projects(Ent, Org) -> ProjList;
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj1) -> {ok, proj_record1};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record1}) -> proj_record1;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record1) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj1) -> true;
                    scm_github_migration_helpers:validate_github_scm_configured(Ent) -> true;
                    scm_github_migration_helpers:list_project_webhooks(Coords1) ->  webhookids1;
                    scm_github_migration_helpers:delete_webhooks(Coords1, webhookids1) -> ok;
                    scm_github_migration_helpers:validate_no_local_git_repo(Coords1) -> ok;
                    scm_github_migration_helpers:initialize_local_git_repo(Coords1) -> ok;
                    scm_github_migration_helpers:set_scm_module_to_new_github(proj_record1) -> ok;
                    scm_github_migration_helpers:validate_automate_has_push_permissions(Coords1) -> true;
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj2) -> {ok, proj_record2};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record2}) -> proj_record2;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record2) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj2) -> true;
                    scm_github_migration_helpers:list_project_webhooks(Coords2) ->  webhookids2;
                    scm_github_migration_helpers:delete_webhooks(Coords2, webhookids2) -> ok;
                    scm_github_migration_helpers:validate_no_local_git_repo(Coords2) -> ok;
                    scm_github_migration_helpers:initialize_local_git_repo(Coords2) -> ok;
                    scm_github_migration_helpers:set_scm_module_to_new_github(proj_record2) -> ok;
                    scm_github_migration_helpers:validate_automate_has_push_permissions(Coords2) -> true;
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj3) -> {ok, proj_record3};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record3}) -> proj_record3;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record3) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj3) -> true;
                    scm_github_migration_helpers:list_project_webhooks(Coords3) ->  webhookids3;
                    scm_github_migration_helpers:delete_webhooks(Coords3, webhookids3) -> ok;
                    scm_github_migration_helpers:validate_no_local_git_repo(Coords3) -> ok;
                    scm_github_migration_helpers:initialize_local_git_repo(Coords3) -> ok;
                    scm_github_migration_helpers:set_scm_module_to_new_github(proj_record3) -> ok;
                    scm_github_migration_helpers:validate_automate_has_push_permissions(Coords3) -> true
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org])),
    ?verifyAll.

migrate_org_does_nothing_if_no_ghv1_projects_exist() ->
    Ent = <<"ent">>,
    Org = <<"org">>,
    ProjList = [],
    ExpectedResponse = {ok, <<"No GitHubV1 projects found for ent/org. Nothing to do.\n">>},

    hoax:expect(receive
                    scm_github_migration_helpers:list_ghv1_projects(Ent, Org) -> ProjList
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org])),
    ?verifyAll.

migrate_org_fails_migrate_all_projects_on_error() ->
    Ent = <<"ent">>,
    Org = <<"org">>,
    ProjList = [Proj1, Proj2, Proj3] = [<<"proj1">>, <<"proj2">>, <<"proj3">>],
    Coords1 = #proj_coordinates{ent_name = Ent, org_name = Org, proj_name = Proj1},
    ExpectedResponse = {error, <<"Migration failed for ent/org.\n",
                                 "Migration failed for ent/org/proj2: proj2_why.\n",
                                 "Migration failed for ent/org/proj3: proj3_why.\n",
                                 "Successfully migrated ent/org/proj1 to GitHubV2.\n">>},

    hoax:expect(receive
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj1) -> {ok, proj_record1};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record1}) -> proj_record1;
                    scm_github_migration_helpers:list_ghv1_projects(Ent, Org) -> ProjList;
                    scm_github_migration_helpers:validate_project_scm_module(proj_record1) -> true;
                    scm_github_migration_helpers:validate_no_open_changes(Ent, Org, Proj1) -> true;
                    scm_github_migration_helpers:validate_github_scm_configured(Ent) -> true;
                    scm_github_migration_helpers:list_project_webhooks(Coords1) ->  webhookids1;
                    scm_github_migration_helpers:delete_webhooks(Coords1, webhookids1) -> ok;
                    scm_github_migration_helpers:validate_no_local_git_repo(Coords1) -> ok;
                    scm_github_migration_helpers:initialize_local_git_repo(Coords1) -> ok;
                    scm_github_migration_helpers:set_scm_module_to_new_github(proj_record1) -> ok;
                    scm_github_migration_helpers:validate_automate_has_push_permissions(Coords1) -> true;
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj2) -> {ok, proj_record2};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record2}) -> erlang:throw(proj2_why);
                    deliv_enterprise:fetch(Ent) -> {ok, ent};
                    deliv_organization:fetch(Ent, Org) -> {ok, org};
                    deliv_project:fetch(Ent, Org, Proj3) -> {ok, proj_record3};
                    scm_github_migration_helpers:validate_input({ok, ent}, {ok, org}, {ok, proj_record3}) -> erlang:throw(proj3_why)
                end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent, Org])),
    ?verifyAll.

migrate_ent_migrates_all_orgs_in_an_enterprise() ->
    Ent = <<"ent">>,
    OrgList = [Org1, Org2, Org3] = [<<"org1">>, <<"org2">>, <<"org3">>],
    Org1ProjList = [Proj11, Proj12, Proj13] = [<<"proj11">>, <<"proj12">>, <<"proj13">>],
    Org2ProjList = [] = [],
    Org3ProjList = [Proj31] = [<<"proj31">>],
    Coords11 = #proj_coordinates{ent_name = Ent, org_name = Org1, proj_name = Proj11},
    Coords12 = #proj_coordinates{ent_name = Ent, org_name = Org1, proj_name = Proj12},
    Coords13 = #proj_coordinates{ent_name = Ent, org_name = Org1, proj_name = Proj13},
    Coords31 = #proj_coordinates{ent_name = Ent, org_name = Org3, proj_name = Proj31},
    ExpectedResponse = {ok, <<"No errors encountered attempting to migrate ent to GitHubV2.\n",
                              "Successfully migrated ent/org1 to GitHubV2.\n"
                              "Successfully migrated ent/org1/proj11 to GitHubV2.\n",
                              "Successfully migrated ent/org1/proj12 to GitHubV2.\n",
                              "Successfully migrated ent/org1/proj13 to GitHubV2.\n",
                              "No GitHubV1 projects found for ent/org2. Nothing to do.\n",
                              "Successfully migrated ent/org3 to GitHubV2.\n"
                              "Successfully migrated ent/org3/proj31 to GitHubV2.\n">>},

    hoax:expect(receive
                  scm_github_migration_helpers:list_orgs(Ent) -> OrgList;
                  scm_github_migration_helpers:list_ghv1_projects(Ent, Org1) -> Org1ProjList;
                  deliv_enterprise:fetch(Ent) -> {ok, ent};
                  deliv_organization:fetch(Ent, Org1) -> {ok, org1};
                  deliv_project:fetch(Ent, Org1, Proj11) -> {ok, proj_record11};
                  scm_github_migration_helpers:validate_input({ok, ent}, {ok, org1}, {ok, proj_record11}) -> proj_record11;
                  scm_github_migration_helpers:validate_project_scm_module(proj_record11) -> true;
                  scm_github_migration_helpers:validate_no_open_changes(Ent, Org1, Proj11) -> true;
                  scm_github_migration_helpers:validate_github_scm_configured(Ent) -> true;
                  scm_github_migration_helpers:list_project_webhooks(Coords11) ->  webhookids11;
                  scm_github_migration_helpers:delete_webhooks(Coords11, webhookids11) -> ok;
                  scm_github_migration_helpers:validate_no_local_git_repo(Coords11) -> ok;
                  scm_github_migration_helpers:initialize_local_git_repo(Coords11) -> ok;
                  scm_github_migration_helpers:set_scm_module_to_new_github(proj_record11) -> ok;
                  scm_github_migration_helpers:validate_automate_has_push_permissions(Coords11) -> true;
                  deliv_project:fetch(Ent, Org1, Proj12) -> {ok, proj_record12};
                  scm_github_migration_helpers:validate_input({ok, ent}, {ok, org1}, {ok, proj_record12}) -> proj_record12;
                  scm_github_migration_helpers:validate_project_scm_module(proj_record12) -> true;
                  scm_github_migration_helpers:validate_no_open_changes(Ent, Org1, Proj12) -> true;
                  scm_github_migration_helpers:list_project_webhooks(Coords12) ->  webhookids12;
                  scm_github_migration_helpers:delete_webhooks(Coords12, webhookids12) -> ok;
                  scm_github_migration_helpers:validate_no_local_git_repo(Coords12) -> ok;
                  scm_github_migration_helpers:initialize_local_git_repo(Coords12) -> ok;
                  scm_github_migration_helpers:set_scm_module_to_new_github(proj_record12) -> ok;
                  scm_github_migration_helpers:validate_automate_has_push_permissions(Coords12) -> true;
                  deliv_project:fetch(Ent, Org1, Proj13) -> {ok, proj_record13};
                  scm_github_migration_helpers:validate_input({ok, ent}, {ok, org1}, {ok, proj_record13}) -> proj_record13;
                  scm_github_migration_helpers:validate_project_scm_module(proj_record13) -> true;
                  scm_github_migration_helpers:validate_no_open_changes(Ent, Org1, Proj13) -> true;
                  scm_github_migration_helpers:list_project_webhooks(Coords13) ->  webhookids13;
                  scm_github_migration_helpers:delete_webhooks(Coords13, webhookids13) -> ok;
                  scm_github_migration_helpers:validate_no_local_git_repo(Coords13) -> ok;
                  scm_github_migration_helpers:initialize_local_git_repo(Coords13) -> ok;
                  scm_github_migration_helpers:set_scm_module_to_new_github(proj_record13) -> ok;
                  scm_github_migration_helpers:validate_automate_has_push_permissions(Coords13) -> true;
                  % Iterate the second org
                  scm_github_migration_helpers:list_ghv1_projects(Ent, Org2) -> Org2ProjList;
                  % Iterate the third org
                  scm_github_migration_helpers:list_ghv1_projects(Ent, Org3) -> Org3ProjList;
                  deliv_organization:fetch(Ent, Org3) -> {ok, org3};
                  deliv_project:fetch(Ent, Org3, Proj31) -> {ok, proj_record31};
                  scm_github_migration_helpers:validate_input({ok, ent}, {ok, org3}, {ok, proj_record31}) -> proj_record31;
                  scm_github_migration_helpers:validate_project_scm_module(proj_record31) -> true;
                  scm_github_migration_helpers:validate_no_open_changes(Ent, Org3, Proj31) -> true;
                  scm_github_migration_helpers:list_project_webhooks(Coords31) ->  webhookids31;
                  scm_github_migration_helpers:delete_webhooks(Coords31, webhookids31) -> ok;
                  scm_github_migration_helpers:validate_no_local_git_repo(Coords31) -> ok;
                  scm_github_migration_helpers:initialize_local_git_repo(Coords31) -> ok;
                  scm_github_migration_helpers:set_scm_module_to_new_github(proj_record31) -> ok;
                  scm_github_migration_helpers:validate_automate_has_push_permissions(Coords31) -> true
               end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent])),
    ?verifyAll.

migrate_ent_points_to_error_when_project_migration_fails() ->
    Ent = <<"ent">>,
    OrgList = [Org1, Org2, Org3] = [<<"org1">>, <<"org2">>, <<"org3">>],
    Org1ProjList = [Proj11, Proj12, Proj13] = [<<"proj11">>, <<"proj12">>, <<"proj13">>],
    Org2ProjList = [] = [],
    Org3ProjList = [Proj31] = [<<"proj31">>],
    Coords11 = #proj_coordinates{ent_name = Ent, org_name = Org1, proj_name = Proj11},
    Coords12 = #proj_coordinates{ent_name = Ent, org_name = Org1, proj_name = Proj12},
    Coords31 = #proj_coordinates{ent_name = Ent, org_name = Org3, proj_name = Proj31},
    ExpectedResponse = {error, <<"Migration failed for ent.\n",
                              "Migration failed for ent/org1.\n",
                              "Migration failed for ent/org1/proj13: error_proj13.\n",
                              "Successfully migrated ent/org1/proj11 to GitHubV2.\n",
                              "Successfully migrated ent/org1/proj12 to GitHubV2.\n",
                              "No GitHubV1 projects found for ent/org2. Nothing to do.\n",
                              "Successfully migrated ent/org3 to GitHubV2.\n"
                              "Successfully migrated ent/org3/proj31 to GitHubV2.\n">>},

    hoax:expect(receive
                  scm_github_migration_helpers:list_orgs(Ent) -> OrgList;
                  scm_github_migration_helpers:list_ghv1_projects(Ent, Org1) -> Org1ProjList;
                  deliv_enterprise:fetch(Ent) -> {ok, ent};
                  deliv_organization:fetch(Ent, Org1) -> {ok, org1};
                  deliv_project:fetch(Ent, Org1, Proj11) -> {ok, proj_record11};
                  scm_github_migration_helpers:validate_input({ok, ent}, {ok, org1}, {ok, proj_record11}) -> proj_record11;
                  scm_github_migration_helpers:validate_project_scm_module(proj_record11) -> true;
                  scm_github_migration_helpers:validate_no_open_changes(Ent, Org1, Proj11) -> true;
                  scm_github_migration_helpers:validate_github_scm_configured(Ent) -> true;
                  scm_github_migration_helpers:list_project_webhooks(Coords11) ->  webhookids11;
                  scm_github_migration_helpers:delete_webhooks(Coords11, webhookids11) -> ok;
                  scm_github_migration_helpers:validate_no_local_git_repo(Coords11) -> ok;
                  scm_github_migration_helpers:initialize_local_git_repo(Coords11) -> ok;
                  scm_github_migration_helpers:set_scm_module_to_new_github(proj_record11) -> ok;
                  scm_github_migration_helpers:validate_automate_has_push_permissions(Coords11) -> true;
                  deliv_project:fetch(Ent, Org1, Proj12) -> {ok, proj_record12};
                  scm_github_migration_helpers:validate_input({ok, ent}, {ok, org1}, {ok, proj_record12}) -> proj_record12;
                  scm_github_migration_helpers:validate_project_scm_module(proj_record12) -> true;
                  scm_github_migration_helpers:validate_no_open_changes(Ent, Org1, Proj12) -> true;
                  scm_github_migration_helpers:list_project_webhooks(Coords12) ->  webhookids12;
                  scm_github_migration_helpers:delete_webhooks(Coords12, webhookids12) -> ok;
                  scm_github_migration_helpers:validate_no_local_git_repo(Coords12) -> ok;
                  scm_github_migration_helpers:initialize_local_git_repo(Coords12) -> ok;
                  scm_github_migration_helpers:set_scm_module_to_new_github(proj_record12) -> ok;
                  scm_github_migration_helpers:validate_automate_has_push_permissions(Coords12) -> true;
                  deliv_project:fetch(Ent, Org1, Proj13) -> {ok, proj_record13};
                  scm_github_migration_helpers:validate_input({ok, ent}, {ok, org1}, {ok, proj_record13}) -> proj_record13;
                  scm_github_migration_helpers:validate_project_scm_module(proj_record13) -> erlang:throw(error_proj13);
                  % Iterate the second org
                  scm_github_migration_helpers:list_ghv1_projects(Ent, Org2) -> Org2ProjList;
                  % Iterate the third org
                  scm_github_migration_helpers:list_ghv1_projects(Ent, Org3) -> Org3ProjList;
                  deliv_organization:fetch(Ent, Org3) -> {ok, org3};
                  deliv_project:fetch(Ent, Org3, Proj31) -> {ok, proj_record31};
                  scm_github_migration_helpers:validate_input({ok, ent}, {ok, org3}, {ok, proj_record31}) -> proj_record31;
                  scm_github_migration_helpers:validate_project_scm_module(proj_record31) -> true;
                  scm_github_migration_helpers:validate_no_open_changes(Ent, Org3, Proj31) -> true;
                  scm_github_migration_helpers:list_project_webhooks(Coords31) ->  webhookids31;
                  scm_github_migration_helpers:delete_webhooks(Coords31, webhookids31) -> ok;
                  scm_github_migration_helpers:validate_no_local_git_repo(Coords31) -> ok;
                  scm_github_migration_helpers:initialize_local_git_repo(Coords31) -> ok;
                  scm_github_migration_helpers:set_scm_module_to_new_github(proj_record31) -> ok;
                  scm_github_migration_helpers:validate_automate_has_push_permissions(Coords31) -> true
               end),

    ?assertEqual(ExpectedResponse, scm_github_migration:migrate([Ent])),
    ?verifyAll.
