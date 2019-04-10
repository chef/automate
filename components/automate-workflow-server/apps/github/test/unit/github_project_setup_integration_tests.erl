-module(github_project_setup_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

-define(DEPLOY_KEY_TITLE, <<"Delivery Builder">>).

%% Due to the unreliability of the Github Enterprise response time / availability,
%% these tests passed at the time of this writing but are disabled for the time being.
%%
%% To enable, change name to project_create_fixture_test_
%% To disable, change name to project_create_fixture_disable
project_create_fixture_disabled() ->
    hoax:parameterized_fixture(?MODULE, "project_create", setup, teardown).

setup() ->
    error_logger:tty(false),
    app_test_helpers:setup_app_env(),
    eu_database:setup(),
    eu_github:setup(),
    eu_github:with_enterprise(<<"github_project_test_enterprise">>,
        eu_data:with_organization(<<"github_project_test_organization">>,
            fun(Enterprise, Organization) ->
                EntName = deliv_enterprise:getval(name, Enterprise),
                OrgName = deliv_organization:getval(name, Organization),
                ProjName = <<"github_project_test_project">>,
                PipeName = <<"master">>,
                RepoOwner = <<"automated-tests">>,
                RepoName = eu_github:repo_name(ProjName),

                {ok, Project} = deliv_project:new(EntName, OrgName, ProjName, PipeName,
                                                  deliv_scm_github, RepoOwner, RepoName),
                Coords = deliv_project:to_coords(Project),

                eu_github:create_github_repo(Coords, RepoName),
                {ok, WebhookId} = github_repo:configure({ok, Project}, no_verify_ssl),

                {{Enterprise, Organization, Project}, {Coords, WebhookId}}
            end)).

teardown({{Enterprise, Organization, Project}, _}) ->
    eu_github:teardown(Enterprise, Organization, Project),
    eu_database:teardown(),
    error_logger:tty(true).

project_create_creates_project({
                                {_, _, Project},
                                {Coords = #proj_coordinates{ent_name = EntName,
                                                            org_name = OrgName,
                                                            proj_name = ProjName},
                                 WebhookId}
                                }) ->
    ?assertEqual({ok, Project}, deliv_project:fetch(EntName, OrgName, ProjName)),
    assert_webhook_exists(Coords, WebhookId).

assert_webhook_exists(Coords, WebhookId) ->
    Route = ["/hooks/", chef_utils:to_str(WebhookId)],
    case deliv_github_client:req(Coords, get, Route, <<>>, []) of
        {ok, Status, _, Response} ->
            Json = chef_json:decode(Response),
            ?assertEqual(200, Status),

            ExpectedUrl = chef_utils:to_bin(["http://127.0.0.1/api/v0/",
                                              "e/github_project_test_enterprise/",
                                              "orgs/github_project_test_organization/",
                                              "projects/github_project_test_project/github-webhook"]),
            ActualUrl = ej:get({<<"config">>, <<"url">>}, Json),
            ?assertEqual(ExpectedUrl, ActualUrl),

            ActualContentType = ej:get({<<"config">>, <<"content_type">>}, Json),
            ?assertEqual(<<"json">>, ActualContentType),

            ExpectedEvents = [<<"pull_request">>, <<"issue_comment">>],
            ActualEvents = ej:get({<<"events">>}, Json),
            ?assertEqual(ExpectedEvents, ActualEvents),

            ActualStatus = ej:get({<<"active">>}, Json),
            ?assertEqual(true, ActualStatus);
        {error, Why} ->
            error(Why)
    end.
