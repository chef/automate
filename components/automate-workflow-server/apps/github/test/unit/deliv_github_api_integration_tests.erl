-module(deliv_github_api_integration_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

-define(GITHUB_ISSUE_TITLE, <<"Github Issue Title">>).

%% Due to the unreliability of the Github Enterprise response time / availability,
%% these tests passed at the time of this writing but are disabled for the time being.
add_label_to_issue_fixture_disabled() ->
    hoax:parameterized_fixture(?MODULE, "add_label_to_issue", setup, teardown).

setup() ->
    error_logger:tty(false),
    eu_database:setup(),
    eu_github:setup(),
    eu_data:with_enterprise(<<"github_api_test_enterprise">>,
        eu_data:with_organization(<<"github_api_test_organization">>,
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

                {Enterprise, Organization, Project}
            end)).

teardown({Enterprise, Organization, Project}) ->
    eu_github:teardown(Enterprise, Organization, Project),
    eu_database:teardown(),
    error_logger:tty(true).

add_label_to_issue_returns_200({Enterprise, Organization, Project}) ->
    Label = <<"Chef">>,
    EntName = deliv_enterprise:getval(name, Enterprise),
    OrgName = deliv_organization:getval(name, Organization),
    ProjName = deliv_project:getval(name, Project),
    Scope = deliv_scopes:from_scoping_names(EntName, OrgName, ProjName),
    {ok, IssueNum} = create_issue(Scope),

    deliv_github_api:add_label_to_issue(Scope, IssueNum, Label),

    ?assertEqual([Label], get_issue_labels(Scope, IssueNum)).

%% I'm leaving this as a helper right now because we don't require full support
create_issue(Scope) ->
    Route = ["/issues"],
    Body = {[{title, ?GITHUB_ISSUE_TITLE}]},
    case deliv_github_client:req(Scope, post, Route, Body) of
        {ok, 201, _Headers, Response} ->
            IssueNum =  ej:get([<<"number">>], chef_json:decode(Response)),
            {ok, IssueNum};
        _ ->
            {error, could_not_create_issue}
    end.

%% I'm leaving this as a helper right now because we don't require full support
get_issue_labels(Scope, IssueId) ->
    Route = ["/issues/", chef_utils:to_str(IssueId)],
    case deliv_github_client:req(Scope, get, Route) of
        {ok, 200, _, ResponseJson} ->
            ej:get({<<"labels">>, {select, all}, <<"name">>}, chef_json:decode(ResponseJson));
        _ ->
            error
    end.
