-module(deliv_github_patchset_integration_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

pr_url_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "pr_url", setup, teardown).

setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup(),
    eu_data:with_enterprise(<<"Ventech">>,
      eu_data:with_organization(<<"SuperScience">>,
        eu_data:with_project(<<"OoRay">>,
          eu_data:with_pipeline(<<"master">>, fun(Ent, Org, Proj, Pipe) ->
            %% Create a patchset
            User = eu_data:fetch_or_create_user(Ent, <<"tventure">>),
            Patchset = eu_data:create_patchset(Ent, User, Org, Proj,
                                               Pipe, <<"VT-01/ooo-ray">>),

            %% Load PR opened payload
            Payload = ct_github:load_payload_from_disk("pull_request_opened.json"),

            %% Create a new github patchset
            deliv_github_patchset:save(Payload, Patchset),

            Patchset
          end)))).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

pr_url_returns_pr_url_for_patchset(Patchset) ->
    ExpectedPullRequestURL = <<"https://github.com/baxterthehacker/public-repo/pull/1">>,
    ActualPullRequestURL = deliv_github_patchset:pr_url(Patchset),
    ?assertEqual(ExpectedPullRequestURL, ActualPullRequestURL).
