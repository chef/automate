-module(scm_change_integration_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

fixture_test_() ->
    eunit_sugar:parameterized_fixture(?MODULE, setup, teardown).

setup() ->
    error_logger:tty(false),
    eu_database:setup(),
    setup_change_subscriptions(),
    Pipeline = <<"master">>,
    eu_data:with_enterprise(<<"scm_change_ent">>,
        eu_data:with_organization(<<"scm_change_org">>,
            eu_data:with_bitbucket_project(<<"scm_change_proj">>,
                Pipeline, <<"BBKY">>, <<"bb_repo">>,
                eu_data:with_change(<<"a_user">>, Pipeline, <<"feature/branch">>,
                    fun(_Enterprise, _Organization, _Project, Change) ->
                            Change
                    end)))).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

setup_change_subscriptions() ->
    application:set_env(delivery, stages_data, [
                                                {verify, do_not_care},
                                                {build, do_not_care},
                                                {acceptance, do_not_care},
                                                {union, do_not_care},
                                                {rehearsal, do_not_care},
                                                {delivered, do_not_care}
                                               ]),
    application:start(gproc).

save_and_fetch(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    PullRequestId = 33,
    PullRequestUrl = <<"https://bitbucket.pr">>,

    scm_change:save(ChangeId, PullRequestId, PullRequestUrl),

    {ok, Result} = scm_change:fetch_by_change_id(ChangeId),

    ?assertEqual(PullRequestId, scm_change:getval(pr_id, Result)),
    ?assertEqual(PullRequestUrl, scm_change:getval(pr_url, Result)).
