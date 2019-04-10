-module(notification_subscriptions_db_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

fetch_fixture_test_() ->
    hoax:fixture(?MODULE, "fetch").

fetch_emails_by_category_test_() ->
    hoax:fixture(?MODULE, "fetch_emails_by_category").

fetch_forwards_db_error() ->
    EntName = <<"NCC-1701">>,
    OrgName = <<"Federation">>,
    ProjName = <<"KobayashiMaru">>,
    UserName = <<"JTKirk">>,
    Error = {error, badness},

    hoax:mock(deliv_db,
              ?expect(select,
                      ?withArgs([notification_subscriptions_db, fetch, [EntName, OrgName, ProjName, UserName]]),
                      ?andReturn(Error))),

    Result = notification_subscriptions_db:fetch(EntName, OrgName, ProjName, UserName),

    ?assertEqual(Error, Result),
    ?verifyAll.

fetch_emails_by_category_logs_failed_called_on_error() ->
    hoax:mock(sqerl_rec,
              ?expect(scalar_fetch,
                      ?withArgs([notification_subscriptions_db, fetch_emails_by_category, [ent, org, proj, category]]),
                      ?andReturn({error, fail_sauce}))),
    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([notification_subscriptions_db, fetch_emails_by_category,
                                 [ent, org, proj, category], fail_sauce]))),

    Result = notification_subscriptions_db:fetch_emails_by_category(ent, org, proj, category),

    ?assertEqual({error, fail_sauce}, Result),
    ?verifyAll.
