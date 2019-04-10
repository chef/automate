-module(notification_subscriptions_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

subscribe_fixture_test_() ->
    hoax:fixture(?MODULE, "subscribe").

subscription_fixture_test_() ->
    hoax:fixture(?MODULE, "subscription").

subscribers_fixture_test_() ->
    hoax:fixture(?MODULE, "subscribers").

subscribe_returns_ok_on_success() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    UserName = <<"HankVenture">>,
    Subscription = [<<"observe">>],

    hoax:mock(notification_subscriptions_db,
              ?expect(upsert,
                      ?withArgs([EntName, OrgName, ProjName, UserName, Subscription]),
                      ?andReturn([subscription]))),

    Result = notification_subscriptions:subscribe(EntName, OrgName, ProjName, UserName, Subscription),

    ?assertEqual(ok, Result),
    ?verifyAll.

subscribe_returns_error_cannot_subscribe_when_one_of_its_elements_is_not_found() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    UserName = <<"HankVenture">>,
    Subscription = [<<"observe">>],

    hoax:mock(notification_subscriptions_db,
              ?expect(upsert,
                      ?withArgs([EntName, OrgName, ProjName, UserName, Subscription]),
                      ?andReturn({error,{<<"23502">>,
                                         <<"null value in column \"project_id\" violates not-null constraint">>}}))),

    Result = notification_subscriptions:subscribe(EntName, OrgName, ProjName, UserName, Subscription),

    ?assertEqual({error, cannot_subscribe}, Result),
    ?verifyAll.

subscribe_forwards_db_error_when_upsert_fails_unexpectedly() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    UserName = <<"HankVenture">>,
    Subscription = [<<"observe">>],

    hoax:mock(notification_subscriptions_db,
              ?expect(upsert,
                      ?withArgs([EntName, OrgName, ProjName, UserName, Subscription]),
                      ?andReturn({error, im_unexpected}))),

    Result = notification_subscriptions:subscribe(EntName, OrgName, ProjName, UserName, Subscription),

    ?assertEqual({error, im_unexpected}, Result),
    ?verifyAll.

subscription_returns_subscription_when_a_user_is_watching_a_project() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    UserName = <<"HankVenture">>,
    Subscription = [<<"review">>, <<"observe">>],

    Row = [{<<"categories">>, Subscription}], % categories is all we care about for test

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName, UserName]),
                      ?andReturn({ok, [Row]}))),

    Result = notification_subscriptions:subscription(EntName, OrgName, ProjName, UserName),

    ?assertEqual(Subscription, Result),
    ?verifyAll.

subscription_returns_empty_list_when_a_user_is_not_watching_a_project() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    UserName = <<"HankVenture">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName, UserName]),
                      ?andReturn({ok, []}))),

    Result = notification_subscriptions:subscription(EntName, OrgName, ProjName, UserName),

    ?assertEqual([], Result),
    ?verifyAll.

subscription_forwards_db_error() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    UserName = <<"HankVenture">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName, UserName]),
                      ?andReturn({error, noooo}))),

    Result = notification_subscriptions:subscription(EntName, OrgName, ProjName, UserName),

    ?assertEqual({error, noooo}, Result),
    ?verifyAll.

subscriber_emails_by_event_returns_list_of_email_with_verify_passed() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Email = <<"hank@venture.com">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"review">>]),
                      ?andReturn([Email]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, verify_passed),

    ?assertEqual([Email], Result).

subscriber_emails_by_event_returns_list_of_email_with_change_approved() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Email = <<"hank@venture.com">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"review">>]),
                      ?andReturn([Email]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, change_approved),

    ?assertEqual([Email], Result).

subscriber_emails_by_event_returns_list_of_email_with_acceptance_passed() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Email = <<"hank@venture.com">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"deliver">>]),
                      ?andReturn([Email]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, acceptance_passed),

    ?assertEqual([Email], Result).

subscriber_emails_by_event_returns_list_of_email_with_acceptance_failed() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Email = <<"hank@venture.com">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"observe">>]),
                      ?andReturn([Email]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, acceptance_failed),

    ?assertEqual([Email], Result).

subscriber_emails_by_event_returns_list_of_email_with_rehearsal_failed() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Email = <<"hank@venture.com">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"observe">>]),
                      ?andReturn([Email]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, rehearsal_failed),

    ?assertEqual([Email], Result).

subscriber_emails_by_event_returns_error_and_logs_on_unhandled_event() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Event = something_unexpected,

    hoax:mock(chef_log,
              ?expect(error,
                      ?withArgs(["Failed to retrieve subscribers for unhandled event ~s in ent ~s, org ~s, project ~s",
                                  [Event, EntName, OrgName, ProjName]]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, Event),

    ?assertEqual({error, unhandled_event}, Result),
    ?verifyAll.

subscriber_emails_by_event_forwards_db_error() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Event = verify_passed,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"review">>]),
                      ?andReturn({error, darn}))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, Event),

    ?assertEqual({error, darn}, Result),
    ?verifyAll.

subscriber_emails_by_event_returns_list_of_email_with_change_delivered() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Email = <<"hank@venture.com">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"deliver">>]),
                      ?andReturn([Email]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, change_delivered),
    ?assertEqual([Email], Result).

subscriber_emails_by_event_returns_list_of_email_with_delivered_passed() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Email = <<"hank@venture.com">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"observe">>]),
                      ?andReturn([Email]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, delivered_passed),

    ?assertEqual([Email], Result).

subscriber_emails_by_event_returns_list_of_email_with_union_failed() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Email = <<"hank@venture.com">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"observe">>]),
                      ?andReturn([Email]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, union_failed),
    ?assertEqual([Email], Result).

subscriber_emails_by_event_returns_list_of_email_with_delivered_failed() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Email = <<"hank@venture.com">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"observe">>]),
                      ?andReturn([Email]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, delivered_failed),

    ?assertEqual([Email], Result).

subscriber_emails_by_event_returns_list_of_email_with_build_failed() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Email = <<"hank@venture.com">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"observe">>]),
                      ?andReturn([Email]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, build_failed),

    ?assertEqual([Email], Result).

subscriber_emails_by_event_returns_list_of_email_with_comment_created() ->
    EntName = <<"HankCo">>,
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Email = <<"hank@venture.com">>,

    hoax:mock(notification_subscriptions_db,
              ?expect(fetch_emails_by_category,
                      ?withArgs([EntName, OrgName, ProjName, <<"review">>]),
                      ?andReturn([Email]))),

    Result = notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, comment_created),

    ?assertEqual([Email], Result).
