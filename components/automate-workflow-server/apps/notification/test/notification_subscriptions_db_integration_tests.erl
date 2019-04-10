-module(notification_subscriptions_db_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_types.hrl").

-compile([export_all]).

upsert_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "upsert_", setup, teardown).

fetch_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "fetch_", setup, teardown).

setup() ->
    error_logger:tty(false),
    eu_database:setup(),

    EntName = <<"NCC-1701">>,
    OrgName = <<"OG">>,
    ProjName = <<"KobayashiMaru">>,
    UserName = <<"JTKirk">>,

    eu_data:with_enterprise(EntName,
        eu_data:with_organization(OrgName,
            eu_data:with_project(ProjName,
                fun(Ent, _Org, Proj) ->
                    User = eu_data:fetch_or_create_user(Ent, UserName),

                    UserId = deliv_user:getval(id, User),
                    ProjId = deliv_project:getval(id, Proj),
                    UserEmail = deliv_user:getval(email, User),

                    [EntName, OrgName, {ProjId, ProjName}, {UserEmail, UserId, UserName}]
                end))).

teardown(_) ->
    eu_database:teardown(),
    error_logger:tty(true).

upsert_saves_a_new_subscription([EntName, OrgName, {ProjId, ProjName}, {_, UserId, UserName}]) ->
    Subscription = [<<"review">>, <<"deliver">>, <<"observe">>],
    Result = notification_subscriptions_db:upsert(EntName, OrgName, ProjName, UserName, Subscription),
    ?assertMatch([{notification_subscriptions_db, _, ProjId, UserId, Subscription}], Result).

upsert_updates_an_existing_subscription([EntName, OrgName, {ProjId, ProjName}, {_, UserId, UserName}]) ->
    Subscription = [<<"review">>, <<"deliver">>, <<"observe">>],
    %Create initial subscription
    notification_subscriptions_db:upsert(EntName, OrgName, ProjName, UserName, Subscription),
    UpdatedSubscription = [<<"review">>, <<"deliver">>],
    %Update subscription to have fewer categories
    Result = notification_subscriptions_db:upsert(EntName, OrgName, ProjName, UserName, UpdatedSubscription),
    ?assertMatch([{notification_subscriptions_db, _, ProjId, UserId, UpdatedSubscription}], Result).

upsert_fails_when_user_does_not_exist([EntName, OrgName, {_, ProjName}, _]) ->
    Subscription = [<<"review">>, <<"deliver">>, <<"observe">>],
    Result = notification_subscriptions_db:upsert(EntName, OrgName, ProjName, <<"JLPicard">>, Subscription),
    ?assertMatch({error,{<<"23502">>,
                         <<"null value in column \"user_id\" violates not-null constraint">>}}, Result).

upsert_fails_when_project_does_not_exist([EntName, OrgName, _, {_, _, UserName}]) ->
    Subscription = [<<"review">>, <<"deliver">>, <<"observe">>],
    Result = notification_subscriptions_db:upsert(EntName, OrgName, <<"DS9">>, UserName, Subscription),
    ?assertMatch({error,{<<"23502">>,
                         <<"null value in column \"project_id\" violates not-null constraint">>}}, Result).

upsert_only_removes_when_categories_list_is_empty([EntName, OrgName, {_, _}, {_, _, UserName}]) ->
    Subscription = [],
    Result = notification_subscriptions_db:upsert(EntName, OrgName, <<"DS9">>, UserName, Subscription),
    ?assertMatch([], Result).

fetch_returns_user_subscription_to_project_when_found([EntName, OrgName, {ProjId, ProjName}, {_, UserId, UserName}]) ->
    Subscription = [<<"review">>, <<"deliver">>, <<"observe">>],
    notification_subscriptions_db:upsert(EntName, OrgName, ProjName, UserName, Subscription),
    Result = notification_subscriptions_db:fetch(EntName, OrgName, ProjName, UserName),
    ?assertMatch({ok, [[{<<"project_id">>, ProjId},
                        {<<"user_id">>, UserId},
                        {<<"categories">>, Subscription}]]}, Result).

fetch_returns_subscription_for_project_in_correct_org_with_projects_of_same_name([EntName, OrgName, {ProjId, ProjName}, {_, UserId, UserName}]) ->
    Subscription = [<<"review">>, <<"deliver">>, <<"observe">>],
    Enterprise = eu_data:fetch_or_create_enterprise(EntName),
    TrickyOrgName = <<"trickyorg">>,
    TrickyOrg = eu_data:fetch_or_create_organization(Enterprise, TrickyOrgName),
    eu_data:fetch_or_create_project(Enterprise, TrickyOrg, ProjName),
    notification_subscriptions_db:upsert(EntName, OrgName, ProjName, UserName, Subscription),
    notification_subscriptions_db:upsert(EntName, TrickyOrgName, ProjName, UserName, Subscription),
    Result = notification_subscriptions_db:fetch(EntName, OrgName, ProjName, UserName),
    ?assertMatch({ok, [[{<<"project_id">>, ProjId},
                        {<<"user_id">>, UserId},
                        {<<"categories">>, Subscription}]]}, Result).

fetch_returns_empty_list_when_subscription_not_found([EntName, OrgName, {_, ProjName}, {_, _, UserName}]) ->
    Result = notification_subscriptions_db:fetch(EntName, OrgName, ProjName, UserName),
    ?assertMatch({ok, []}, Result).

fetch_emails_by_category_returns_email_address_of_subscribed_users([EntName, OrgName, {_, ProjName}, {UserEmail, _, UserName}]) ->
    Subscription = [<<"review">>, <<"deliver">>, <<"observe">>],
    notification_subscriptions_db:upsert(EntName, OrgName, ProjName, UserName, Subscription),
    Result = notification_subscriptions_db:fetch_emails_by_category(EntName, OrgName, ProjName, <<"observe">>),
    ?assertEqual([UserEmail], Result).

fetch_emails_by_category_returns_nothing_if_no_email_for_user([EntName, OrgName, {_, ProjName}, {_, _, _}]) ->
    Subscription = [<<"review">>, <<"deliver">>, <<"observe">>],
    NewName = <<"newusername">>,
    deliv_user:insert(EntName,
                       [{name, NewName},
                        {first_name, <<"Deliv">>},
                        {last_name, <<"Changeset">>},
                        {user_type, <<"internal">>}]),
    notification_subscriptions_db:upsert(EntName, OrgName, ProjName, NewName, Subscription),
    Result = notification_subscriptions_db:fetch_emails_by_category(EntName, OrgName, ProjName, <<"observe">>),
    ?assertEqual([], Result).

fetch_emails_by_category_subscribed_project_not_category_returns_no_emails([EntName, OrgName, {_, ProjName}, {_, _, UserName}]) ->
    Subscription = [<<"review">>, <<"deliver">>],
    notification_subscriptions_db:upsert(EntName, OrgName, ProjName, UserName, Subscription),
    Result = notification_subscriptions_db:fetch_emails_by_category(EntName, OrgName, ProjName, <<"observe">>),
    ?assertEqual([], Result).

fetch_emails_by_category_returns_users_with_defined_emails([EntName, OrgName, {_, ProjName}, {UserEmail, _, UserName}]) ->
    Subscription = [<<"review">>, <<"deliver">>, <<"observe">>],
    % subscribe user with email
    notification_subscriptions_db:upsert(EntName, OrgName, ProjName, UserName, Subscription),
    NewName = <<"newusername">>,
    deliv_user:insert(EntName,
                       [{name, NewName},
                        {first_name, <<"Deliv">>},
                        {last_name, <<"Changeset">>},
                        {user_type, <<"internal">>}]),
    % subscribe user without email
    notification_subscriptions_db:upsert(EntName, OrgName, ProjName, NewName, Subscription),
    AnotherName = <<"anotherusername">>,
    deliv_user:insert(EntName,
                       [{name, AnotherName},
                        {first_name, <<"Deliv">>},
                        {last_name, <<"Changeset">>},
                        {user_type, <<"internal">>}]),
    % subscribe user without email
    notification_subscriptions_db:upsert(EntName, OrgName, ProjName, AnotherName, Subscription),
    Result = notification_subscriptions_db:fetch_emails_by_category(EntName, OrgName, ProjName, <<"observe">>),
    ?assertEqual([UserEmail], Result).

fetch_emails_subscribes_to_two_projects_with_same_name_retrieves_one_email([EntName, OrgName, {_, ProjName}, {UserEmail, _, UserName}]) ->
    Subscription = [<<"review">>, <<"deliver">>, <<"observe">>],
    Enterprise = eu_data:fetch_or_create_enterprise(EntName),
    TrickyOrgName = <<"trickyorg">>,
    TrickyOrg = eu_data:fetch_or_create_organization(Enterprise, TrickyOrgName),
    eu_data:fetch_or_create_project(Enterprise, TrickyOrg, ProjName),
    notification_subscriptions_db:upsert(EntName, OrgName, ProjName, UserName, Subscription),
    notification_subscriptions_db:upsert(EntName, TrickyOrgName, ProjName, UserName, Subscription),
    Result = notification_subscriptions_db:fetch_emails_by_category(EntName, OrgName, ProjName, <<"observe">>),
    ?assertEqual([UserEmail], Result).
