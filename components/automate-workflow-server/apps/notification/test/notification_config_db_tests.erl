-module(notification_config_db_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_types.hrl").

-compile(export_all).

fixture_test_() ->
    hoax:fixture(?MODULE).

fetch_returns_an_error_when_db_call_fails() ->
    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgOrg">>,

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([notification_config_db,
                                 fetch_by_organization,
                                 [EntName, OrgName]]),
                      ?andReturn({error, why}))),

    ?assertEqual({error, why}, notification_config_db:fetch(EntName, OrgName)),
    ?verifyAll.

fetch_with_arity_of_1_calls_fetch_by_ent_id() ->
    EntName = <<"NCC-1701">>,
    Config = [],

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([notification_config_db,
                                 fetch_by_enterprise,
                                 [EntName]]),
                      ?andReturn(Config))),

    ?assertEqual(Config, notification_config_db:fetch(EntName)),
    ?verifyAll.

delete_forwards_error_when_db_call_fails() ->
    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgOrg">>,
    OrgId = 1,

    ExpectedError = {error, uhoh},

    hoax:mock(deliv_organization, [
              ?expect(fetch,
                      ?withArgs([EntName, OrgName]),
                      ?andReturn({ok, org})),
              ?expect(getval,
                      ?withArgs([id, org]),
                      ?andReturn(OrgId))
              ]),

    hoax:mock(sqerl_rec,
            ?expect(cquery,
                    ?withArgs([notification_config_db, delete_by_org_id, [OrgId]]),
                    ?andReturn(ExpectedError))),

    Result = notification_config_db:delete(EntName, OrgName),

    ?assertEqual(ExpectedError, Result),
    ?verifyAll.

delete_returns_error_org_not_found_when_org_is_missing() ->
    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgOrg">>,

    hoax:mock(deliv_organization,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName]),
                      ?andReturn({error, not_found}))),
    hoax:mock(deliv_db,
              ?expect(delete,
                      ?withArgs([?any, ?any, ?any]),
                      ?times(0))),

    Result = notification_config_db:delete(EntName, OrgName),

    ?assertEqual({error, org_not_found}, Result),
    ?verifyAll.

upsert_returns_an_error_when_db_call_fails() ->
    Type = slack_webhook,
    OrgId = 1,
    Settings = {[
                 {<<"url">>, <<"https://my.slack.com/whatevs">>}
                ]},
    EncodedSettings = chef_json:encode(Settings),
    Enabled = true,
    Name = <<"seven-of-nine">>,
    Config = #notification_config{notification_type = Type,
                                  name = Name,
                                  settings = Settings,
                                  enabled = Enabled,
                                  organization_id = OrgId},

    % enterprise Id is undefined for slack because it is configured at the org level
    hoax:mock(deliv_db,
      ?expect(qfetch,
              ?withArgs([notification_config_db,
                         upsert,
                         [Type, Name, EncodedSettings, Enabled, OrgId, undefined, undefined]]),
              ?andReturn({error, why}))),

    ?assertEqual({error, why}, notification_config_db:upsert(Config)),
    ?verifyAll.

upsert_returns_db_op_result() ->
    Type = slack_webhook,
    OrgId = 1,
    Settings = {[
                 {<<"url">>, <<"https://my.slack.com/whatevs">>}
                ]},
    EncodedSettings = chef_json:encode(Settings),
    Enabled = true,
    Name = <<"seven-of-nine">>,
    Config = #notification_config{notification_type = Type,
                                  name = Name,
                                  settings = Settings,
                                  enabled = Enabled,
                                  organization_id = OrgId},

    %enterprise Id is undefined for slack because it is configured at the org level
    hoax:mock(deliv_db,
      ?expect(qfetch,
              ?withArgs([notification_config_db,
                         upsert,
                         [Type, Name, EncodedSettings, Enabled, OrgId, undefined, undefined]]),
              ?andReturn(Config))),

    ?assertEqual(Config, notification_config_db:upsert(Config)),
    ?verifyAll.

delete_returns_error_ent_not_found_when_ent_is_missing() ->
    EntName = <<"NCC-1701">>,

    hoax:mock(deliv_enterprise,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({error, not_found}))),

    Result = notification_config_db:delete(EntName),

    ?assertEqual({error, ent_not_found}, Result),
    ?verifyAll.

delete_returns_ok_if_configuration_not_found() ->
    EntName = <<"NCC-1701">>,
    EntId = 1,
    NotificationScope = enterprise,

    hoax:mock(deliv_enterprise, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, ent})),
              ?expect(getval,
                      ?withArgs([id, ent]),
                      ?andReturn(EntId))
              ]),
    hoax:mock(sqerl_rec,
              ?expect(cquery,
                      ?withArgs([notification_config_db, delete_by_ent_id, [EntId]]),
                      ?andReturn({ok, 0}))),

    hoax:mock(chef_log,
                ?expect(debug,
                        ?withArgs(["Could not delete notification configuration for ~p ~p: not found", [NotificationScope, EntName]]))),

    ?assertEqual(ok, notification_config_db:delete(EntName)),
    ?verifyAll.

delete_logs_and_returns_an_error_if_call_fails_for_another_reason() ->
    EntName = <<"NCC-1701">>,
    EntId = 1,

    hoax:mock(deliv_enterprise, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, ent})),
              ?expect(getval,
                      ?withArgs([id, ent]),
                      ?andReturn(EntId))
              ]),
    hoax:mock(sqerl_rec,
              ?expect(cquery,
                      ?withArgs([notification_config_db, delete_by_ent_id, [EntId]]),
                      ?andReturn({error, why}))),

    hoax:mock(chef_log,
                ?expect(failed_call,
                        ?withArgs([notification_config_db, delete, [EntName], why]))),

    ?assertEqual({error, why}, notification_config_db:delete(EntName)),
    ?verifyAll.

delete_returns_ok_when_delete_success() ->
    EntName = <<"NCC-1701">>,
    EntId = 1,
    NotificationScope = enterprise,

    hoax:mock(deliv_enterprise, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, ent})),
              ?expect(getval,
                      ?withArgs([id, ent]),
                      ?andReturn(EntId))
              ]),
    hoax:mock(sqerl_rec,
              ?expect(cquery,
                      ?withArgs([notification_config_db, delete_by_ent_id, [EntId]]),
                      ?andReturn({ok, 1}))),
    hoax:mock(chef_log,
              ?expect(info,
                      ?withArgs(["Deleted notification configuration for ~p ~p", [NotificationScope, EntName]]))),

    ?assertEqual(ok, notification_config_db:delete(EntName)),
    ?verifyAll.

fetch_enabled_config_by_type_when_smtp_forwards_db_error() ->
    Change = change,
    ChangeId = 14,
    EntName = <<"NCC-1701">>,
    Error = {error, nooowhyyy},

    hoax:mock(deliv_change, [
              ?expect(getval,
                      ?withArgs([id, Change]),
                      ?andReturn(ChangeId)),
              ?expect(scoping_names,
                      ?withArgs([ChangeId]),
                      ?andReturn([EntName, <<"dontcare">>, <<"dontcare">>]))
    ]),

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([notification_config_db, fetch_enabled_by_type, [smtp, EntName, <<"dontcare">>, <<"dontcare">>]]),
                      ?andReturn(Error))),

    Result = notification_config_db:fetch_enabled_config_by_type(smtp, Change),

    ?assertEqual(Error, Result),
    ?verifyAll.

fetch_enabled_config_by_type_when_slack_webhook_forwards_db_error() ->
    Change = change,
    ChangeId = 14,
    EntName = <<"NCC-1701">>,
    OrgName = <<"dontcare">>,
    Error = {error, nooowhyyy},

    hoax:mock(deliv_change, [
              ?expect(getval,
                      ?withArgs([id, Change]),
                      ?andReturn(ChangeId)),
              ?expect(scoping_names,
                      ?withArgs([ChangeId]),
                      ?andReturn([EntName, OrgName, <<"dontcare">>]))
    ]),

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([notification_config_db, fetch_enabled_by_type, [slack_webhook, EntName, OrgName, <<"dontcare">>]]),
                      ?andReturn(Error))),

    Result = notification_config_db:fetch_enabled_config_by_type(slack_webhook, Change),

    ?assertEqual(Error, Result),
    ?verifyAll.
