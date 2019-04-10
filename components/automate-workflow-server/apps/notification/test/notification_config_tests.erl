-module(notification_config_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_types.hrl").

-compile(export_all).

notification_config_test_() ->
    [
     hoax:fixture(?MODULE, save_),
     eunit_sugar:fixture(?MODULE, gen_smtp_options_),
     hoax:fixture(?MODULE, with_merged_password_)
    ].

save_saves_new_smtp_config() ->
    EntName = <<"Gizmonics">>,
    EntId = 1,
    Host = <<"smtp.gizmonics.com">>,
    Port = 25,
    Login = <<"tvsfrank@smtp.gizmonics.com">>,
    Password = <<"i<3iceDancing">>,
    SenderEmail = <<"tvsfrank@gizmonics.com">>,
    SenderName = <<"TVs Frank">>,

    JSON = {[
        {<<"host">>, Host},
        {<<"port">>, Port},
        {<<"smtp_login">>, Login},
        {<<"password">>, Password},
        {<<"sender_email">>, SenderEmail},
        {<<"sender_name">>, SenderName}
    ]},

    EncodedPassword = base64:encode(Password),
    Settings = ej:set([<<"password">>], JSON, EncodedPassword),

    ExpectedConfig = #notification_config{
        notification_type = smtp,
        settings = Settings,
        enabled = true,
        enterprise_id = EntId
    },

    hoax:mock(deliv_enterprise, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, enterprise})),
              ?expect(getval,
                      ?withArgs([name, enterprise]),
                      ?andReturn(EntName)),
              ?expect(getval,
                      ?withArgs([id, enterprise]),
                      ?andReturn(EntId))
              ]),
    hoax:mock(notification_config_db,
              ?expect(upsert,
                      ?withArgs([ExpectedConfig]),
                      ?andReturn([config]))),

    notification_config:save(smtp, [EntName], JSON),
    ?verifyAll.

save_smtp_config_without_password_updates_existing_smtp_config() ->
    EntName = <<"Gizmonics">>,
    EntId = 1,
    Host = <<"smtp.gizmonics.com">>,
    Port = 25,
    Login = <<"tvsfrank@smtp.gizmonics.com">>,
    SenderEmail = <<"tvsfrank@gizmonics.com">>,
    SenderName = <<"TVs Frank">>,

    JSON = {[
        {<<"host">>, Host},
        {<<"port">>, Port},
        {<<"smtp_login">>, Login},
        {<<"sender_email">>, SenderEmail},
        {<<"sender_name">>, SenderName}
    ]},

    Password = <<"i<3iceDancing">>,
    EncodedPassword = base64:encode(Password),
    Settings = ej:set([<<"password">>], JSON, EncodedPassword),

    OldSettings = ej:set([<<"sender_name">>], Settings, <<"Cambot">>),
    OldConfig = #notification_config{
        notification_type = smtp,
        settings = OldSettings,
        enabled = true,
        enterprise_id = EntId
    },

    ExpectedConfig = #notification_config{
        notification_type = smtp,
        settings = Settings,
        enabled = true,
        enterprise_id = EntId
    },

    hoax:mock(deliv_enterprise, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, enterprise})),
              ?expect(getval,
                      ?withArgs([name, enterprise]),
                      ?andReturn(EntName)),
              ?expect(getval,
                      ?withArgs([id, enterprise]),
                      ?andReturn(EntId))
              ]),
    hoax:mock(notification_config_db, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn([OldConfig])),
              ?expect(upsert,
                      ?withArgs([ExpectedConfig]),
                      ?andReturn([config]))
              ]),

    ?assertEqual([config], notification_config:save(smtp, [EntName], JSON)),
    ?verifyAll.

save_smtp_config_without_password_and_no_existing_config_stores_config_without_password_and_returns_success() ->
    EntName = <<"Gizmonics">>,
    EntId = 1,
    Host = <<"smtp.gizmonics.com">>,
    Port = 25,
    Login = <<"tvsfrank@smtp.gizmonics.com">>,
    SenderEmail = <<"tvsfrank@gizmonics.com">>,
    SenderName = <<"TVs Frank">>,

    JSON = {[
        {<<"host">>, Host},
        {<<"port">>, Port},
        {<<"smtp_login">>, Login},
        {<<"sender_email">>, SenderEmail},
        {<<"sender_name">>, SenderName}
    ]},

    ExpectedConfig = #notification_config{
        notification_type = smtp,
        settings = JSON,
        enabled = true,
        enterprise_id = EntId
    },

    hoax:mock(deliv_enterprise, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, enterprise})),
              ?expect(getval,
                      ?withArgs([name, enterprise]),
                      ?andReturn(EntName)),
              ?expect(getval,
                      ?withArgs([id, enterprise]),
                      ?andReturn(EntId))
              ]),
    hoax:mock(notification_config_db, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn([])),
              ?expect(upsert,
                      ?withArgs([ExpectedConfig]),
                      ?andReturn([config]))]),

    Actual = notification_config:save(smtp, [EntName], JSON),
    ?assertMatch([config], Actual),
    ?verifyAll.

save_smtp_config_without_password_and_without_smtp_login_and_no_existing_config_stores_config_without_password_and_without_smtp_login_and_returns_success() ->
    EntName = <<"Gizmonics">>,
    EntId = 1,
    Host = <<"smtp.gizmonics.com">>,
    Port = 25,
    SenderEmail = <<"tvsfrank@gizmonics.com">>,
    SenderName = <<"TVs Frank">>,

    JSON = {[
        {<<"host">>, Host},
        {<<"port">>, Port},
        {<<"sender_email">>, SenderEmail},
        {<<"sender_name">>, SenderName}
    ]},

    ExpectedConfig = #notification_config{
        notification_type = smtp,
        settings = JSON,
        enabled = true,
        enterprise_id = EntId
    },

    hoax:mock(deliv_enterprise, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, enterprise})),
              ?expect(getval,
                      ?withArgs([name, enterprise]),
                      ?andReturn(EntName)),
              ?expect(getval,
                      ?withArgs([id, enterprise]),
                      ?andReturn(EntId))
              ]),
    hoax:mock(notification_config_db, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn([])),
              ?expect(upsert,
                      ?withArgs([ExpectedConfig]),
                      ?andReturn([config]))]),

    Actual = notification_config:save(smtp, [EntName], JSON),
    ?assertMatch([config], Actual),
    ?verifyAll.

save_smtp_config_without_password_forwards_db_error() ->
    EntName = <<"Gizmonics">>,
    EntId = 1,
    Host = <<"smtp.gizmonics.com">>,
    Port = 25,
    Login = <<"tvsfrank@smtp.gizmonics.com">>,
    SenderEmail = <<"tvsfrank@gizmonics.com">>,
    SenderName = <<"TVs Frank">>,

    JSON = {[
        {<<"host">>, Host},
        {<<"port">>, Port},
        {<<"smtp_login">>, Login},
        {<<"sender_email">>, SenderEmail},
        {<<"sender_name">>, SenderName}
    ]},

    hoax:mock(deliv_enterprise, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, enterprise})),
              ?expect(getval,
                      ?withArgs([name, enterprise]),
                      ?andReturn(EntName)),
              ?expect(getval,
                      ?withArgs([id, enterprise]),
                      ?andReturn(EntId))
              ]),
    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({error, reason}))),

    Error = notification_config:save(smtp, [EntName], JSON),
    ?assertEqual({error, reason}, Error),
    ?verifyAll.

save_returns_error_ent_not_found_when_scope_is_ent_and_ent_not_found() ->
    EntName = <<"Gizmonics">>,
    Host = <<"smtp.gizmonics.com">>,
    Port = 25,
    Login = <<"tvsfrank@smtp.gizmonics.com">>,
    Password = <<"i<3iceDancing">>,
    SenderEmail = <<"tvsfrank@gizmonics.com">>,
    SenderName = <<"TVs Frank">>,

    JSON = {[
        {<<"host">>, Host},
        {<<"port">>, Port},
        {<<"smtp_login">>, Login},
        {<<"password">>, Password},
        {<<"sender_email">>, SenderEmail},
        {<<"sender_name">>, SenderName}
    ]},

    hoax:mock(deliv_enterprise,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({error, not_found}))),

    Error = notification_config:save(smtp, [EntName], JSON),
    ?assertEqual({error, ent_not_found}, Error),
    ?verifyAll.

save_saves_slack_webhook_config_for_org() ->
    EntName = <<"Gizmonics">>,
    OrgName = <<"Eegah">>,
    OrgId = 1,
    URL = <<"https://slack.webhook.com/url">>,
    Name = <<"#bonk-bonk">>,
    Enabled = true,

    JSON = {[
        {<<"name">>, Name},
        {<<"url">>, URL},
        {<<"enabled">>, Enabled}
    ]},

    ExpectedConfig = #notification_config{
        notification_type = slack_webhook,
        name = Name,
        settings = {[
            {<<"url">>, URL}
        ]},
        enabled = Enabled,
        organization_id = OrgId
    },

    hoax:mock(deliv_organization, [
              ?expect(fetch,
                      ?withArgs([EntName, OrgName]),
                      ?andReturn({ok, organization})),
              ?expect(getval,
                      ?withArgs([id, organization]),
                      ?andReturn(OrgId))
              ]),
    hoax:mock(notification_config_db,
              ?expect(upsert,
                      ?withArgs([ExpectedConfig]),
                      ?andReturn([config]))),

    notification_config:save(slack_webhook, [EntName, OrgName], JSON),
    ?verifyAll.

save_returns_error_org_not_found_when_scope_is_org_and_org_not_found() ->
    EntName = <<"Gizmonics">>,
    OrgName = <<"Eegah">>,
    URL = <<"https://slack.webhook.com/url">>,
    Name = <<"#bonk-bonk">>,
    Enabled = true,

    JSON = {[
        {<<"name">>, Name},
        {<<"url">>, URL},
        {<<"enabled">>, Enabled}
    ]},

    hoax:mock(deliv_organization,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName]),
                      ?andReturn({error, not_found}))),

    Error = notification_config:save(slack_webhook, [EntName, OrgName], JSON),
    ?assertEqual({error, org_not_found}, Error),
    ?verifyAll.

save_saves_slack_webhook_config_for_proj() ->
    EntName = <<"Gizmonics">>,
    OrgName = <<"Eegah">>,
    ProjName = <<"DuneBuggy">>,
    ProjId = 1,
    URL = <<"https://slack.webhook.com/url">>,
    Name = <<"#bonk-bonk">>,
    Enabled = true,

    JSON = {[
        {<<"name">>, Name},
        {<<"url">>, URL},
        {<<"enabled">>, Enabled}
    ]},

    ExpectedConfig = #notification_config{
        notification_type = slack_webhook,
        name = Name,
        settings = {[
            {<<"url">>, URL}
        ]},
        enabled = Enabled,
        project_id = ProjId
    },

    hoax:mock(deliv_project, [
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn({ok, project})),
              ?expect(getval,
                      ?withArgs([id, project]),
                      ?andReturn(ProjId))
              ]),
    hoax:mock(notification_config_db,
              ?expect(upsert,
                      ?withArgs([ExpectedConfig]),
                      ?andReturn(config))),

    notification_config:save(slack_webhook, [EntName, OrgName, ProjName], JSON),
    ?verifyAll.

save_returns_error_proj_not_found_when_scope_is_proj_and_proj_not_found() ->
    EntName = <<"Gizmonics">>,
    OrgName = <<"Eegah">>,
    ProjName = <<"DuneBuggy">>,
    URL = <<"https://slack.webhook.com/url">>,
    Name = <<"#bonk-bonk">>,
    Enabled = true,

    JSON = {[
        {<<"name">>, Name},
        {<<"url">>, URL},
        {<<"enabled">>, Enabled}
    ]},

    hoax:mock(deliv_project,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn({error, not_found}))),

    Error = notification_config:save(slack_webhook, [EntName, OrgName, ProjName], JSON),
    ?assertEqual({error, proj_not_found}, Error),
    ?verifyAll.

gen_smtp_options_when_smtp_login_and_password_are_set_returns_options_with_username_and_password() ->
    Host = <<"smtp.example.com">>,
    Port = 25,
    Username = <<"mail">>,
    Password = <<"password">>,
    EncodedPassword = base64:encode(Password),
    Settings = {[
                 {<<"password">>, EncodedPassword},
                 {<<"smtp_login">>, Username},
                 {<<"host">>, Host},
                 {<<"port">>, Port}
                ]},
    Opts = [{username, Username}, {password, Password}, {relay, Host}, {port, Port}],
    ?assertEqual(Opts, notification_config:gen_smtp_options(Settings)).

gen_smtp_options_when_smtp_login_is_set_and_password_is_not_set_returns_options_without_username_and_password() ->
    Host = <<"smtp.example.com">>,
    Port = 25,
    Username = <<"mail">>,
    Settings = {[
                 {<<"smtp_login">>, Username},
                 {<<"host">>, Host},
                 {<<"port">>, Port}
                ]},
    Opts = [{relay, Host}, {port, Port}],
    ?assertEqual(Opts, notification_config:gen_smtp_options(Settings)).

gen_smtp_options_when_smtp_login_and_password_are_both_not_set_returns_options_without_username_and_password() ->
    Host = <<"smtp.example.com">>,
    Port = 25,
    Settings = {[
                 {<<"host">>, Host},
                 {<<"port">>, Port}
                ]},
    Opts = [{relay, Host}, {port, Port}],
    ?assertEqual(Opts, notification_config:gen_smtp_options(Settings)).

with_merged_password_when_a_new_password_is_given_returns_settings_with_new_password_irregardless_of_stored_settings() ->
    EntName = <<"ent">>,
    IncomingSettings = {[
                         {<<"password">>, <<"new password">>}
                        ]},
    Expected = {ok, {[{<<"password">>, base64:encode(<<"new password">>)}]}},

    Actual = notification_config:with_merged_password(EntName, IncomingSettings),
    ?assertEqual(Expected, Actual),
    ?verifyAll.

with_merged_password_when_a_notification_config_exists_for_enterprise_and_no_new_password_is_given_returns_settings_with_password_from_old_config() ->
    EntName = <<"ent">>,
    IncomingSettings = {[]},
    OldSettings = {[
                    {<<"password">>, base64:encode(<<"old password">>)}
                   ]},
    OldConfig = #notification_config{notification_type = smtp,
                                     settings = OldSettings},
    hoax:expect(receive
                    notification_config_db:fetch(EntName) -> [OldConfig]
                end),
    Expected = {ok, {[{<<"password">>, base64:encode(<<"old password">>)}]}},

    Actual = notification_config:with_merged_password(EntName, IncomingSettings),
    ?assertEqual(Expected, Actual),
    ?verifyAll.

with_merged_password_when_a_notification_config_exists_for_enterprise_with_no_password_stored_and_no_new_password_is_given_returns_settings_from_new_config_with_no_password() ->
    EntName = <<"ent">>,
    IncomingSettings = {[{<<"host">>, <<"new.host">>}]},
    OldSettings = {[{<<"host">>, <<"old.host">>}]},
    OldConfig = #notification_config{notification_type = smtp,
                                     settings = OldSettings},
    hoax:expect(receive
                    notification_config_db:fetch(EntName) -> [OldConfig]
                end),
    Expected = {ok, {[{<<"host">>, <<"new.host">>}]}},

    Actual = notification_config:with_merged_password(EntName, IncomingSettings),
    ?assertEqual(Expected, Actual),
    ?verifyAll.

with_merged_password_when_no_notification_config_exists_for_enterprise_and_no_new_password_is_given_returns_new_settings_with_no_password() ->
    EntName = <<"ent">>,
    IncomingSettings = {[{<<"host">>, <<"new.host">>}]},
    hoax:expect(receive
                    notification_config_db:fetch(EntName) -> []
                end),
    Expected = {ok, IncomingSettings},

    Actual = notification_config:with_merged_password(EntName, IncomingSettings),
    ?assertEqual(Expected, Actual),
    ?verifyAll.

with_merged_password_when_no_password_is_given_and_db_lookups_fails_forwards_error() ->
    EntName = <<"ent">>,
    IncomingSettings = {[{<<"host">>, <<"new.host">>}]},
    hoax:expect(receive
                    notification_config_db:fetch(EntName) -> {error, reason}
                end),
    Expected = {error, reason},

    Actual = notification_config:with_merged_password(EntName, IncomingSettings),
    ?assertEqual(Expected, Actual),
    ?verifyAll.
