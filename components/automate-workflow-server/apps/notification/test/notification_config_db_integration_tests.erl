-module(notification_config_db_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_types.hrl").

-compile([export_all]).

delete_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "delete_", setup, teardown).

fetch_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "fetch_", setup, teardown).

upsert_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "upsert_", setup, teardown).

setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup(),
    eu_data:with_enterprise(<<"NCC-1701">>,
      eu_data:with_organization(<<"BorgOrg">>,
        eu_data:with_project(<<"OneOfUs">>,
          eu_data:with_pipeline(<<"master">>, fun(Ent, Org, Proj, Pipe) ->
            User = eu_data:fetch_or_create_user(Ent, <<"JLPicard">>),
            Patchset = eu_data:create_patchset(Ent, User, Org, Proj, Pipe, <<"Assimilate">>),
            Change = eu_data:change_from_patchset(Patchset),

            [Org, Change, Proj]
        end)))).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

upsert_webhook_notification_config([Organization | _]) ->
    OrgId = deliv_organization:getval(id, Organization),
    Config = #notification_config{
        notification_type = slack_webhook,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.slack.com/whatevs">>}
        ]},
        enabled = true,
        organization_id = OrgId
    },

    Result = notification_config_db:upsert(Config),
    ?assert(is_list(Result)).

upsert_webhook_notification_config_with_project([_Org, _Change, Project | _]) ->
    ProjId = deliv_project:getval(id, Project),
    Name = <<"seven-of-nine">>,
    Settings = {[
        {<<"url">>, <<"https://my.slack.com/whatevs">>}
    ]},

    Config = #notification_config{
        notification_type = slack_webhook,
        name = Name,
        settings = Settings,
        enabled = true,
        project_id = ProjId
    },

    DbSettings = chef_json:encode(Settings),
    Result = notification_config_db:upsert(Config),
    ?assertMatch([{notification_config_db, _, <<"slack_webhook">>, <<"seven-of-nine">>,
                   DbSettings, true, undefined, undefined, ProjId}], Result).

upsert_returns_error_on_insert_failure(_) ->
    ?assertMatch({error, _}, notification_config_db:upsert(#notification_config{})).

fetch_returns_empty_list_when_no_notification_config_defined(_) ->
    ?assertEqual([], notification_config_db:fetch(<<"NCC-1701">>, <<"BorgOrg">>)).

fetch_returns_notification_config_when_defined([Organization | _]) ->
    OrgId = deliv_organization:getval(id, Organization),
    Config = #notification_config{
        notification_type = slack_webhook,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.slack.com/whatevs">>}
        ]},
        enabled = true,
        organization_id = OrgId
    },
    notification_config_db:upsert(Config),

    ?assertEqual([Config], notification_config_db:fetch(<<"NCC-1701">>, <<"BorgOrg">>)).

fetch_returns_notification_config_when_defined_by_project([_, _, Project | _]) ->
    ProjId = deliv_project:getval(id, Project),
    Config = #notification_config{
        notification_type = slack_webhook,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.slack.com/whatevs">>}
        ]},
        enabled = true,
        project_id = ProjId
    },
    notification_config_db:upsert(Config),

    ?assertEqual([Config], notification_config_db:fetch(<<"NCC-1701">>, <<"BorgOrg">>, <<"OneOfUs">>)).

fetch_returns_empty_list_when_no_notification_config_defined_for_project(_) ->
    ?assertEqual([], notification_config_db:fetch(<<"NCC-1701">>, <<"BorgOrg">>, <<"OneOfUs">>)).

upsert_updates_notification_config_for_org_when_already_exists([Organization | _]) ->
    OrgId = deliv_organization:getval(id, Organization),
    Config = #notification_config{
        notification_type = slack_webhook,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.slack.com/whatevs">>}
        ]},
        enabled = true,
        organization_id = OrgId
    },

    notification_config_db:upsert(Config),

    Config2 = #notification_config{
        notification_type = slack_webhook,
        name = <<"different-name">>,
        settings = {[
            {<<"url">>, <<"https://my.slack.com/whatevs">>}
        ]},
        enabled = true,
        organization_id = OrgId
    },

    notification_config_db:upsert(Config2),

    [ConfigurationsForOrg] = notification_config_db:fetch(<<"NCC-1701">>, <<"BorgOrg">>),

    ?assertEqual(Config2, ConfigurationsForOrg).

delete_returns_ok_on_success([Organization | _]) ->
    OrgId = deliv_organization:getval(id, Organization),
    Config = #notification_config{
        notification_type = slack_webhook,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.slack.com/whatevs">>}
        ]},
        enabled = true,
        organization_id = OrgId
    },

    notification_config_db:upsert(Config),

    Result = notification_config_db:delete(<<"NCC-1701">>, <<"BorgOrg">>),

    ?assertEqual(ok, Result).

delete_returns_ok_when_no_configuration_exists_for_org(_) ->
    Result = notification_config_db:delete(<<"NCC-1701">>, <<"BorgOrg">>),
    ?assertEqual(ok, Result).

upsert_smtp_notification_config([Organization | _]) ->
    EntId = deliv_organization:getval(enterprise_id, Organization),
    Config = #notification_config{
        notification_type = smtp,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.smtpserver.com">>},
            {<<"port">>, 25},
            {<<"smtp_login">>, <<"postmaster">>},
            {<<"password">>, <<"drventuresucks">>},
            {<<"sender_email">>, <<"widewale@councilof13.com">>},
            {<<"sender_name">>, <<"Council of 13">>}
        ]},
        enabled = true,
        enterprise_id = EntId
    },

    notification_config_db:upsert(Config),

    [ConfigurationsForEnt] = notification_config_db:fetch(<<"NCC-1701">>),

    ?assertEqual(Config, ConfigurationsForEnt).

upsert_smtp_notification_config_without_password([Organization | _]) ->
    EntId = deliv_organization:getval(enterprise_id, Organization),
    Config = #notification_config{
        notification_type = smtp,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.smtpserver.com">>},
            {<<"port">>, 25},
            {<<"smtp_login">>, <<"postmaster">>},
            {<<"sender_email">>, <<"widewale@councilof13.com">>},
            {<<"sender_name">>, <<"Council of 13">>}
        ]},
        enabled = true,
        enterprise_id = EntId
    },

    notification_config_db:upsert(Config),

    [ConfigurationsForEnt] = notification_config_db:fetch(<<"NCC-1701">>),

    ?assertEqual(Config, ConfigurationsForEnt).

upsert_smtp_notification_config_without_password_and_without_login([Organization | _]) ->
    EntId = deliv_organization:getval(enterprise_id, Organization),
    Config = #notification_config{
        notification_type = smtp,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.smtpserver.com">>},
            {<<"port">>, 25},
            {<<"sender_email">>, <<"widewale@councilof13.com">>},
            {<<"sender_name">>, <<"Council of 13">>}
        ]},
        enabled = true,
        enterprise_id = EntId
    },

    notification_config_db:upsert(Config),

    [ConfigurationsForEnt] = notification_config_db:fetch(<<"NCC-1701">>),

    ?assertEqual(Config, ConfigurationsForEnt).

delete_enterprise_returns_ok_on_success([Organization | _]) ->
    EntId = deliv_organization:getval(enterprise_id, Organization),
    Config = #notification_config{
        notification_type = smtp,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.smtpserver.com">>},
            {<<"port">>, 25},
            {<<"smtp_login">>, <<"postmaster">>},
            {<<"password">>, <<"drventuresucks">>},
            {<<"sender_email">>, <<"widewale@councilof13.com">>},
            {<<"sender_name">>, <<"Council of 13">>}
        ]},
        enabled = true,
        enterprise_id = EntId
    },

    notification_config_db:upsert(Config),
    Result = notification_config_db:delete(<<"NCC-1701">>),

    ?assertEqual(ok, Result).

delete_enterprise_returns_ok_if_no_config_found(_) ->
    Result = notification_config_db:delete(<<"NCC-1701">>),

    ?assertEqual(ok, Result).

delete_project_config_returns_ok_on_success([_, _, Proj | _]) ->
    ProjId = deliv_project:getval(id, Proj),
    {EntName, OrgName, ProjName} = deliv_project:scoping_names_by_id(ProjId),

    Config = #notification_config{
        notification_type = slack_webhook,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.slackwebhook.com">>}
        ]},
        enabled = true,
        project_id = ProjId
    },

    notification_config_db:upsert(Config),
    Result = notification_config_db:delete(EntName, OrgName, ProjName),
    [] = notification_config_db:fetch(EntName, OrgName, ProjName), % assert it was really deleted

    ?assertEqual(ok, Result).

delete_project_config_returns_ok_when_not_found([_, _, Proj | _]) ->
    ProjId = deliv_project:getval(id, Proj),
    {EntName, OrgName, ProjName} = deliv_project:scoping_names_by_id(ProjId),

    Result = notification_config_db:delete(EntName, OrgName, ProjName),
    [] = notification_config_db:fetch(EntName, OrgName, ProjName), % assert it was really deleted

    ?assertEqual(ok, Result).

fetch_enabled_config_by_type_returns_smtp_config_when_called_by_smtp([Org, Change | _]) ->
    EntId = deliv_organization:getval(enterprise_id, Org),
    Config = #notification_config{
        notification_type = smtp,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.smtpserver.com">>},
            {<<"port">>, 25},
            {<<"smtp_login">>, <<"postmaster">>},
            {<<"password">>, <<"drventuresucks">>},
            {<<"sender_email">>, <<"widewale@councilof13.com">>},
            {<<"sender_name">>, <<"Council of 13">>}
        ]},
        enabled = true,
        enterprise_id = EntId
    },

    notification_config_db:upsert(Config),

    Result = notification_config_db:fetch_enabled_config_by_type(smtp, Change),
    ?assertEqual([Config], Result).

fetch_enabled_config_by_type_when_smtp_returns_empty_list_when_config_not_enabled([Org, Change | _]) ->
    EntId = deliv_organization:getval(enterprise_id, Org),
    Config = #notification_config{
        notification_type = smtp,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.smtpserver.com">>},
            {<<"port">>, 25},
            {<<"smtp_login">>, <<"postmaster">>},
            {<<"password">>, <<"drventuresucks">>},
            {<<"sender_email">>, <<"widewale@councilof13.com">>},
            {<<"sender_name">>, <<"Council of 13">>}
        ]},
        enabled = false,
        enterprise_id = EntId
    },

    notification_config_db:upsert(Config),

    Result = notification_config_db:fetch_enabled_config_by_type(smtp, Change),
    ?assertEqual([], Result).

fetch_enabled_config_by_type_when_smtp_returns_empty_list_when_config_not_found([_Org, Change | _]) ->
    Result = notification_config_db:fetch_enabled_config_by_type(smtp, Change),
    ?assertEqual([], Result).

fetch_enabled_config_by_type_when_slack_webhook_returns_slack_webhook_config([Org, Change | _]) ->
    OrgId = deliv_organization:getval(id, Org),
    Config = #notification_config{
        notification_type = slack_webhook,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.slackwebook.com">>}
        ]},
        enabled = true,
        organization_id = OrgId
    },

    notification_config_db:upsert(Config),

    Result = notification_config_db:fetch_enabled_config_by_type(slack_webhook, Change),
    ?assertEqual([Config], Result).

fetch_enabled_config_by_type_when_two_slack_webhooks_returns_slack_webhook_configs([Org, Change, Project | _]) ->
    OrgId = deliv_organization:getval(id, Org),
    ProjId = deliv_project:getval(id, Project),
    Config = #notification_config{
        notification_type = slack_webhook,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.slackwebook.com">>}
        ]},
        enabled = true,
        organization_id = OrgId
    },

    Config2 = #notification_config{
        notification_type = slack_webhook,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.slackwebook.com">>}
        ]},
        enabled = true,
        project_id = ProjId
    },

    notification_config_db:upsert(Config),
    notification_config_db:upsert(Config2),

    Result = notification_config_db:fetch_enabled_config_by_type(slack_webhook, Change),

    ?assertEqual(2, length(Result)),

    ?assert(lists:any(
        fun(ResultConfig) -> ResultConfig == Config end,
        Result
    )),

    ?assert(lists:any(
        fun(ResultConfig) -> ResultConfig == Config2 end,
        Result
    )).

fetch_enabled_config_by_type_when_slack_webhook_returns_empty_list_when_config_not_enabled([Org, Change | _]) ->
    OrgId = deliv_organization:getval(id, Org),
    Config = #notification_config{
        notification_type = slack_webhook,
        name = <<"seven-of-nine">>,
        settings = {[
            {<<"url">>, <<"https://my.slackwebook.com">>}
        ]},
        enabled = false,
        organization_id = OrgId
    },

    notification_config_db:upsert(Config),

    Result = notification_config_db:fetch_enabled_config_by_type(slack_webhook, Change),
    ?assertEqual([], Result).

fetch_enabled_config_by_type_when_slack_webhook_returns_empty_list_when_config_not_found([_Org, Change | _]) ->
    Result = notification_config_db:fetch_enabled_config_by_type(slack_webhook, Change),
    ?assertEqual([], Result).
