-module(notification_routes_tests).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

routes_should_return_notification_route_list_test() ->
    ok = application:set_env(delivery, read_ttl_secs, 3600),
    ok = application:set_env(delivery, write_ttl_secs, 3600),

    State = deliv_routes:set_ttls(#handler{}),

    TestNotificationRoute = {
        deliv_web_utils:route_prefix() ++ ":ent_name/notifications/slack-webhook/test",
        notification_hand_test_notification,
        State#handler{authz = {enterprise, [{deliv_web_utils:bin_method(put),[<<"admin">>]}]}}},

     NotificationSlackConfigRoute = {
        deliv_web_utils:route_prefix() ++ ":ent_name/orgs/:org_name/notifications/slack-webhook",
        notification_hand_slack_webhook,
        State#handler{authz = {organization, [
                                 {deliv_web_utils:bin_method(delete), [<<"admin">>]},
                                 {deliv_web_utils:bin_method(get), [<<"admin">>,
                                                                    <<"committer">>,
                                                                    <<"reviewer">>,
                                                                    <<"shipper">>,
                                                                    <<"observer">>]},
                                 {deliv_web_utils:bin_method(put), [<<"admin">>]}
                               ]}}
    },

    NotificationConfigSmtpRoute = {
        deliv_web_utils:route_prefix() ++ ":ent_name/notifications/smtp",
        notification_hand_smtp,
        State#handler{authz = {enterprise, [
                                {deliv_web_utils:bin_method(delete), [<<"admin">>]},
                                {deliv_web_utils:bin_method(get), [<<"admin">>]},
                                {deliv_web_utils:bin_method(put), [<<"admin">>]}
                              ]}}
    },

    TestSmtpRoute = {
        deliv_web_utils:route_prefix() ++ ":ent_name/notifications/smtp/test",
        notification_hand_test_smtp,
        State#handler{authz = {enterprise, [
                                 {deliv_web_utils:bin_method(post), [<<"admin">>]}
                               ]}}
    },

    ConfigurationsForEnterpriseRoute = {
        deliv_web_utils:route_prefix() ++ ":ent_name/notifications",
        notification_hand_config_for_ent,
        State#handler{authz = {enterprise, [
                                 {deliv_web_utils:bin_method(get), [<<"admin">>,
                                                                    <<"committer">>,
                                                                    <<"reviewer">>,
                                                                    <<"shipper">>,
                                                                    <<"observer">>]}
                               ]}}

    },

    WatchProjectRoute = {
        deliv_web_utils:route_prefix() ++ ":ent_name/orgs/:org_name/projects/:proj_name/notifications/watch",
        notification_hand_watch,
        State#handler{authz = {project, [
                                 {deliv_web_utils:bin_method(get), [<<"admin">>,
                                                                    <<"committer">>,
                                                                    <<"reviewer">>,
                                                                    <<"shipper">>,
                                                                    <<"observer">>]},
                                 {deliv_web_utils:bin_method(put), [<<"admin">>,
                                                                    <<"committer">>,
                                                                    <<"reviewer">>,
                                                                    <<"shipper">>,
                                                                    <<"observer">>]}
                               ]}}

    },

    NotificationSlackConfigProjectRoute = {
        deliv_web_utils:route_prefix() ++ ":ent_name/orgs/:org_name/projects/:proj_name/notifications/slack-webhook",
        notification_hand_project_slack_webhook,
        State#handler{authz = {organization, [
                                 {deliv_web_utils:bin_method(delete), [<<"admin">>]},
                                 {deliv_web_utils:bin_method(get), [<<"admin">>,
                                                                    <<"committer">>,
                                                                    <<"reviewer">>,
                                                                    <<"shipper">>,
                                                                    <<"observer">>]},
                                 {deliv_web_utils:bin_method(put), [<<"admin">>]}
                               ]}}
    },

    ExpectedRoutes = [TestNotificationRoute,
                      NotificationSlackConfigRoute,
                      NotificationConfigSmtpRoute,
                      TestSmtpRoute,
                      ConfigurationsForEnterpriseRoute,
                      WatchProjectRoute,
                      NotificationSlackConfigProjectRoute],

    ?assertEqual(ExpectedRoutes, notification_routes:routes()).
