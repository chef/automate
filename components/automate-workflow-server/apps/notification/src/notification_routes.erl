-module(notification_routes).

-export([
         routes/0
        ]).

-include_lib("delivery/include/deliv_types.hrl").

-define(ALL_ROLES, [<<"admin">>, <<"committer">>, <<"reviewer">>, <<"shipper">>, <<"observer">>]).

routes() ->
    State = deliv_routes:set_ttls(#handler{}),

    [
     {deliv_web_utils:route_prefix() ++ ":ent_name/notifications/slack-webhook/test",
        notification_hand_test_notification,
        State#handler{authz = {enterprise, [{deliv_web_utils:bin_method(put), [<<"admin">>]}]}}},

     %% allows enterprise admins, too. authorization scopes upward.
     {deliv_web_utils:route_prefix() ++ ":ent_name/orgs/:org_name/notifications/slack-webhook",
        notification_hand_slack_webhook,
        State#handler{authz = {organization,[{deliv_web_utils:bin_method(delete), [<<"admin">>]},
                                             {deliv_web_utils:bin_method(get), ?ALL_ROLES},
                                             {deliv_web_utils:bin_method(put), [<<"admin">>]}]}}},

     {deliv_web_utils:route_prefix() ++ ":ent_name/notifications/smtp",
      notification_hand_smtp,
      State#handler{authz = {enterprise, [{deliv_web_utils:bin_method(delete), [<<"admin">>]},
                                          {deliv_web_utils:bin_method(get), [<<"admin">>]},
                                          {deliv_web_utils:bin_method(put), [<<"admin">>]}]}}},

     {deliv_web_utils:route_prefix() ++ ":ent_name/notifications/smtp/test",
      notification_hand_test_smtp,
      State#handler{authz = {enterprise, [{deliv_web_utils:bin_method(post), [<<"admin">>]}]}}},

     {deliv_web_utils:route_prefix() ++ ":ent_name/notifications",
      notification_hand_config_for_ent,
      State#handler{authz = {enterprise, [{deliv_web_utils:bin_method(get), ?ALL_ROLES}]}}},

     {deliv_web_utils:route_prefix() ++ ":ent_name/orgs/:org_name/projects/:proj_name/notifications/watch",
      notification_hand_watch,
      State#handler{authz = {project, [{deliv_web_utils:bin_method(get), ?ALL_ROLES},
                                       {deliv_web_utils:bin_method(put), ?ALL_ROLES}]}}},

     {deliv_web_utils:route_prefix() ++
        ":ent_name/orgs/:org_name/projects/:proj_name/notifications/slack-webhook",
      notification_hand_project_slack_webhook,
      State#handler{authz = {organization, [{deliv_web_utils:bin_method(delete), [<<"admin">>]},
                                            {deliv_web_utils:bin_method(get), ?ALL_ROLES},
                                            {deliv_web_utils:bin_method(put), [<<"admin">>]}]}}}
    ].
