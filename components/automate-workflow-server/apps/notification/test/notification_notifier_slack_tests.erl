-module(notification_notifier_slack_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_macros.hrl").
-include("../src/notification_types.hrl").

-compile(export_all).

notify_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "notify_", setup, teardown).

setup() ->
    %% Create a notification config
    OrgId = 1,
    WebhookURL = <<"https://my.slack-webhook.url">>,
    #notification_config{notification_type = slack_webhook,
                         name = <<"helper">>,
                         settings = {[ {<<"url">>, WebhookURL} ]},
                         enabled = true,
                         organization_id = OrgId}.

teardown(_) ->
    ok.

notification_type_returns_slack_webhook_test() ->
    ?assertEqual(slack_webhook, notification_notifier_slack:notification_type()).

notify_creates_appropriate_json_and_logs_error_when_not_200(#notification_config{settings = Settings} = Config) ->
    WebhookURL = ej:get([<<"url">>], Settings),
    Headers = [{"Content-Type", "application/json"}],
    Json = {[]},

    hoax:mock(notification_slack_content,
              ?expect(verify_passed,
                      ?withArgs([change]),
                      ?andReturn(Json))),

    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, WebhookURL, Json, Headers]),
                      ?andReturn({ok, 404, resp_headers, <<"Bad token">>}))),

    hoax:mock(chef_log,
              ?expect(error,
              ?withArgs(["Failed to notify Slack webhook at ~s: ~p, ~s",
                          [WebhookURL, 404, <<"Bad token">>]]))),


    Result = notification_notifier_slack:notify(Config, verify_passed, change),
    ?assertEqual(ok, Result),
    ?verifyAll.

notify_creates_appropriate_json_and_cannot_post_logs_error(#notification_config{settings = Settings} = Config) ->
    WebhookURL = ej:get([<<"url">>], Settings),
    Headers = [{"Content-Type", "application/json"}],
    Json = {[]},

    hoax:mock(notification_slack_content,
              ?expect(verify_passed,
                      ?withArgs([change]),
                      ?andReturn(Json))),

    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, WebhookURL, Json, Headers]),
                      ?andReturn({error, reason}))),

    hoax:mock(chef_log,
              ?expect(error,
                      ?withArgs(["Failed to notify Slack webhook at ~s: unreachable",
                                  [WebhookURL]]))),


    Result = notification_notifier_slack:notify(Config, verify_passed, change),
    ?assertEqual(ok, Result),
    ?verifyAll.

notify_sends_slack_notification(#notification_config{settings = Settings} = Config) ->
    WebhookURL = ej:get([<<"url">>], Settings),
    Headers = [{"Content-Type", "application/json"}],
    Json = {[]},

    hoax:mock(notification_slack_content,
              ?expect(verify_passed,
                      ?withArgs([change]),
                      ?andReturn(Json))),

    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, WebhookURL, Json, Headers]),
                      ?andReturn({ok, 200, resp_headers, resp_body}))),
    hoax:mock(chef_log,
              ?expect(error,
                      ?withArgs(["Failed to notify Slack webhook at ~s: unreachable",
                                  [WebhookURL]]),
                      ?times(0))),

    Result = notification_notifier_slack:notify(Config, verify_passed, change),
    ?assertEqual(ok, Result),
    ?verifyAll.

notify_gets_correct_json_when_comment_created_event_and_sends_slack_notification(#notification_config{settings = Settings} = Config) ->
    WebhookURL = ej:get([<<"url">>], Settings),
    Headers = [{"Content-Type", "application/json"}],
    Json = {[]},

    hoax:mock(notification_slack_content,
              ?expect(comment_created,
                      ?withArgs([{change, comment}]),
                      ?andReturn(Json))),

    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, WebhookURL, Json, Headers]),
                      ?andReturn({ok, 200, resp_headers, resp_body}))),
    hoax:mock(chef_log,
              ?expect(error,
                      ?withArgs(["Failed to notify Slack webhook at ~s: unreachable",
                                  [WebhookURL]]),
                      ?times(0))),

    Result = notification_notifier_slack:notify(Config, comment_created, {change, comment}),
    ?assertEqual(ok, Result),
    ?verifyAll.
