-module(notification_notifier_slack).

% Sends slack notifications.

-behaviour(notification_notifier).

-include("notification_types.hrl").
-include("notification_macros.hrl").

-export([
         notification_type/0,
         notify/3
        ]).

-spec notification_type() -> slack_webhook.
notification_type() -> slack_webhook.

-spec notify(#notification_config{},
             notification_event(),
             d_change() | {d_change(), d_comment()}) -> ok.
notify(#notification_config{settings = Settings}, EventType, EventPayload) ->
    Json = notification_slack_content:EventType(EventPayload),
    WebhookURL = ej:get([<<"url">>], Settings),
    Headers = [{"Content-Type", "application/json"}],
    case deliv_http:req(post, WebhookURL, Json, Headers) of
        {ok, Status, _RespHeaders, _RespBody} when Status >= 200, Status < 300 ->
            ok;
        {ok, Status, _RespHeaders, RespBody} ->
            chef_log:error("Failed to notify Slack webhook at ~s: ~p, ~s",
                            [WebhookURL, Status, RespBody]);
        {error, _Why} ->
            chef_log:error("Failed to notify Slack webhook at ~s: unreachable",
                            [WebhookURL])
    end,
    ok.
