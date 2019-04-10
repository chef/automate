-module(notification_notifier_smtp).

% Sends smtp notifications.

-behaviour(notification_notifier).

-include("notification_types.hrl").

-export([
         callback/1,
         notification_type/0,
         notify/3
        ]).

-spec notification_type() -> smtp.
notification_type() -> smtp.

-spec notify(#notification_config{},
             notification_event(),
             d_change() | {d_change(), d_comment()}) -> ok.
notify(Config, Event, EventPayload) ->
    case notification_smtp_content:Event(EventPayload) of
        {error, no_content} ->
            ok;
        EmailContent ->
            EmailAddresses = handle_fetch_emails(EventPayload, Event),
            handle_notify(EmailAddresses, EmailContent, Config)
    end.

%% Private

handle_notify({error, _Why}, _EmailContent, _Config) ->
    ok;
handle_notify(EmailAddresses, EmailContent, Config) ->
    [compose_and_send_email(EmailAddress, Config, EmailContent) || EmailAddress <- EmailAddresses],
    ok.

handle_fetch_emails({Change, _Comment}, Event) ->
    fetch_emails(Change, Event);
handle_fetch_emails(Change, Event) ->
    fetch_emails(Change, Event).

fetch_emails(Change, Event) ->
    ChangeId = deliv_change:getval(id, Change),
    [EntName, OrgName, ProjName | _ ] = deliv_change:scoping_names(ChangeId),
    notification_subscriptions:subscriber_emails_by_event(EntName, OrgName, ProjName, Event).

compose_and_send_email(_EmailAddress, _Config, {error, no_content}) ->
    %% SMTP doesn't send a notification for this event (yet, hehe)
    ok;
compose_and_send_email(EmailAddress, #notification_config{settings = Settings} = Config, EmailContent) ->
    Options = notification_config:gen_smtp_options(Settings),
    SenderEmailAddress = ej:get([<<"sender_email">>], Settings),
    SendableEmail = mimemail:encode(notification_smtp_content:compose_email(Config, EmailAddress, EmailContent)),
    gen_smtp_client:send({SenderEmailAddress, [EmailAddress], SendableEmail}, Options, fun ?MODULE:callback/1),
    ok.

callback({ok, _Receipt}) ->
    ok;
callback({error, Type, Message}) ->
    chef_log:error("Failed to deliver notification via smtp due to error: ~s ~p.", [Type, Message]);
callback({exit, ExitReason}) ->
    chef_log:error("Failed to deliver notification via smtp due to: ~s.", [ExitReason]).
