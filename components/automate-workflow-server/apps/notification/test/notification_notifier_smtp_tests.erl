-module(notification_notifier_smtp_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_types.hrl").

-compile(export_all).

notify_fixture_test_() ->
    hoax:fixture(?MODULE, "notify").

callback_fixture_test_() ->
    hoax:fixture(?MODULE, "callback").

%% Creates SMTP configuration JSON, without optional parameters set.
config_json() -> config_json(auth).

config_json(auth) ->
    {[
      {<<"host">>, <<"smtp.gmail.com">>},
      {<<"port">>, 587},
      {<<"smtp_login">>, <<"Hank Venture">>},
      {<<"password">>, base64:encode(<<"IamBatman">>)},
      {<<"sender_email">>, <<"hank@hank.co">>}
     ]};
config_json(noauth) ->
    {[
      {<<"host">>, <<"smtp.gmail.com">>},
      {<<"port">>, 587},
      {<<"sender_email">>, <<"hank@hank.co">>}
     ]}.

smtp_options(Settings) -> smtp_options(Settings, auth).

smtp_options(Settings, auth) ->
    [
     {username, ej:get([<<"smtp_login">>], Settings)},
     {password, base64:decode(ej:get([<<"password">>], Settings))},
     {relay, ej:get([<<"host">>], Settings)},
     {port, ej:get([<<"port">>], Settings)}
    ];
smtp_options(Settings, noauth) ->
    [
     {relay, ej:get([<<"host">>], Settings)},
     {port, ej:get([<<"port">>], Settings)}
    ].

notification_type_returns_smtp_test() ->
    ?assertEqual(smtp, notification_notifier_smtp:notification_type()).

notify_does_nothing_if_event_type_is_not_yet_handled() ->
    Settings = config_json(),
    Config = #notification_config{notification_type = smtp, settings = Settings},

    hoax:mock(notification_smtp_content,
              ?expect(verify_passed,
                      ?withArgs([change]),
                      ?andReturn({error, no_content}))),

    Result = notification_notifier_smtp:notify(Config, verify_passed, change),
    ?verifyAll,
    ?assertEqual(ok, Result).

notify_composes_and_sends_email_to_a_single_fetched_user() ->
    Settings = config_json(),
    Config = #notification_config{notification_type = smtp, settings = Settings},
    ChangeId = <<"1">>,
    EntName = <<"Hank Co">>,
    OrgName = <<"Super World">>,
    ProjName = <<"Hank Bucks">>,
    UserEmailAddress = <<"dean@ventech.com">>,
    SenderEmail = ej:get([<<"sender_email">>], Settings),
    Options = smtp_options(Settings),

    hoax:mock(deliv_change, [
              ?expect(getval,
                      ?withArgs([id, change]),
                      ?andReturn(ChangeId)),
              ?expect(scoping_names,
                      ?withArgs([ChangeId]),
                      ?andReturn([EntName, OrgName, ProjName]))]),
    hoax:mock(notification_subscriptions,
              ?expect(subscriber_emails_by_event,
                      ?withArgs([EntName, OrgName, ProjName, verify_passed]),
                      ?andReturn([UserEmailAddress]))),
    hoax:mock(notification_smtp_content, [
              ?expect(verify_passed,
                      ?withArgs([change]),
                      ?andReturn(email_content)),
              ?expect(compose_email,
                      ?withArgs([Config, UserEmailAddress, email_content]),
                      ?andReturn(full_email))]),
    hoax:mock(mimemail,
              ?expect(encode,
                      ?withArgs([full_email]),
                      ?andReturn(sendable_email))),
    hoax:mock(gen_smtp_client,
              ?expect(send,
                      ?withArgs([{SenderEmail, [UserEmailAddress], sendable_email}, Options, fun notification_notifier_smtp:callback/1]))),

    Result = notification_notifier_smtp:notify(Config, verify_passed, change),
    ?verifyAll,
    ?assertEqual(ok, Result).


notify_composes_and_sends_email_to_a_single_fetched_user_when_there_is_no_smtp_login_and_password_setting() ->
    Settings = config_json(noauth),
    Config = #notification_config{notification_type = smtp, settings = Settings},
    ChangeId = <<"1">>,
    EntName = <<"Hank Co">>,
    OrgName = <<"Super World">>,
    ProjName = <<"Hank Bucks">>,
    UserEmailAddress = <<"dean@ventech.com">>,
    SenderEmail = ej:get([<<"sender_email">>], Settings),
    Options = smtp_options(Settings, noauth),

    hoax:mock(deliv_change, [
              ?expect(getval,
                      ?withArgs([id, change]),
                      ?andReturn(ChangeId)),
              ?expect(scoping_names,
                      ?withArgs([ChangeId]),
                      ?andReturn([EntName, OrgName, ProjName]))]),
    hoax:mock(notification_subscriptions,
              ?expect(subscriber_emails_by_event,
                      ?withArgs([EntName, OrgName, ProjName, verify_passed]),
                      ?andReturn([UserEmailAddress]))),
    hoax:mock(notification_smtp_content, [
              ?expect(verify_passed,
                      ?withArgs([change]),
                      ?andReturn(email_content)),
              ?expect(compose_email,
                      ?withArgs([Config, UserEmailAddress, email_content]),
                      ?andReturn(full_email))]),
    hoax:mock(mimemail,
              ?expect(encode,
                      ?withArgs([full_email]),
                      ?andReturn(sendable_email))),
    hoax:mock(gen_smtp_client,
              ?expect(send,
                      ?withArgs([{SenderEmail, [UserEmailAddress], sendable_email}, Options, fun notification_notifier_smtp:callback/1]))),

    Result = notification_notifier_smtp:notify(Config, verify_passed, change),
    ?verifyAll,
    ?assertEqual(ok, Result).

notify_composes_and_sends_email_to_multiple_fetched_users() ->
    Settings = config_json(),
    Config = #notification_config{notification_type = smtp, settings = Settings},
    ChangeId = <<"1">>,
    EntName = <<"Hank Co">>,
    OrgName = <<"Super World">>,
    ProjName = <<"Hank Bucks">>,
    UserEmailAddress = <<"dean@ventech.com">>,
    SecondUserAddress = <<"mrwhite@conjectural.tech">>,
    SenderEmail = ej:get([<<"sender_email">>], Settings),
    Options = smtp_options(Settings),

    hoax:mock(deliv_change, [
              ?expect(getval,
                      ?withArgs([id, change]),
                      ?andReturn(ChangeId)),
              ?expect(scoping_names,
                      ?withArgs([ChangeId]),
                      ?andReturn([EntName, OrgName, ProjName]))]),
    hoax:mock(notification_subscriptions,
              ?expect(subscriber_emails_by_event,
                      ?withArgs([EntName, OrgName, ProjName, verify_passed]),
                      ?andReturn([UserEmailAddress, SecondUserAddress]))),
    hoax:mock(notification_smtp_content, [
              ?expect(verify_passed,
                      ?withArgs([change]),
                      ?andReturn(email_content)),
              ?expect(compose_email,
                      ?withArgs([Config, UserEmailAddress, email_content]),
                      ?andReturn(full_email)),
              ?expect(compose_email,
                      ?withArgs([Config, SecondUserAddress, email_content]),
                      ?andReturn(full_email))]),
    hoax:mock(mimemail,
              ?expect(encode,
                      ?withArgs([full_email]),
                      ?andReturn(sendable_email))),
    hoax:mock(gen_smtp_client, [
              ?expect(send,
                      ?withArgs([{SenderEmail, [UserEmailAddress], sendable_email}, Options, fun notification_notifier_smtp:callback/1])),
              ?expect(send,
                      ?withArgs([{SenderEmail, [SecondUserAddress], sendable_email}, Options, fun notification_notifier_smtp:callback/1]))]),

    Result = notification_notifier_smtp:notify(Config, verify_passed, change),
    ?verifyAll,
    ?assertEqual(ok, Result).


notify_composes_and_sends_email_to_a_single_fetched_user_when_change_and_comment_are_payload() ->
    Settings = config_json(),
    Config = #notification_config{notification_type = smtp, settings = Settings},
    ChangeId = <<"1">>,
    EntName = <<"Hank Co">>,
    OrgName = <<"Super World">>,
    ProjName = <<"Hank Bucks">>,
    UserEmailAddress = <<"dean@ventech.com">>,
    SenderEmail = ej:get([<<"sender_email">>], Settings),
    Options = smtp_options(Settings),

    hoax:mock(deliv_change, [
              ?expect(getval,
                      ?withArgs([id, change]),
                      ?andReturn(ChangeId)),
              ?expect(scoping_names,
                      ?withArgs([ChangeId]),
                      ?andReturn([EntName, OrgName, ProjName]))]),
    hoax:mock(notification_subscriptions,
              ?expect(subscriber_emails_by_event,
                      ?withArgs([EntName, OrgName, ProjName, comment_created]),
                      ?andReturn([UserEmailAddress]))),
    hoax:mock(notification_smtp_content, [
              ?expect(comment_created,
                      ?withArgs([{change, comment}]),
                      ?andReturn(email_content)),
              ?expect(compose_email,
                      ?withArgs([Config, UserEmailAddress, email_content]),
                      ?andReturn(full_email))]),
    hoax:mock(mimemail,
              ?expect(encode,
                      ?withArgs([full_email]),
                      ?andReturn(sendable_email))),
    hoax:mock(gen_smtp_client,
              ?expect(send,
                      ?withArgs([{SenderEmail, [UserEmailAddress], sendable_email}, Options, fun notification_notifier_smtp:callback/1]))),

    Result = notification_notifier_smtp:notify(Config, comment_created, {change, comment}),
    ?verifyAll,
    ?assertEqual(ok, Result).

callback_does_nothing_on_send_success() ->
    notification_notifier_smtp:callback({ok, receipt}),
    ?verifyAll.

callback_logs_error_on_send_error() ->
    hoax:mock(chef_log,
              ?expect(error,
                      ?withArgs(["Failed to deliver notification via smtp due to: ~s.", [because]]))),
    notification_notifier_smtp:callback({exit, because}),
    ?verifyAll.

callback_logs_error_on_send_failure() ->
    hoax:mock(chef_log,
              ?expect(error,
                      ?withArgs(["Failed to deliver notification via smtp due to error: ~s ~p.", [error_type, error_message]]))),
    notification_notifier_smtp:callback({error, error_type, error_message}),
    ?verifyAll.
