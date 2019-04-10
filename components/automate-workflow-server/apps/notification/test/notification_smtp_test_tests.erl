-module(notification_smtp_test_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

send_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "send_", setup, teardown).

setup() ->
    error_logger:tty(false),
    eu_database:setup(),

    EntName = <<"HankCo">>,
    eu_data:with_enterprise(EntName, fun(Enterprise) ->
        %% smtp config setup
        Host = <<"smtp.mailgun.org">>,
        Port = 25,
        SmtpLogin = <<"postmaster@sandbox.mailgun.org">>,
        Password = <<"P@ssword1">>,
        SenderEmail = <<"delivery-notification@chef.io">>,
        SenderName = <<"Hank Venture">>,

        ConfigValues = {Host, Port, SmtpLogin, Password, SenderEmail, SenderName},

        %% user setup
        %%
        %% eu_data:fetch_or_create_user creates a user with an email address.
        %% in case this is ever not the case, we'll set an email for this user
        %% (it's a pretty cool email, too).
        UserName = <<"hankventure">>,
        UserEmail = <<"batman@hank.co">>,
        User = eu_data:fetch_or_create_user(Enterprise, UserName),
        User2 = deliv_user:setvals([{email, UserEmail}], User),
        deliv_user:update(User2),

        [EntName, UserName, UserEmail, ConfigValues]
    end).

teardown(_) ->
    eu_database:teardown(),
    error_logger:tty(true).

%% Creates SMTP configuration JSON, without optional parameters set.
config_json(Host, Port, SmtpLogin, Password, SenderEmail, SenderName) ->
    {[
        {<<"host">>, Host},
        {<<"port">>, Port},
        {<<"smtp_login">>, SmtpLogin},
        {<<"password">>, Password},
        {<<"sender_email">>, SenderEmail},
        {<<"sender_name">>, SenderName}
    ]}.

config_json(Host, Port, SmtpLogin, SenderEmail, SenderName) ->
    {[
        {<<"host">>, Host},
        {<<"port">>, Port},
        {<<"smtp_login">>, SmtpLogin},
        {<<"sender_email">>, SenderEmail},
        {<<"sender_name">>, SenderName}
    ]}.

%% The body of a test notification email.
expected_content(EntName) ->
    ["Congratulations! Your SMTP server has been successfully configured in Chef Automate. Users in the ",
     EntName, " enterprise can now enable email notifications for projects they have access to.\r\n\r\n",
     "To start receiving email notifications for a project, go to the project\'s main page, and click the \"Watch Project\" button."].

send_sets_from_to_sender_email_when_sender_name_not_provided([EntName, UserName,
        UserEmail, {Host, Port, SmtpLogin, Password, SenderEmail, _SenderName}]) ->

    MergedSettings = config_json(Host, Port, SmtpLogin, Password, SenderEmail, null),

    ExpectedEmailParts = ["Subject: Test Message From Chef Automate\r\n",
                          "To: ", UserEmail, "\r\n",
                          "From: ", SenderEmail, "\r\n\r\n",
                          expected_content(EntName)],

    ExpectedEmail = chef_utils:iodata_to_str(lists:flatten(ExpectedEmailParts)),
    ExpectedOptions = [{relay, Host},
                       {username, SmtpLogin},
                       {password, Password},
                       {port, Port}],
    hoax:expect(receive
                    notification_config:with_merged_password(EntName, settings) -> {ok, MergedSettings};
                    notification_config:gen_smtp_options(MergedSettings) -> ExpectedOptions;
                    gen_smtp_client:send_blocking({SenderEmail, [UserEmail], ExpectedEmail}, ExpectedOptions) -> <<"Great success\r\n">>
                end),

    Result = notification_smtp_test:send(EntName, UserName, settings),
    ?assertEqual(<<"Great success\r\n">>, Result),
    ?verifyAll.

send_sets_from_to_sender_name_and_email_when_sender_name_provided([EntName, UserName,
        UserEmail, {Host, Port, SmtpLogin, Password, SenderEmail, SenderName}]) ->

    MergedSettings = config_json(Host, Port, SmtpLogin, Password, SenderEmail, SenderName),

    ExpectedEmailParts = ["Subject: Test Message From Chef Automate\r\n",
                          "To: ", UserEmail, "\r\n",
                          "From: ", SenderName, " <", SenderEmail, ">\r\n\r\n",
                          expected_content(EntName)],

    ExpectedEmail = chef_utils:iodata_to_str(lists:flatten(ExpectedEmailParts)),
    ExpectedOptions = [{relay, Host},
                       {username, SmtpLogin},
                       {password, Password},
                       {port, Port}],

    hoax:expect(receive
                    notification_config:with_merged_password(EntName, settings) -> {ok, MergedSettings};
                    notification_config:gen_smtp_options(MergedSettings) -> ExpectedOptions;
                    gen_smtp_client:send_blocking({SenderEmail, [UserEmail], ExpectedEmail}, ExpectedOptions) -> {error, unknown}
                end),

    Result = notification_smtp_test:send(EntName, UserName, settings),
    ?assertEqual({error, unknown}, Result),
    ?verifyAll.

send_when_with_merged_password_returns_settings_uses_these_settings([EntName, UserName,
        UserEmail, {Host, Port, SmtpLogin, _Password, SenderEmail, SenderName}]) ->
    MergedSettings = config_json(Host, Port, SmtpLogin, SenderEmail, SenderName),
    Password = base64:encode(<<"savedpassword">>),
    ExpectedEmailParts = ["Subject: Test Message From Chef Automate\r\n",
                          "To: ", UserEmail, "\r\n",
                          "From: ", SenderName, " <", SenderEmail, ">\r\n\r\n",
                          expected_content(EntName)],
    ExpectedEmail = chef_utils:iodata_to_str(lists:flatten(ExpectedEmailParts)),
    ExpectedOptions = [{relay, Host},
                       {username, SmtpLogin},
                       {password, base64:decode(Password)},
                       {port, Port}],
    hoax:expect(receive
                    notification_config:with_merged_password(EntName, settings) -> {ok, MergedSettings};
                    notification_config:gen_smtp_options(MergedSettings) -> ExpectedOptions;
                    gen_smtp_client:send_blocking({SenderEmail, [UserEmail], ExpectedEmail}, ExpectedOptions) -> <<"Great success\r\n">>
                end),

    Result = notification_smtp_test:send(EntName, UserName, settings),
    ?assertEqual(<<"Great success\r\n">>, Result),
    ?verifyAll.

send_forwards_error_if_with_merged_password_returns_error([EntName, UserName,
        _UserEmail, _]) ->
    hoax:expect(receive
                    notification_config:with_merged_password(EntName, settings) -> {error, whyyyyyy}
                end),

    Result = notification_smtp_test:send(EntName, UserName, settings),
    ?assertEqual({error, whyyyyyy}, Result),
    ?verifyAll.
