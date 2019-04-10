-module(notification_smtp_test).

-include_lib("delivery/include/deliv_types.hrl").

-export([
         send/3
        ]).

-spec send(binary(), binary(), json()) -> {error, user_not_found | no_user_email} | {error, user_not_found, any()} | any().
send(EntName, UserName, IncomingSmtpConfig) ->
    case notification_config:with_merged_password(EntName, IncomingSmtpConfig) of
        {error, _Why} = Error ->
            Error;
        {ok, SmtpConfig} ->
            SenderName = ej:get([<<"sender_name">>], SmtpConfig),
            SenderEmail = ej:get([<<"sender_email">>], SmtpConfig),
            Options = notification_config:gen_smtp_options(SmtpConfig),

            %% Returns SMTP server's receipt or {error, Type, Msg} or {error, Why}.
            EmailSendFun =
                fun(ReceiverEmail) ->
                    Email = notification_smtp_content:test_email(ReceiverEmail, SenderName, SenderEmail, EntName),
                    gen_smtp_client:send_blocking({SenderEmail, [ReceiverEmail], Email}, Options)
                end,
            deliv_user:email_user(EntName, UserName, EmailSendFun)
    end.
