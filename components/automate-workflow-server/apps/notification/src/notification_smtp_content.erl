-module(notification_smtp_content).

-include("notification_types.hrl").

-export([
        acceptance_failed/1,
        acceptance_passed/1,
        change_approved/1,
        change_delivered/1,
        comment_created/1,
        compose_email/3,
        build_failed/1,
        delivered_failed/1,
        delivered_passed/1,
        rehearsal_failed/1,
        test_email/4,
        union_failed/1,
        verify_passed/1
    ]).

-record(email_metadata, {change                         :: d_change(),
                         comment = undefined            :: undefined | d_comment(),
                         subject                        :: string(),
                         content                        :: string(),
                         url_scope = status             :: url_scope(),
                         additional_field = undefined   :: undefined | atom()
                        }).

% This email is just for testing the SMTP configuration setup. No change information or mimetuples needed.
-spec test_email(binary(), binary(), binary(), binary()) -> list().
test_email(ReceiverEmail, SenderName, SenderEmail, EntName) ->
    Subject = "Subject: Test Message From Chef Automate\r\n",
    To = ["To: ", ReceiverEmail, "\r\n"],
    From = ["From: ", erlang:binary_to_list(handle_sender_format(SenderEmail, SenderName)), "\r\n\r\n"],
    Content = ["Congratulations! Your SMTP server has been successfully configured in Chef Automate. Users in the ",
               EntName, " enterprise can now enable email notifications for projects they have access to.\r\n\r\n",
               "To start receiving email notifications for a project, go to the project\'s main page, and click the \"Watch Project\" button."],

    chef_utils:iodata_to_str(lists:flatten([Subject, To, From, Content])).

-spec acceptance_passed(d_change()) -> {binary(), binary()}.
acceptance_passed(Change) ->
    notification_email(
        #email_metadata{change = Change,
                        subject = "Change ready to be Delivered",
                        content = "Acceptance stage has completed, a change is <strong>ready to be Delivered</strong>.",
                        url_scope = summary}
    ).

-spec change_approved(d_change()) -> {binary(), binary()}.
change_approved(Change) ->
    notification_email(
        #email_metadata{change = Change,
                        subject = "Change Approved",
                        content = "A change has been approved.",
                        additional_field = approved_by}
    ).

-spec change_delivered(d_change()) -> {binary(), binary()}.
change_delivered(Change) ->
    notification_email(
        #email_metadata{change = Change,
                        subject = "Change Delivered",
                        content = "A change has been delivered.",
                        additional_field = delivered_by}
    ).

-spec comment_created({d_change(), d_comment()}) -> {binary(), binary()}.
comment_created({Change, Comment}) ->
    notification_email(
        #email_metadata{change = Change,
                        comment = Comment,
                        subject = "New comment added",
                        content = "A new comment has been added to this change.",
                        url_scope = review}
    ).

-spec delivered_failed(d_change()) -> {binary(), binary()}.
delivered_failed(Change) ->
    notification_email(
        #email_metadata{change = Change,
                        subject = "Delivered Failure",
                        content = "Delivered stage has failed for this change.",
                        additional_field = undefined}
    ).

-spec delivered_passed(d_change()) -> {binary(), binary()}.
delivered_passed(Change) ->
    notification_email(
        #email_metadata{change = Change,
                        subject = "Delivered Complete!",
                        content = "Delivered stage is complete!",
                        additional_field = undefined}
    ).

-spec rehearsal_failed(d_change()) -> {binary(), binary()}.
rehearsal_failed(Change) ->
    notification_email(
        #email_metadata{change = Change,
                        subject = "Rehearsal Failure",
                        content = "Rehearsal stage has failed for this change."}
    ).

-spec union_failed(d_change()) -> {binary(), binary()}.
union_failed(Change) ->
    notification_email(
        #email_metadata{change = Change,
                        subject = "Union Failure",
                        content = "Union stage has failed for this change."}
    ).

-spec verify_passed(d_change()) -> {binary(), binary()}.
verify_passed(Change) ->
    notification_email(
        #email_metadata{change = Change,
                        subject = "Change ready to be Approved",
                        content = "Verify stage has completed, a change is <strong>ready to be Approved</strong>.",
                        url_scope = review}
    ).

-spec build_failed(d_change()) -> {binary(), binary()}.
build_failed(Change) ->
    notification_email(
        #email_metadata{change = Change,
                        subject = "Build Failure",
                        content = "Build stage has failed for this change."}
    ).

-spec acceptance_failed(d_change()) -> {binary(), binary()}.
acceptance_failed(Change) ->
    notification_email(
        #email_metadata{change = Change,
                        subject = "Acceptance Failure",
                        content = "Acceptance stage has failed for this change.",
                        url_scope = status}
    ).

% this composes the mimetuples that can be encoded by gen_smtp.
% The return ultimately is
% {Type, Subtype, Headers, ContentTypeParams, EmailContent}
compose_email(#notification_config{settings = Settings}, UserEmail, {Subject, EmailContent}) ->
    SenderEmail = handle_sender_format(ej:get([<<"sender_email">>], Settings),
                                       ej:get([<<"sender_name">>], Settings)),
    {
        <<"text">>,
        <<"html">>,
        [
          {<<"From">>, SenderEmail},
          {<<"To">>, UserEmail},
          {<<"Subject">>, Subject}
        ],
        [],
        EmailContent
    }.

%% Private

notification_email(#email_metadata{change = Change,
                                   comment = Comment,
                                   subject = Subject,
                                   content = Content,
                                   url_scope = URLScope,
                                   additional_field = AdditionalField}) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),

    %% Create the URL we link to
    ScopedURL = notification_web_utils:scoped_web_url_for_change(EntName, OrgName,
                                                                 ProjName, ChangeId,
                                                                 URLScope),

    %% Create the URL to manage watching projects. This will link to the delivery
    %% project page no matter the project's SCM type.
    ProjectChangeURL = deliv_web_utils:make_web_url_for_project(EntName, OrgName, ProjName),
    FormattedSubject = chef_utils:to_bin(string:concat(Subject, " / ~s / ~s"), [ProjName, ChangeTitle]),

    %% This is the second sentence.
    CommentInfo = add_comment_fields_if_defined(Comment),
    GenericEmailContent = " Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "~s" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s",
    EmailFormatter = string:concat(Content, GenericEmailContent),
    EmailContentFormatted = chef_utils:to_bin(EmailFormatter, [ScopedURL, ChangeTitle, CommentInfo, ProjName, SubmittedBy]),

    %% maybe add delivered by field? Append manage watch link on the end
    EmailContentAdditional = handle_additional_field(EmailContentFormatted, ProjectChangeURL, Change, AdditionalField),
    {FormattedSubject, EmailContentAdditional}.

handle_additional_field(EmailContent, ProjectChangeURL, Change, approved_by) ->
    ApprovedBy = deliv_change:getval(approved_by, Change),
    AdditionalContent = chef_utils:to_bin("<br /><strong>Approved by:</strong> ~s", [ApprovedBy]),
    add_manage_text(<<EmailContent/binary, AdditionalContent/binary>>, ProjectChangeURL);
handle_additional_field(EmailContent, ProjectChangeURL, Change, delivered_by) ->
    DeliveredBy = deliv_change:getval(delivered_by, Change),
    AdditionalContent = chef_utils:to_bin("<br /><strong>Delivered by:</strong> ~s", [DeliveredBy]),
    add_manage_text(<<EmailContent/binary, AdditionalContent/binary>>, ProjectChangeURL);
handle_additional_field(EmailContent, ProjectChangeURL, _Change, undefined) ->
    add_manage_text(EmailContent, ProjectChangeURL).

add_manage_text(EmailContent, ProjectChangeURL) ->
    ManageText = "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
    ManageFormatted = chef_utils:to_bin(ManageText, [ProjectChangeURL]),
    <<EmailContent/binary, ManageFormatted/binary>>.

handle_sender_format(SenderEmail, null) ->
    SenderEmail;
handle_sender_format(SenderEmail, SenderName) ->
    chef_utils:to_bin("~s <~s>", [SenderName, SenderEmail]).

add_comment_fields_if_defined(undefined) ->
    "";
add_comment_fields_if_defined(Comment) ->
    Content = erlang:binary_to_list(deliv_comment:getval(content, Comment)),
    CommenterId = deliv_comment:getval(submitter_id, Comment),
    case deliv_user:fetch(CommenterId) of
        {ok, Commenter} ->
            CommenterName = erlang:binary_to_list(deliv_user:getval(name, Commenter)),

            string:join(["<br /><br /><strong>Comment:</strong> ", Content, "<br /><strong>From:</strong> ", CommenterName], "");
        %% if we cannot find the commenter, we still would like the email to be sent
        {error, _Why} ->
            string:join(["<br /><br /><strong>Comment:</strong> ", Content], "")
    end.
