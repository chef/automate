-module(notification_smtp_content_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_types.hrl").

-compile(export_all).

parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, setup, teardown).

setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup(),

    %% Create a change and a notification config.
    eu_data:with_enterprise(<<"Ventech">>,
      eu_data:with_organization(<<"SuperScience">>,
        eu_data:with_project(<<"OoRay">>,
          eu_data:with_pipeline(<<"master">>, fun(Ent, Org, Proj, Pipe) ->
            %% Create a change
            User = eu_data:fetch_or_create_user(Ent, <<"tventure">>),
            Patchset = eu_data:create_patchset(Ent, User, Org, Proj,
                                               Pipe, <<"VT-01/ooo-ray">>),
            Change = eu_data:change_from_patchset(Patchset),
            Change2 = deliv_change:setvals([
                                              {title, <<"Implemented ooo sound">>},
                                              {approved_by, <<"Hank Venture">>},
                                              {delivered_by, <<"Brock Samson">>}
                                            ],
                                           Change),
            Change2
          end)))).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

test_email_creates_a_test_email_test() ->
    ReceiverEmail = <<"stcloud@nothingtoseehere.com">>,
    SenderName = <<"BillyQuizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    EntName = <<"Ventech">>,

    TestContent = "Subject: Test Message From Chef Automate\r\n" ++
                  "To: stcloud@nothingtoseehere.com\r\n" ++
                  "From: BillyQuizboy <BillyQuizboy@conjectural.tech>\r\n\r\n" ++
                  "Congratulations! Your SMTP server has been successfully " ++
                  "configured in Chef Automate. Users in the Ventech enterprise " ++
                  "can now enable email notifications for projects they have " ++
                  "access to.\r\n\r\n" ++
                  "To start receiving email notifications for a project, " ++
                  "go to the project's main page, and click the \"Watch Project\" button.",

    Result = notification_smtp_content:test_email(ReceiverEmail, SenderName, SenderEmail, EntName),

    ?assertEqual(TestContent, Result).

verify_passed_without_sender_name_creates_email(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderName = null,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/review">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = review,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
            ?expect(scoped_web_url_for_change,
                    ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                    ?andReturn(ScopedChangeURL))),

    Subject = chef_utils:to_bin("Change ready to be Approved / ~s / ~s", [ProjName, ChangeTitle]),

    ProjectPageURL = deliv_web_utils:make_web_url_for_project(EntName, OrgName, ProjName),

    EmailContent = chef_utils:to_bin("Verify stage has completed, " ++
        "a change is <strong>ready to be Approved</strong>. Follow the change " ++
        "link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, ProjName, SubmittedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderEmail},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:verify_passed(Change),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result).

verify_passed_with_sender_name_creates_email(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = review,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
          ?expect(scoped_web_url_for_change,
                  ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                  ?andReturn(ScopedChangeURL))),

    Subject = chef_utils:to_bin("Change ready to be Approved / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("Verify stage has completed, " ++
        "a change is <strong>ready to be Approved</strong>. " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, ProjName, SubmittedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:verify_passed(Change),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result).

change_delivered_creates_email(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    DeliveredBy = deliv_change:getval(delivered_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = status,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                      ?andReturn(ScopedChangeURL))),

    Subject = chef_utils:to_bin("Change Delivered / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("A change has been delivered. " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><strong>Delivered by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, ProjName, SubmittedBy, DeliveredBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:change_delivered(Change),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result).

union_failed_with_sender_name_creates_email(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = status,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
          ?expect(scoped_web_url_for_change,
                  ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                  ?andReturn(ScopedChangeURL))),

    Subject = chef_utils:to_bin("Union Failure / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("Union stage has failed for this change. " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, ProjName, SubmittedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:union_failed(Change),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result).

change_approved_creates_email(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    ApprovedBy = deliv_change:getval(approved_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = status,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
          ?expect(scoped_web_url_for_change,
                  ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                  ?andReturn(ScopedChangeURL))),

    Subject = chef_utils:to_bin("Change Approved / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("A change has been approved. " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><strong>Approved by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, ProjName, SubmittedBy, ApprovedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:change_approved(Change),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result).

acceptance_passed_creates_email(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = summary,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
          ?expect(scoped_web_url_for_change,
                  ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                  ?andReturn(ScopedChangeURL))),

    Subject = chef_utils:to_bin("Change ready to be Delivered / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("Acceptance stage has completed, a change is " ++
        "<strong>ready to be Delivered</strong>. " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, ProjName, SubmittedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:acceptance_passed(Change),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result).

acceptance_failed_creates_email(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = status,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
          ?expect(scoped_web_url_for_change,
                  ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                  ?andReturn(ScopedChangeURL))),

    Subject = chef_utils:to_bin("Acceptance Failure / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("Acceptance stage has failed for this change. " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, ProjName, SubmittedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:acceptance_failed(Change),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result).

delivered_failed_creates_email(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = status,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
          ?expect(scoped_web_url_for_change,
                  ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                  ?andReturn(ScopedChangeURL))),

    Subject = chef_utils:to_bin("Delivered Failure / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("Delivered stage has failed for this change. " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, ProjName, SubmittedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:delivered_failed(Change),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result).

delivered_passed_creates_email(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = status,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
          ?expect(scoped_web_url_for_change,
                  ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                  ?andReturn(ScopedChangeURL))),

    Subject = chef_utils:to_bin("Delivered Complete! / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("Delivered stage is complete! " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, ProjName, SubmittedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:delivered_passed(Change),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result).

rehearsal_failed_creates_email(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = status,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
          ?expect(scoped_web_url_for_change,
                  ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                  ?andReturn(ScopedChangeURL))),

    Subject = chef_utils:to_bin("Rehearsal Failure / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("Rehearsal stage has failed for this change. " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, ProjName, SubmittedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:rehearsal_failed(Change),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result).

comment_created_creates_email(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},
    CommentContent = <<"We should be using chef_log:failed_call for this error">>,
    CommenterId = 2,
    CommenterName = <<"Dean Venture">>,
    Comment = deliv_comment:fromlist([{content, CommentContent}, {submitter_id, CommenterId}]),

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = review,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                      ?andReturn(ScopedChangeURL))),
    hoax:mock(deliv_user, [
              ?expect(fetch,
                      ?withArgs([CommenterId]),
                      ?andReturn({ok, commenter})),
              ?expect(getval,
                      ?withArgs([name, commenter]),
                      ?andReturn(CommenterName))]),

    Subject = chef_utils:to_bin("New comment added / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("A new comment has been added to this change. " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Comment:</strong> ~s" ++
        "<br /><strong>From:</strong> ~s" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, CommentContent, CommenterName, ProjName, SubmittedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:comment_created({Change, Comment}),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result),
    ?verifyAll.

comment_created_creates_email_even_if_commenter_is_not_found(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},
    CommentContent = <<"We should be using chef_log:failed_call for this error">>,
    CommenterId = 2,
    Comment = deliv_comment:fromlist([{content, CommentContent}, {submitter_id, CommenterId}]),

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = review,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                      ?andReturn(ScopedChangeURL))),
    hoax:mock(deliv_user,
              ?expect(fetch,
                      ?withArgs([CommenterId]),
                      ?andReturn({error, why}))),

    Subject = chef_utils:to_bin("New comment added / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("A new comment has been added to this change. " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Comment:</strong> ~s" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, CommentContent, ProjName, SubmittedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:comment_created({Change, Comment}),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result),
    ?verifyAll.

build_failed_returns_error_no_content(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    SenderName = <<"Billy Quizboy">>,
    SenderEmail = <<"BillyQuizboy@conjectural.tech">>,
    SenderFormat = <<"Billy Quizboy <BillyQuizboy@conjectural.tech>">>,
    UserEmail = <<"stcloud@nothingtoseehere.com">>,
    Settings = {[
                  {<<"sender_email">>, SenderEmail},
                  {<<"sender_name">>, SenderName}
               ]},
    Config = #notification_config{settings = Settings},

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ScopedChangeURL = <<"http://my.change.url/change">>,
    ProjectPageURL = <<"http://my.project.com/changes">>,
    URLScope = status,

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_project,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ProjectPageURL))),
    hoax:mock(notification_web_utils,
          ?expect(scoped_web_url_for_change,
                  ?withArgs([EntName, OrgName, ProjName, ChangeId, URLScope]),
                  ?andReturn(ScopedChangeURL))),

    Subject = chef_utils:to_bin("Build Failure / ~s / ~s", [ProjName, ChangeTitle]),

    EmailContent = chef_utils:to_bin("Build stage has failed for this change. " ++
        "Follow the change link for more details:" ++
        "<br /><br /><a href=\"~s\">~s</a>" ++
        "<br /><br /><strong>Project:</strong> ~s" ++
        "<br /><strong>Submitted by:</strong> ~s" ++
        "<br /><br /><a href=\"~s\">Manage watch settings for this project</a>",
        [ScopedChangeURL, ChangeTitle, ProjName, SubmittedBy, ProjectPageURL]),

    Email = {<<"text">>, <<"html">>, [
                        {<<"From">>, SenderFormat},
                        {<<"To">>, UserEmail},
                        {<<"Subject">>, Subject}],
                    [],
                    EmailContent},

    SubjectAndBody = notification_smtp_content:build_failed(Change),
    Result = notification_smtp_content:compose_email(Config, UserEmail, SubjectAndBody),
    ?assertEqual(Email, Result),
    ?verifyAll.
