-module(notification_slack_content_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_macros.hrl").
-include_lib("delivery/include/deliv_types.hrl").

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

verify_passed_creates_json_for_verify_passed_notification(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ReviewChangeURL = <<"http://my.change.url/review">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, review]),
                      ?andReturn(ReviewChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nVerify Passed. Change is ready for review.", [ReviewChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nVerify Passed. Change is ready for review.", [ReviewChangeURL, ChangeTitle])},
                {<<"color">>, <<"#58B957">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:verify_passed(Change)),
    ?verifyAll.

verify_passed_for_github_integrated_project_links_to_github_pr_files(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    ReviewChangeURL = <<"https://github.com/chef/ohai/pull/513/files">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, review]),
                      ?andReturn(ReviewChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nVerify Passed. Change is ready for review.", [ReviewChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nVerify Passed. Change is ready for review.", [ReviewChangeURL, ChangeTitle])},
                {<<"color">>, <<"#58B957">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:verify_passed(Change)),
    ?verifyAll.

change_approved_creates_json_for_change_approved_notification(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    ApprovedBy = deliv_change:getval(approved_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    ExpectedChangeURL = <<"http://my.change.url/status">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, status]),
                      ?andReturn(ExpectedChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nChange Approved!", [ExpectedChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nChange Approved!", [ExpectedChangeURL, ChangeTitle])},
                {<<"color">>, <<"#E3E4E6">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]},
                    {[
                        {<<"title">>, <<"Approved by:">>},
                        {<<"value">>, ApprovedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:change_approved(Change)),
    ?verifyAll.

change_approved_for_github_integrated_project_links_to_github_pr(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    ApprovedBy = deliv_change:getval(approved_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    PullRequestURL = <<"https://github.com/chef/ohai/pull/513">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, status]),
                      ?andReturn(PullRequestURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nChange Approved!", [PullRequestURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nChange Approved!", [PullRequestURL, ChangeTitle])},
                {<<"color">>, <<"#E3E4E6">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]},
                    {[
                        {<<"title">>, <<"Approved by:">>},
                        {<<"value">>, ApprovedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:change_approved(Change)),
    ?verifyAll.

acceptance_passed_creates_json_for_acceptance_passed_notification(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    SummaryChangeURL = <<"http://my.change.url/summary">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, summary]),
                      ?andReturn(SummaryChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nAcceptance Passed. Change is ready for delivery.", [SummaryChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nAcceptance Passed. Change is ready for delivery.", [SummaryChangeURL, ChangeTitle])},
                {<<"color">>, <<"#58B957">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:acceptance_passed(Change)),
    ?verifyAll.

acceptance_passed_for_github_integrated_project_links_to_github_pr(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    PullRequestURL = <<"https://github.com/chef/ohai/pull/513">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, summary]),
                      ?andReturn(PullRequestURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nAcceptance Passed. Change is ready for delivery.", [PullRequestURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nAcceptance Passed. Change is ready for delivery.", [PullRequestURL, ChangeTitle])},
                {<<"color">>, <<"#58B957">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:acceptance_passed(Change)),
    ?verifyAll.

acceptance_failed_creates_json_for_acceptance_failed_notification(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    SummaryChangeURL = <<"http://my.change.url/summary">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, status]),
                      ?andReturn(SummaryChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nChange failed at the Acceptance stage.", [SummaryChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nChange failed at the Acceptance stage.", [SummaryChangeURL, ChangeTitle])},
                {<<"color">>, <<"#D00000">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:acceptance_failed(Change)),
    ?verifyAll.

change_delivered_creates_json_for_change_delivered_notification(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    DeliveredBy = deliv_change:getval(delivered_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    SummaryChangeURL = <<"http://my.change.url/status">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, status]),
                      ?andReturn(SummaryChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nChange Delivered!", [SummaryChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nChange Delivered!", [SummaryChangeURL, ChangeTitle])},
                {<<"color">>, <<"#E3E4E6">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]},
                    {[
                        {<<"title">>, <<"Delivered by:">>},
                        {<<"value">>, DeliveredBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:change_delivered(Change)),
    ?verifyAll.

union_failure_creates_json_for_union_failure_notification(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    ExpectedChangeURL = <<"http://my.change.url/status">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, status]),
                      ?andReturn(ExpectedChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nChange failed at the Union stage.", [ExpectedChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nChange failed at the Union stage.", [ExpectedChangeURL, ChangeTitle])},
                {<<"color">>, <<"#D00000">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:union_failed(Change)),
    ?verifyAll.

rehearsal_failure_creates_json_for_rehearsal_failure_notification(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    ExpectedChangeURL = <<"http://my.change.url/status">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, status]),
                      ?andReturn(ExpectedChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nChange failed at the Rehearsal stage.", [ExpectedChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nChange failed at the Rehearsal stage.", [ExpectedChangeURL, ChangeTitle])},
                {<<"color">>, <<"#D00000">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:rehearsal_failed(Change)),
    ?verifyAll.

delivered_passed_creates_json_for_delivered_passed_notification(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    ExpectedChangeURL = <<"http://my.change.url/status">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, status]),
                      ?andReturn(ExpectedChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nDelivered stage has completed for this change.", [ExpectedChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nDelivered stage has completed for this change.", [ExpectedChangeURL, ChangeTitle])},
                {<<"color">>, <<"#58B957">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:delivered_passed(Change)),
    ?verifyAll.

delivered_failed_creates_json_for_delivered_failed_notification(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    ExpectedChangeURL = <<"http://my.change.url/status">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, status]),
                      ?andReturn(ExpectedChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nChange failed at the Delivered stage.", [ExpectedChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nChange failed at the Delivered stage.", [ExpectedChangeURL, ChangeTitle])},
                {<<"color">>, <<"#D00000">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:delivered_failed(Change)),
    ?verifyAll.

verify_passed_encodes_json_for_special_chars(Change) ->
    Change2 = deliv_change:setvals([{title, <<"Implemented <ooo> sound & waveform">>},
                                    {approved_by, <<"Hank Venture">>},
                                    {delivered_by, <<"Brock Samson">>}
                                   ], Change),

    ChangeId = deliv_change:getval(id, Change2),
    ChangeTitleEncoded = <<"Implemented &lt;ooo&gt; sound &amp; waveform">>,
    SubmittedBy = deliv_change:getval(submitted_by, Change2),

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ReviewChangeURL = <<"http://my.change.url/review">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, review]),
                      ?andReturn(ReviewChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nVerify Passed. Change is ready for review.", [ReviewChangeURL, ChangeTitleEncoded])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nVerify Passed. Change is ready for review.", [ReviewChangeURL, ChangeTitleEncoded])},
                {<<"color">>, <<"#58B957">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:verify_passed(Change2)),
    ?verifyAll.

verify_passed_encodes_json_for_newline_chars_in_middle(Change) ->
     Change2 = deliv_change:setvals([{title, <<"Implemented\nnewline">>},
                                     {approved_by, <<"Hank Venture">>},
                                     {delivered_by, <<"Brock Samson">>}
                                    ], Change),

    ChangeId = deliv_change:getval(id, Change2),
    ChangeTitleEncoded = <<"Implemented newline">>,
    SubmittedBy = deliv_change:getval(submitted_by, Change2),

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ReviewChangeURL = <<"http://my.change.url/review">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, review]),
                      ?andReturn(ReviewChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nVerify Passed. Change is ready for review.", [ReviewChangeURL, ChangeTitleEncoded])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nVerify Passed. Change is ready for review.", [ReviewChangeURL, ChangeTitleEncoded])},
                {<<"color">>, <<"#58B957">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:verify_passed(Change2)),
    ?verifyAll.

verify_passed_encodes_json_for_empty_title(Change) ->
     Change2 = deliv_change:setvals([{title, <<>>},
                                     {approved_by, <<"Hank Venture">>},
                                     {delivered_by, <<"Brock Samson">>}
                                    ], Change),

    ChangeId = deliv_change:getval(id, Change2),
    ChangeTitle = <<"Untitled Change">>,
    SubmittedBy = deliv_change:getval(submitted_by, Change2),

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ReviewChangeURL = <<"http://my.change.url/review">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, review]),
                      ?andReturn(ReviewChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nVerify Passed. Change is ready for review.", [ReviewChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nVerify Passed. Change is ready for review.", [ReviewChangeURL, ChangeTitle])},
                {<<"color">>, <<"#58B957">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:verify_passed(Change2)),
    ?verifyAll.

comment_created_creates_json_for_comment_created_notification(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    CommenterName = <<"Dean Venture">>,
    Content = <<"I dunno about this stuff, guys">>,
    CommenterId = 1,
    Comment = deliv_comment:fromlist([{content, Content},{submitter_id, CommenterId}]),
    CommenterEmail = <<"dean@ventech.com">>,
    ThumbUrl = <<"https://s.gravatar.com/avatar/74a79a82f3752c57d719453d0d636cb6">>,

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ReviewChangeURL = <<"http://my.change.url/review">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, review]),
                      ?andReturn(ReviewChangeURL))),
    hoax:mock(deliv_comment, [
              ?expect(getval,
                      ?withArgs([content, Comment]),
                      ?andReturn(Content)),
              ?expect(getval,
                      ?withArgs([submitter_id, Comment]),
                      ?andReturn(CommenterId))]),
    hoax:mock(deliv_user, [
              ?expect(fetch,
                      ?withArgs([CommenterId]),
                      ?andReturn({ok, user})),
              ?expect(getval,
                      ?withArgs([name, user]),
                      ?andReturn(CommenterName)),
              ?expect(getval,
                      ?withArgs([email, user]),
                      ?andReturn(CommenterEmail))]),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>", [ReviewChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>", [ReviewChangeURL, ChangeTitle])},
                {<<"color">>, <<"#E3E4E6">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"New comment:">>},
                        {<<"value">>, Content}
                    ]},
                    {[
                        {<<"title">>, <<"Comment from:">>},
                        {<<"value">>, CommenterName}
                    ]},
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]},
                {<<"thumb_url">>, ThumbUrl},
                {<<"mrkdwn_in">>, [<<"fields">>]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:comment_created({Change, Comment})),
    ?verifyAll.

comment_created_forwards_error_if_no_commenter_exists(Change) ->
    Content = <<"I dunno about this stuff, guys">>,
    CommenterId = 1,
    Comment = deliv_comment:fromlist([{content, Content},{submitter_id, CommenterId}]),

    hoax:mock(deliv_user,
              ?expect(fetch,
                      ?withArgs([CommenterId]),
                      ?andReturn({error, why}))),
    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([deliv_user, fetch, [CommenterId], why]))),

    ?assertEqual({error, why}, notification_slack_content:comment_created({Change, Comment})),
    ?verifyAll.

comment_created_does_not_add_avatar_when_email_is_undefined(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    CommenterName = <<"Dean Venture">>,
    Content = <<"I dunno about this stuff, guys">>,
    CommenterId = 1,
    Comment = deliv_comment:fromlist([{content, Content},{submitter_id, CommenterId}]),

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ReviewChangeURL = <<"http://my.change.url/review">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, review]),
                      ?andReturn(ReviewChangeURL))),
    hoax:mock(deliv_user, [
              ?expect(fetch,
                      ?withArgs([CommenterId]),
                      ?andReturn({ok, user})),
              ?expect(getval,
                      ?withArgs([name, user]),
                      ?andReturn(CommenterName)),
              ?expect(getval,
                      ?withArgs([email, user]),
                      ?andReturn(undefined))]),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>", [ReviewChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>", [ReviewChangeURL, ChangeTitle])},
                {<<"color">>, <<"#E3E4E6">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"New comment:">>},
                        {<<"value">>, Content}
                    ]},
                    {[
                        {<<"title">>, <<"Comment from:">>},
                        {<<"value">>, CommenterName}
                    ]},
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]},
                {<<"mrkdwn_in">>, [<<"fields">>]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:comment_created({Change, Comment})),
    ?verifyAll.

comment_created_does_not_add_avatar_when_email_is_empty(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),
    CommenterName = <<"Dean Venture">>,
    Content = <<"I dunno about this stuff, guys">>,
    CommenterId = 1,
    Comment = deliv_comment:fromlist([{content, Content},{submitter_id, CommenterId}]),

    [EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    ReviewChangeURL = <<"http://my.change.url/review">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, review]),
                      ?andReturn(ReviewChangeURL))),
    hoax:mock(deliv_user, [
              ?expect(fetch,
                      ?withArgs([CommenterId]),
                      ?andReturn({ok, user})),
              ?expect(getval,
                      ?withArgs([name, user]),
                      ?andReturn(CommenterName)),
              ?expect(getval,
                      ?withArgs([email, user]),
                      ?andReturn(<<>>))]),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>", [ReviewChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>", [ReviewChangeURL, ChangeTitle])},
                {<<"color">>, <<"#E3E4E6">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"New comment:">>},
                        {<<"value">>, Content}
                    ]},
                    {[
                        {<<"title">>, <<"Comment from:">>},
                        {<<"value">>, CommenterName}
                    ]},
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]},
                {<<"mrkdwn_in">>, [<<"fields">>]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:comment_created({Change, Comment})),
    ?verifyAll.

build_failed_creates_json_for_build_failed_notification(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = deliv_change:getval(title, Change),
    SubmittedBy = deliv_change:getval(submitted_by, Change),

    [EntName, OrgName, ProjName | _PipeName] = deliv_change:scoping_names(ChangeId),
    ExpectedChangeURL = <<"http://my.change.url/status">>,
    hoax:mock(notification_web_utils,
              ?expect(scoped_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId, status]),
                      ?andReturn(ExpectedChangeURL))),

    ExpectedJson = {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, chef_utils:to_bin("<~s|~s>\nChange failed at the Build stage.", [ExpectedChangeURL, ChangeTitle])},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, chef_utils:to_bin("<~s|~s>\nChange failed at the Build stage.", [ExpectedChangeURL, ChangeTitle])},
                {<<"color">>, <<"#D00000">>},
                {<<"fields">>, [
                    {[
                        {<<"title">>, <<"Project:">>},
                        {<<"value">>, ProjName}
                    ]},
                    {[
                        {<<"title">>, <<"Change submitted by:">>},
                        {<<"value">>, SubmittedBy},
                        {<<"short">>, true}
                    ]}
                ]}
            ]}
        ]}
    ]},

    ?assertEqual(ExpectedJson, notification_slack_content:build_failed(Change)),
    ?verifyAll.
