-module(notification_web_utils_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_types.hrl").

-compile([export_all]).

scoped_web_url_for_change_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "scoped_web_url_for_change_", setup, teardown).

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, "to_json_").

setup() ->
    EntName = <<"Ventech">>,
    OrgName = <<"ScienceNowLabs">>,
    ProjName = <<"O.S.SparkleDream">>,
    ChangeId = 256,
    [EntName, OrgName, ProjName, ChangeId].

teardown(_) ->
    ok.

scoped_web_url_for_change_links_to_review_tab_when_scope_is_review([EntName, OrgName, ProjName, ChangeId]) ->
    WebURL = <<"https://url.to.change">>,
    ExpectedScopedWebURL = <<"https://url.to.change/review">>,

    hoax:mock(deliv_project,
        [
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn({ok, project})),
              ?expect(scm_type,
                      ?withArgs([project]),
                      ?andReturn(<<"local">>))
        ]),

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId]),
                      ?andReturn(WebURL))),

    Result = notification_web_utils:scoped_web_url_for_change(EntName, OrgName, ProjName, ChangeId, review),

    ?assertEqual(ExpectedScopedWebURL, Result),
    ?verifyAll.

scoped_web_url_for_change_links_to_status_tab_when_scope_is_status([EntName, OrgName, ProjName, ChangeId]) ->
    WebURL = <<"https://url.to.change">>,
    ExpectedScopedWebURL = <<"https://url.to.change/status">>,

    hoax:mock(deliv_project,
        [
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn({ok, project})),
              ?expect(scm_type,
                      ?withArgs([project]),
                      ?andReturn(<<"local">>))
        ]),

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId]),
                      ?andReturn(WebURL))),

    Result = notification_web_utils:scoped_web_url_for_change(EntName, OrgName, ProjName, ChangeId, status),

    ?assertEqual(ExpectedScopedWebURL, Result),
    ?verifyAll.

scoped_web_url_for_change_links_to_summary_tab_when_scope_is_summary([EntName, OrgName, ProjName, ChangeId]) ->
    WebURL = <<"https://url.to.change">>,
    ExpectedScopedWebURL = <<"https://url.to.change/summary">>,

    hoax:mock(deliv_project,
        [
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn({ok, project})),
              ?expect(scm_type,
                      ?withArgs([project]),
                      ?andReturn(<<"local">>))
        ]),

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId]),
                      ?andReturn(WebURL))),

    Result = notification_web_utils:scoped_web_url_for_change(EntName, OrgName, ProjName, ChangeId, summary),

    ?assertEqual(ExpectedScopedWebURL, Result),
    ?verifyAll.

scoped_web_url_for_change_when_github_links_to_files_tab_of_pr_when_scope_is_review([EntName, OrgName, ProjName, ChangeId]) ->
    WebURL = <<"https://github.com/pr/1">>,
    ExpectedScopedWebURL = <<"https://github.com/pr/1/files">>,

    hoax:mock(deliv_project,
        [
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn({ok, project})),
              ?expect(scm_type,
                      ?withArgs([project]),
                      ?andReturn(<<"github">>))
        ]),

    hoax:mock(deliv_patchset,
              ?expect(latest_patchset_for_change,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, patchset}))),

    hoax:mock(deliv_github_patchset,
              ?expect(pr_url,
                      ?withArgs([patchset]),
                      ?andReturn(WebURL))),

    Result = notification_web_utils:scoped_web_url_for_change(EntName, OrgName, ProjName, ChangeId, review),

    ?assertEqual(ExpectedScopedWebURL, Result),
    ?verifyAll.

scoped_web_url_for_change_when_github_links_to_pr_when_scope_is_status([EntName, OrgName, ProjName, ChangeId]) ->
    WebURL = ExpectedScopedWebURL = <<"https://github.com/pr/1">>,

    hoax:mock(deliv_project,
        [
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn({ok, project})),
              ?expect(scm_type,
                      ?withArgs([project]),
                      ?andReturn(<<"github">>))
        ]),

    hoax:mock(deliv_patchset,
              ?expect(latest_patchset_for_change,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, patchset}))),

    hoax:mock(deliv_github_patchset,
              ?expect(pr_url,
                      ?withArgs([patchset]),
                      ?andReturn(WebURL))),

    Result = notification_web_utils:scoped_web_url_for_change(EntName, OrgName, ProjName, ChangeId, status),

    ?assertEqual(ExpectedScopedWebURL, Result),
    ?verifyAll.

scoped_web_url_for_change_when_github_links_to_pr_when_scope_is_summary([EntName, OrgName, ProjName, ChangeId]) ->
    WebURL = ExpectedScopedWebURL = <<"https://github.com/pr/1">>,

    hoax:mock(deliv_project,
        [
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn({ok, project})),
              ?expect(scm_type,
                      ?withArgs([project]),
                      ?andReturn(<<"github">>))
        ]),

    hoax:mock(deliv_patchset,
              ?expect(latest_patchset_for_change,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, patchset}))),

    hoax:mock(deliv_github_patchset,
              ?expect(pr_url,
                      ?withArgs([patchset]),
                      ?andReturn(WebURL))),

    Result = notification_web_utils:scoped_web_url_for_change(EntName, OrgName, ProjName, ChangeId, summary),

    ?assertEqual(ExpectedScopedWebURL, Result),
    ?verifyAll.

to_json_converts_slack_webhook_config_to_json() ->
    Name = <<"#rfr">>,
    URL = <<"https://my.slack.webhook/url">>,
    Enabled = true,

    Config = #notification_config{
        notification_type = slack_webhook,
        name = Name,
        settings = {[{<<"url">>, URL}]},
        enabled = Enabled},

    ExpectedJSON = {[
        {<<"webhook">>, {[
            {<<"url">>, URL},
            {<<"name">>, Name},
            {<<"enabled">>, Enabled}
        ]}}
    ]},

    JSON = notification_web_utils:to_json(Config),
    ?assertEqual(ExpectedJSON, JSON).

to_json_converts_smtp_config_to_json() ->
    Host = <<"it.happening.one.night">>,
    Port = 25,
    Login = <<"mangrenade">>,
    Password = <<"encryptme">>,
    Email = <<"hatred@venturetec.com">>,
    SenderName = <<"Sgt Hatred">>,

    Settings = {[
                 {<<"host">>, Host},
                 {<<"port">>, Port},
                 {<<"smtp_login">>, Login},
                 {<<"password">>, Password},
                 {<<"sender_email">>, Email},
                 {<<"sender_name">>, SenderName}
               ]},

    Config = #notification_config{
        notification_type = smtp,
        settings = Settings
    },

    ExpectedJSON = {[
                     {<<"host">>, Host},
                     {<<"port">>, Port},
                     {<<"smtp_login">>, Login},
                     {<<"sender_email">>, Email},
                     {<<"sender_name">>, SenderName}
                    ]},

    JSON = notification_web_utils:to_json(Config),
    ?assertEqual(ExpectedJSON, JSON).
