-module(deliv_github_chatops_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

-define(CHANGE_ID, <<"mocked_change_id">>).
-define(ENT_NAME, <<"mocked_ent_name">>).
-define(ORG_NAME, <<"mocked_org_name">>).
-define(PROJ_NAME, <<"mocked_proj_name">>).
-define(DELIVERY_USER_NAME, <<"mocked_delivery_user_name">>).

hoax_fixture_test_ () ->
    hoax:fixture(?MODULE).

handle_does_nothing_if_delivery_not_mentioned() ->
    ok = deliv_github_chatops:handle_comment_created({{<<"ent">>, <<"changeid">>}, 1,
                                                      <<"Looks good!">>, <<"mocked_author">>}).

handle_comment_review_calls_trigger_stage_and_removes_label() ->
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = 1,
    Comment = <<"Change: ", ?CHANGE_ID/binary, " reviewed by: @", Author/binary>>,

    hoax:mock(deliv_change,
              ?expect(trigger_stage,
                      ?withArgs([verify, ?CHANGE_ID]),
                      ?andReturn(ok))),
    hoax:mock(deliv_github_api,
              [?expect(create_issue_comment,
                       ?withArgs([Scope, IssueId, Comment]),
                       ?andReturn({ok, ignored})),
               ?expect(delete_label_from_issue,
                       ?withArgs([Scope, IssueId, <<"Review Required">>]),
                       ?andReturn(ok))]),
    mock_fetch_by_alias_name(Author, Approver),
    mock_get_user_name(Approver),
    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, review, allow),
    ?assertMatch({ok, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery review this!">>, Author})),
    ?verifyAll.

handle_comment_review_returns_error_when_no_permissions() ->
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = 1,
    Message = ["Github user ", Author, " does not have permissions to execute action ", <<"review">>, "."],
    mock_fetch_by_alias_name(Author, Approver),
    mock_get_user_name(Approver),
    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, review, forbid),

    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, iolist_to_binary(Message)]),
                      ?andReturn({ok, ignored}))),

    ?assertMatch({error, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery review this!">>, Author})),
    ?verifyAll.

handle_comment_review_does_not_trigger_with_invalid_user() ->
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = 1,
    Message = <<"Oauth setup is incorrect.  No link found for this repo">>,

    mock_fetch_by_alias_name(Author, Approver, {error, oauth_not_found}),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, Message]),
                      ?andReturn({ok, ignored}))),
    ?assertMatch({error, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery review this!">>, Author})),
    ?verifyAll.

handle_comment_review_gives_error_message_with_invalid_state() ->
    ChangeId = ?CHANGE_ID,
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = 1,
    Message = <<"Failed to review change: ", ChangeId/binary, "\n",
                "  Change must not have been reviewed or be in verify to be",
                " reviewed. @", Author/binary>>,

    hoax:mock(deliv_change,
              [?expect(trigger_stage,
                       ?withArgs([verify, ChangeId]),
                       ?andReturn({error, invalid_state}))]),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, iolist_to_binary(Message)]),
                      ?andReturn({ok, ignored}))),
    mock_fetch_by_alias_name(Author, Approver),
    mock_get_user_name(Approver),
    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, review, allow),
    ?assertMatch({ok, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery review this!">>, Author})),
    ?verifyAll.

handle_comment_approve_calls_merge() ->
    ChangeId = ?CHANGE_ID,
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = 1,
    Comment = <<"Change: ", ChangeId/binary, " approved by: @", Author/binary>>,

    hoax:mock(deliv_change,
              ?expect(merge,
                      ?withArgs([ChangeId, Approver]),
                      ?andReturn(ok))),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, Comment]),
                      ?andReturn({ok, ignored}))),
    mock_fetch_by_alias_name(Author, Approver),
    mock_get_user_name(Approver),
    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, approve, allow),
    ?assertMatch({ok, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery approve this!">>, Author})),
    ?verifyAll.

handle_comment_approve_does_not_merge_with_invalid_user() ->
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = 1,
    Message = ["Delivery user ", Author, " not found.  Please contact your delivery",
               " administrator to create an account with that name, or approve the",
               " commit as a different user"],

    mock_fetch_by_alias_name(Author, Approver, {error, user_not_found}),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, iolist_to_binary(Message)]),
                      ?andReturn({ok, ignored}))),
    ?assertMatch({error, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery approve this!">>, Author})),
    ?verifyAll.

handle_comment_approve_gives_error_message_if_not_verified() ->
    ChangeId = <<"mocked_change_id">>,
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = 1,
    Message = <<"Failed to approve change: ", ChangeId/binary, "\n",
                "  Change must pass verify before approving. @", Author/binary>>,

    hoax:mock(deliv_scopes,
              [?expect('#get',
                       ?withArgs([change_id, Scope]),
                       ?andReturn(ChangeId)),
               ?expect('#get',
                       ?withArgs([ent_name, Scope]),
                       ?andReturn(?ENT_NAME))]),
    hoax:mock(deliv_change,
              [?expect(merge,
                       ?withArgs([ChangeId, Approver]),
                       ?andReturn({error, invalid_state}))]),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, iolist_to_binary(Message)]),
                      ?andReturn({ok, ignored}))),
    mock_fetch_by_alias_name(Author, Approver),
    mock_get_user_name(Approver),
    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, approve, allow),
    ?assertMatch({ok, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery approve this!">>, Author})),
    ?verifyAll.

handle_comment_approve_gives_error_message_if_already_merged() ->
    ChangeId = ?CHANGE_ID,
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = 1,
    Message = <<"Failed to approve change: ", ChangeId/binary, "\n",
                "  Change is already merged. @", Author/binary>>,

    hoax:mock(deliv_change,
              [?expect(merge,
                       ?withArgs([ChangeId, Approver]),
                       ?andReturn({error, patchset_already_merged}))]),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, iolist_to_binary(Message)]),
                      ?andReturn({ok, ignored}))),
    mock_fetch_by_alias_name(Author, Approver),
    mock_get_user_name(Approver),
    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, approve, allow),
    ?assertMatch({ok, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery approve this!">>, Author})),
    ?verifyAll.

handle_comment_deliver_calls_accept() ->
    ChangeId = ?CHANGE_ID,
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = 1,
    Comment = <<"Change: ", ChangeId/binary, " delivered by: @", Author/binary>>,

    hoax:mock(deliv_change,
              ?expect(accept,
                      ?withArgs([ChangeId, Approver]),
                      ?andReturn(ok))),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, Comment]),
                      ?andReturn({ok, ignored}))),
    mock_fetch_by_alias_name(Author, Approver),
    mock_get_user_name(Approver),
    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, deliver, allow),
    ?assertMatch({ok, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery deliver this!">>, Author})),
    ?verifyAll.

handle_comment_deliver_does_not_accept_with_invalid_user() ->
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = 1,
    Message = ["Delivery user ", Author, " not found.  Please contact your delivery",
               " administrator to create an account with that name, or approve the",
               " commit as a different user"],

    mock_fetch_by_alias_name(Author, Approver, {error, user_not_found}),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, iolist_to_binary(Message)]),
                      ?andReturn({ok, ignored}))),
    ?assertMatch({error, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery deliver this!">>, Author})),
    ?verifyAll.

handle_comment_deliver_gives_error_message_if_not_accepted() ->
    ChangeId = <<"mocked_change_id">>,
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = 1,
    Message = <<"Failed to deliver change: ", ChangeId/binary, "\n",
                "  Change must pass acceptance before delivering. @", Author/binary>>,

    hoax:mock(deliv_scopes,
              [?expect('#get',
                       ?withArgs([change_id, Scope]),
                       ?andReturn(ChangeId)),
               ?expect('#get',
                       ?withArgs([ent_name, Scope]),
                       ?andReturn(?ENT_NAME))]),
    hoax:mock(deliv_change,
              [?expect(accept,
                       ?withArgs([ChangeId, Approver]),
                       ?andReturn({error, invalid_state}))]),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, iolist_to_binary(Message)]),
                      ?andReturn({ok, ignored}))),
    mock_fetch_by_alias_name(Author, Approver),
    mock_get_user_name(Approver),
    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, deliver, allow),

    ?assertMatch({ok, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery deliver this!">>, Author})),
    ?verifyAll.

handle_comment_deliver_gives_error_message_if_already_merged() ->
    ChangeId = ?CHANGE_ID,
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    IssueId = <<1>>,
    Message = <<"Failed to deliver change: ", ChangeId/binary, "\n",
                "  Change is already delivered. @", Author/binary>>,

    hoax:mock(deliv_scopes,
              [?expect('#get',
                       ?withArgs([change_id, Scope]),
                       ?andReturn(ChangeId)),
               ?expect('#get',
                       ?withArgs([ent_name, Scope]),
                       ?andReturn(?ENT_NAME))]),
    hoax:mock(deliv_change,
              [?expect(accept,
                       ?withArgs([ChangeId, Approver]),
                       ?andReturn({error, changeset_already_accepted}))]),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, iolist_to_binary(Message)]),
                      ?andReturn({ok, ignored}))),
    mock_fetch_by_alias_name(Author, Approver),
    mock_get_user_name(Approver),
    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, deliver, allow),
    ?assertMatch({ok, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"Nice, @delivery deliver this!">>, Author})),
    ?verifyAll.

handle_build_event_for_change_no_post_comment_before_verify() ->
    ChangeId = ?CHANGE_ID,
    Scope = mock_scope(),
    Patchset = deliv_patchset:'#new'(),

    hoax:mock(deliv_scopes,
              [?expect('#get',
                       ?withArgs([change_id, Scope]),
                       ?andReturn(ChangeId))]),
    hoax:mock(deliv_patchset,
              [?expect(latest_patchset_for_change,
                       ?withArgs([ChangeId]),
                       ?andReturn({ok, Patchset})),
               ?expect(getval,
                       ?withArgs([status, Patchset]),
                       ?andReturn(<<"open">>))]),
    hoax:mock(deliv_change,
              [?expect(validate_merge,
                       ?withArgs([ChangeId]),
                       ?andReturn({error, invalid_state})),
               ?expect(validate_accept,
                       ?withArgs([ChangeId]),
                       ?andReturn({error, invalid_state}))]),

    deliv_github_chatops:handle_build_event_for_change(Scope),
    ?verifyAll.

handle_build_event_for_change_no_post_comment_after_merge() ->
    IssueId = 1,
    Author = <<"mocked_author">>,
    Approver = deliv_user:'#new'(),
    Scope = mock_scope(),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, IssueId, ?any]),
                      ?andReturn({ok, ignored}))),
    hoax:mock(deliv_change,
              ?expect(merge,
                      ?withArgs([?CHANGE_ID, Approver]),
                      ?andReturn({error, involid_state}))),

    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, approve, allow),
    mock_fetch_by_alias_name(Author, Approver),
    mock_get_user_name(Approver),
    ?assertMatch({ok, _Why},
                 deliv_github_chatops:handle_comment_created({Scope, IssueId,
                                                              <<"@delivery approve">>, Author})),
    ?verifyAll.

check_user_permissions_returns_allow_when_action_allowed() ->
    Command = approve,
    Approver = deliv_user:'#new'(),
    Scope = deliv_scopes:'#new_common'([{ent_name, <<"Ent">>},
                                        {org_name, <<"Org">>},
                                        {proj_name, <<"Proj">>}]),

    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, approve, allow),
    mock_get_user_name(Approver),

    ?assertMatch(allow,
                 deliv_github_chatops:check_user_permissions(Scope, {ok, Approver}, Command)),
    ?verifyAll.

check_user_permissions_returns_forbid_when_action_not_allowed() ->
    Command = approve,
    Approver = deliv_user:'#new'(),
    Scope = deliv_scopes:'#new_common'([{ent_name, <<"Ent">>},
                                        {org_name, <<"Org">>},
                                        {proj_name, <<"Proj">>}]),

    mock_authorized_git_action(Scope, ?DELIVERY_USER_NAME, approve, forbid),
    mock_get_user_name(Approver),

    ?assertMatch(forbid,
                 deliv_github_chatops:check_user_permissions(Scope, {ok, Approver}, Command)),
    ?verifyAll.

check_user_permissions_returns_error_when_error_given() ->
    ?assertMatch({error, ignored},
                 deliv_github_chatops:check_user_permissions(ignored, {error, ignored}, ignored)).

mock_scope() ->
    deliv_scopes:'#new_common'([{ent_name, ?ENT_NAME},
                                {org_name, ?ORG_NAME},
                                {proj_name, ?PROJ_NAME},
                                {scoping_names, [?ENT_NAME, ?ORG_NAME,
                                                 ?PROJ_NAME, <<"Pipe">>]},
                                {change_id, ?CHANGE_ID}]).

mock_fetch_by_alias_name(Author, Approver) ->
    mock_fetch_by_alias_name(Author, Approver, {ok, Approver}).

mock_fetch_by_alias_name(Author, _Approver, Return) ->
    OauthApp = oauth_app,
    hoax:mock(deliv_github_oauth,
              ?expect(fetch_app_by_enterprise,
                      ?withArgs([?ENT_NAME]),
                      ?andReturn({ok, OauthApp}))),
    hoax:mock(deliv_user,
              ?expect(fetch_by_alias,
                      ?withArgs([OauthApp, Author]),
                      ?andReturn(Return))
             ).

mock_get_user_name(Approver) ->
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([name, Approver]),
                      ?andReturn(?DELIVERY_USER_NAME))).

mock_authorized_git_action(Scope, UserName, Command, Result) ->
    hoax:mock(deliv_git,
              ?expect(authorized_git_action,
                      ?withArgs([Scope, UserName, Command]),
                      ?andReturn(Result))).
