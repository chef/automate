-module(deliv_github_event_tests).

-include_lib("delivery/include/deliv_types.hrl").
-include("../../src/deliv_github_event_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

-define(ENT_NAME, <<"EntName">>).
-define(ORG_NAME, <<"OrgName">>).
-define(PROJ_NAME, <<"ProjName">>).
-define(DELIV_USER_NAME, <<"DeliveryUserName">>).
-define(QUARANTINE_USER, <<"untrusted_github_user">>).
-define(COORDINATES, #proj_coordinates{ent_name = ?ENT_NAME, org_name = ?ORG_NAME, proj_name = ?PROJ_NAME}).

handle_fixture_test_() ->
    hoax:fixture(?MODULE, "handle_").

handle_pull_request_event_type_action_opened_valid_user() ->
    Payload = ct_github:load_payload_from_disk("pull_request_opened.json"),
    User = deliv_user:'#new'(),
    _Scope = mock_deliv_scopes(),
    mock_fetch_user_alias_and_return({ok, User}),
    ChangeId = mock_create_change(github_pull_request_opened, ?DELIV_USER_NAME, Payload),
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([name, User]),
                      ?andReturn(?DELIV_USER_NAME))),
    hoax:mock(deliv_change,
              ?expect(verify_patchset,
                      ?withArgs([ChangeId]))),

    deliv_github_event:handle(#github_pr{action = opened, proj_coordinates = ?COORDINATES,
                                                 payload = Payload}),
    ?verifyAll.

handle_pull_request_event_type_action_opened_invalid_user() ->
    Payload = ct_github:load_payload_from_disk("pull_request_opened.json"),
    Scope = mock_deliv_scopes(),
    mock_fetch_user_alias_and_return({error, not_found}),
    _ChangeId = mock_create_change(github_pull_request_opened, ?QUARANTINE_USER, Payload),
    hoax:mock(deliv_github_pull_request,
              ?expect(quarantine,
                      ?withArgs([Payload, Scope]))),

    deliv_github_event:handle(#github_pr{action = opened, proj_coordinates = ?COORDINATES,
                                                 payload = Payload}),

    ?verifyAll.

handle_pull_request_event_type_action_opened_from_a_forked_project() ->
    Payload = ct_github:load_payload_from_disk("forked_pull_request_opened.json"),
    Scope = mock_deliv_scopes(),
    PrNum = deliv_github_pull_request:pull_request_number(Payload),
    Comment = <<"I'm sorry, but this project does not accept pull requests from forked repositories.">>,
    hoax:mock(deliv_github_pull_request, [
              ?expect(forked,
                      ?withArgs([Payload]),
                      ?andReturn(true)),
              ?expect(pull_request_number,
                      ?withArgs([Payload]),
                      ?andReturn(PrNum))
            ]),
    hoax:mock(deliv_github_api,
              ?expect(create_issue_comment,
                      ?withArgs([Scope, PrNum, Comment]),
                      ?andReturn({ok, response_body}))),

    ?assertEqual({error, forked_project}, deliv_github_event:handle(#github_pr{action = opened, proj_coordinates = ?COORDINATES, payload = Payload})),

    ?verifyAll.

handle_pull_request_event_type_action_synchronize() ->
    Payload = ct_github:load_payload_from_disk("pull_request_synchronize.json"),
    _Scope = mock_deliv_scopes(),
    User = deliv_user:'#new'(),
    mock_fetch_user_alias_and_return({ok, User}),
    ChangeId = mock_create_change(github_pull_request_synchronize, ?DELIV_USER_NAME, Payload),
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([name, User]),
                      ?andReturn(?DELIV_USER_NAME))),
    hoax:mock(deliv_change,
              ?expect(verify_patchset,
                      ?withArgs([ChangeId]))),

    deliv_github_event:handle(#github_pr{action = synchronized, proj_coordinates = ?COORDINATES,
                                                 payload = Payload}),
    ?verifyAll.

handle_pull_closed_event_type() ->
    hoax:mock(chef_log,
              ?expect(debug,
                      ?withArgs(["Received pull request closed from Github for ~s/~s/~s", [?ENT_NAME, ?ORG_NAME, ?PROJ_NAME]]))),

    deliv_github_event:handle(#github_pr{action=closed, proj_coordinates = ?COORDINATES, payload = unused_payload}),
    ?verifyAll.

handle_issue_comment_created() ->
    Payload = ct_github:load_payload_from_disk("issue_comment_created.json"),
    ChangeId = mocked_change_id,

    IssueUrl = <<"https://github.chef.co/api/v3/repos/jmink/outdoors/issues/14">>,
    IssueNumber = 14,
    Comment = <<"@delivery approve">>,
    Author = <<"jmink">>,
    Scope = deliv_scopes:'#new_common'(),

    hoax:mock(deliv_github_patchset,
              ?expect(fetch_latest_by_issue_url,
                      ?withArgs([IssueUrl]),
                      ?andReturn([github_patchset]))),
    hoax:mock(deliv_github_patchset,
              ?expect(get_change_id_by_patchset,
                      ?withArgs([github_patchset]),
                      ?andReturn(ChangeId))),
    hoax:mock(deliv_scopes,
              ?expect(from_change_id,
                      ?withArgs([ChangeId]),
                      ?andReturn(Scope))),
    hoax:mock(deliv_event,
              ?expect(publish,
                      ?withArgs([github_comment_created, {Scope, IssueNumber, Comment, Author}]))),

    deliv_github_event:handle(#github_comment{action=created, proj_coordinates = ?COORDINATES, payload = Payload}),
    ?verifyAll.

%%
%% Mock Helpers
%%

mock_fetch_user_alias_and_return(Return) ->
    AppId = 1,
    OauthApp = deliv_oauth_application:fromlist([{id, AppId}]),

    hoax:mock(deliv_github_oauth,
              ?expect(fetch_app_by_enterprise,
                      ?withArgs([?ENT_NAME]),
                      ?andReturn({ok, OauthApp}))),
    hoax:mock(deliv_user,
              ?expect(fetch_by_alias,
                      ?withArgs([OauthApp, <<"baxterthehacker">>]),
                      ?andReturn(Return))).


mock_deliv_scopes() ->
    Scope = deliv_scopes:'#new_common'(),
    hoax:mock(deliv_scopes,
              ?expect(from_scoping_names,
                      ?withArgs([?ENT_NAME, ?ORG_NAME, ?PROJ_NAME]),
                      ?andReturn(Scope))),
    Scope.

mock_create_change(EventKey, UserName, Payload) ->
    ChangeId = <<"ChangeUUID">>,
    Patchset = deliv_patchset:'#new'(),
    GithubUserName = deliv_github_pull_request:user_name(Payload),
    PrNum = deliv_github_pull_request:pull_request_number(Payload),
    Org = deliv_github_pull_request:repo_owner(Payload),
    Repo = deliv_github_pull_request:repo_name(Payload),
    PipeName = deliv_github_pull_request:pipe_name(Payload),
    FeatBranchName = deliv_github_pull_request:feature_branch(Payload),
    ToCommitSha = deliv_github_pull_request:commit_sha(Payload),
    ChangeTitle = deliv_github_pull_request:change_title(Payload),
    ChangeDescription = deliv_github_pull_request:change_desc(Payload),
    Change = UpdatedChange = deliv_change:'#new'(),
    ChangeWithTitleAndDescription = deliv_change:fromlist([
                                                           {title, ChangeTitle},
                                                           {description, ChangeDescription}
                                                          ]),
    hoax:mock(deliv_github_pull_request, [
                                          ?expect(user_name,
                                                  ?withArgs([Payload]),
                                                  ?andReturn(GithubUserName)),
                                          ?expect(pull_request_number,
                                                  ?withArgs([Payload]),
                                                  ?andReturn(PrNum)),
                                          ?expect(repo_owner,
                                                  ?withArgs([Payload]),
                                                  ?andReturn(Org)),
                                          ?expect(repo_name,
                                                  ?withArgs([Payload]),
                                                  ?andReturn(Repo)),
                                          ?expect(pipe_name,
                                                  ?withArgs([Payload]),
                                                  ?andReturn(PipeName)),
                                          ?expect(feature_branch,
                                                  ?withArgs([Payload]),
                                                  ?andReturn(FeatBranchName)),
                                          ?expect(commit_sha,
                                                  ?withArgs([Payload]),
                                                  ?andReturn(ToCommitSha)),
                                          ?expect(change_title,
                                                  ?withArgs([Payload]),
                                                  ?andReturn(ChangeTitle)),
                                          ?expect(change_desc,
                                                  ?withArgs([Payload]),
                                                  ?andReturn(ChangeDescription)),
                                          ?expect(forked,
                                                  ?withArgs([Payload]),
                                                  ?andReturn(false))
                                         ]),
    hoax:mock(deliv_patchset, [
                               ?expect(new,
                                       ?withArgs([?ENT_NAME,
                                                  UserName,
                                                  ?ORG_NAME,
                                                  ?PROJ_NAME,
                                                  PipeName,
                                                  FeatBranchName,
                                                  ToCommitSha]),
                                       ?andReturn([Patchset])),
                               ?expect(getval,
                                       ?withArgs([change_id, Patchset]),
                                       ?andReturn(ChangeId))
                              ]),
    hoax:mock(deliv_change, [
                             ?expect(fetch_by_id,
                                     ?withArgs([ChangeId]),
                                     ?andReturn({ok, Change})),
                             ?expect(setvals,
                                     ?withArgs([
                                                [{title, ChangeTitle},
                                                 {description, ChangeDescription}],
                                                Change
                                               ]),
                                     ?andReturn(ChangeWithTitleAndDescription)),
                             ?expect(update,
                                     ?withArgs([ChangeWithTitleAndDescription]),
                                     ?andReturn(UpdatedChange))
                            ]),
    hoax:mock(deliv_github_patchset, [?expect(save, ?withArgs([Payload, Patchset]))]),
    hoax:mock(deliv_event, [
                            ?expect(publish,
                                    ?withArgs([EventKey, {ChangeId, Payload}])),
                            ?expect(publish,
                                    ?withArgs([patchset_created, Patchset]))
                           ]),
    ChangeId.
