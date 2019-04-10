-module(deliv_github_api_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../../src/deliv_github_user_messages.hrl").

-compile(export_all).

hoax_fixture_test_ () ->
    hoax:fixture(?MODULE).

%% add_label_to_issue/3
add_label_to_issue_calls_proper_api() ->
    Scope = deliv_scopes:'#new_common'(),
    Route = ["/issues/", "1", "/labels"],
    Label = <<"Label">>,
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, post, Route, [Label]]),
                      ?andReturn({ok, 200, [], <<>>}))),

    ?assertEqual(ok, deliv_github_api:add_label_to_issue(Scope, 1, Label)),
    ?verifyAll.

add_label_to_issue_returns_msg_when_it_fails() ->
    Scope = deliv_scopes:'#new_common'(),
    Route = ["/issues/", "1", "/labels"],
    Label = <<"Label">>,
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, post, Route, [Label]]),
                      ?andReturn({ok, 400, [], <<>>}))),

    ?assertEqual({error, failed_to_add_label},
                 deliv_github_api:add_label_to_issue(Scope, 1, Label)),
    ?verifyAll.

add_label_to_issue_forwards_errors() ->
    Scope = deliv_scopes:'#new_common'(),
    Route = ["/issues/", "1", "/labels"],
    Label = <<"Label">>,
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, post, Route, [Label]]),
                      ?andReturn({error, why}))),

    ?assertEqual({error, why},
                 deliv_github_api:add_label_to_issue(Scope, 1, Label)),
    ?verifyAll.

%% get_file_contents/3
get_file_contents_returns_decoded_file_contents() ->
    Scope = deliv_scopes:'#new_common'([{scoping_names, [<<"Ent">>, <<"Org">>, <<"Proj">>, <<"Pipe">>]}]),
    Sha = <<"3d21ec53a331a6f037a91c368710b99387d012c1">>,
    Route = [<<"/contents/">>, <<"README.md">>, <<"?ref=">>, Sha],
    ResponseBody = ct_github:load_raw_payload_from_disk(<<"contents_README_md.json">>),
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, get, Route]),
                      ?andReturn({ok, 200, [], ResponseBody}))),

    ?assertEqual({ok, <<"README.md">>},
                 deliv_github_api:get_file_contents(Scope, <<"README.md">>,
                                                    <<"3d21ec53a331a6f037a91c368710b99387d012c1">>)),
    ?verifyAll.

get_file_contents_returns_error_when_config_fails_to_load() ->
    Scope = deliv_scopes:'#new_common'([{scoping_names, [<<"Ent">>, <<"Org">>, <<"Proj">>, <<"Pipe">>]}]),
    Sha = <<"3d21ec53a331a6f037a91c368710b99387d012c1">>,
    Route = [<<"/contents/">>, <<"README.md">>, <<"?ref=">>, Sha],
    ResponseBody = ct_github:load_payload_from_disk(<<"contents_README_md.json">>),
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, get, Route]),
                      ?andReturn({ok, 404, [], ResponseBody}))),
    hoax:mock(chef_log,
              ?expect(error,
                      ?withArgs(["Could not load the file ~s for "
                                 "~s/~s/~s/~s at ~s and sha ~s : HTTP Reponse Code ~b",
                                 [<<"README.md">>, <<"Ent">>, <<"Org">>, <<"Proj">>, <<"Pipe">>, Route, Sha, 404]]))),

    ?assertEqual({error, config_not_found},
                 deliv_github_api:get_file_contents(Scope, <<"README.md">>,
                                                    <<"3d21ec53a331a6f037a91c368710b99387d012c1">>)),
    ?verifyAll.

get_file_contents_forwards_errors() ->
    Scope = deliv_scopes:'#new_common'([{scoping_names, [<<"Ent">>, <<"Org">>, <<"Proj">>, <<"Pipe">>]}]),
    Sha = <<"3d21ec53a331a6f037a91c368710b99387d012c1">>,
    Route = [<<"/contents/">>, <<"README.md">>, <<"?ref=">>, Sha],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, get, Route]),
                      ?andReturn({error, why}))),
    hoax:mock(chef_log,
              ?expect(error,
                      ?withArgs(["Could not load the file ~s for "
                                 "~s/~s/~s/~s at ~s and sha ~s : ~p",
                                 [<<"README.md">>, <<"Ent">>, <<"Org">>, <<"Proj">>, <<"Pipe">>, Route, Sha, why]]))),

    ?assertEqual({error, why},
                 deliv_github_api:get_file_contents(Scope, <<"README.md">>,
                                                    <<"3d21ec53a331a6f037a91c368710b99387d012c1">>)),
    ?verifyAll.

merge_pull_request_returns_response_sha() ->
    Scope = deliv_scopes:'#new_common'(), %%    PatchsetId = 123,
    Message = <<"Mocked Message">>,
    PullRequestNumber = 1,
    Sha = <<"fake">>,
    Body = {[{<<"commit_message">>, Message}, {<<"sha">>, Sha}]},

    Route = ["/pulls/", chef_utils:to_str(PullRequestNumber), "/merge"],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, put, Route, Body]),
                      ?andReturn({ok, 200, <<"fake headers">>, chef_json:encode(ct_github:load_payload_from_disk(<<"pull_request_merged.json">>))}))),

    Response = deliv_github_api:merge_pull_request(Scope, PullRequestNumber, Sha, Message),
    ?assertEqual({ok, <<"6dcb09b5b57875f334f61aebed695e2e4193db5e">>}, Response),
    ?verifyAll.

merge_pull_request_returns_error_when_github_doesnt_merge() ->
    Scope = deliv_scopes:'#new_common'(),
    PullRequestNumber = 1,
    Message = <<"Mocked Message">>,
    Sha = <<"fake">>,
    Body = {[{<<"commit_message">>, Message}, {<<"sha">>, Sha}]},

    Route = ["/pulls/", chef_utils:to_str(PullRequestNumber), "/merge"],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, put, Route, Body]),
                      ?andReturn({ok, 405, <<"fake headers">>, ct_github:load_payload_from_disk(<<"pull_request_failed_merge.json">>)}))),
    Actual = deliv_github_api:merge_pull_request(Scope, PullRequestNumber, Sha, Message),
    ?assertEqual({error, failed_to_merge_branch}, Actual),
    ?verifyAll.

delete_branch_returns_ok_on_success() ->
    Scope = deliv_scopes:'#new_common'(),
    Branch = <<"mockedBranch">>,

    Route = ["/git/refs/heads/", Branch],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, delete, Route, {[]}]),
                      ?andReturn({ok, 204, ignored, ignored}))),

    Actual = deliv_github_api:delete_branch(Scope, Branch),
    ?assertEqual(ok, Actual),
    ?verifyAll.

delete_branch_returns_error_when_delete_fails() ->
    Scope = deliv_scopes:'#new_common'(),
    Branch = <<"mockedBranch">>,

    Route = ["/git/refs/heads/", Branch],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, delete, Route, {[]}]),
                      ?andReturn({ok, 400, ignored, ignored}))),

    Actual = deliv_github_api:delete_branch(Scope, Branch),
    ?assertEqual({error, github_branch_delete_failed}, Actual),
    ?verifyAll.

set_pull_request_status_returns_ok_on_success() ->
    Scope = deliv_scopes:'#new_common'(),

    Sha = <<"mocked_sha">>,
    Body = {[{<<"state">>, <<"pending">>},
             {<<"target_url">>, <<"http://yahoo.com">>},
             {<<"description">>, <<"Running!">>},
             {<<"context">>, <<"chef_delivery">>}]},
    Route = ["/statuses/", Sha],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, post, Route, Body]),
                      ?andReturn({ok, 201, ignored, ignored}))),

    Actual = deliv_github_api:set_pull_request_status(Scope, Sha, pending, <<"Running!">>,
                                                      <<"http://yahoo.com">>),
    ?assertEqual(ok, Actual),
    ?verifyAll.

set_pull_request_status_returns_error_when_github_errors() ->
    Scope = deliv_scopes:'#new_common'(),
    Description = <<"Chilling in test land">>,

    Sha = <<"mocked_sha">>,
    Body = {[{<<"state">>, <<"pending">>},
             {<<"target_url">>, <<"http://yahoo.com">>},
             {<<"description">>, Description},
             {<<"context">>, <<"chef_delivery">>}]},
    Route = ["/statuses/", Sha],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, post, Route, Body]),
                      ?andReturn({ok, 404, ignored, ignored}))),

    Actual = deliv_github_api:set_pull_request_status(Scope, Sha, pending, Description,
                                                      <<"http://yahoo.com">>),
    ?assertEqual({error, status_update_failed}, Actual),
    ?verifyAll.

create_commit_comment_returns_ok_on_success() ->
    Scope = deliv_scopes:'#new_common'(),

    Sha = <<"mocked_sha">>,
    Comment = <<"comment">>,
    Body = {[{<<"body">>, Comment}]},
    ResponseBody = Body,
    Route = ["/commits/", Sha, "/comments"],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, post, Route, Body]),
                      ?andReturn({ok, 201, ignored, ResponseBody}))),

    Actual = deliv_github_api:create_commit_comment(Scope, Sha, Comment),
    ?assertEqual({ok, ResponseBody}, Actual),
    ?verifyAll.

create_commit_comment_returns_error_when_github_errors() ->
    Scope = deliv_scopes:'#new_common'(),

    Sha = <<"mocked_sha">>,
    Comment = <<"comment">>,
    Body = {[{<<"body">>, Comment}]},
    Route = ["/commits/", Sha, "/comments"],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, post, Route, Body]),
                      ?andReturn({ok, 404, ignored, ignored}))),

    Actual = deliv_github_api:create_commit_comment(Scope, Sha, Comment),
    ?assertEqual({error, create_commit_comment_failed}, Actual),
    ?verifyAll.

create_issue_comment_returns_ok_on_success() ->
    Scope = deliv_scopes:'#new_common'(),

    IssueId = 1,
    Comment = <<"comment">>,
    Body = {[{<<"body">>, Comment}]},
    ResponseBody = Body,
    Route = ["/issues/", chef_utils:to_bin(IssueId), "/comments"],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, post, Route, Body]),
                      ?andReturn({ok, 201, ignored, ResponseBody}))),

    Actual = deliv_github_api:create_issue_comment(Scope, IssueId, Comment),
    ?assertEqual({ok, ResponseBody}, Actual),
    ?verifyAll.

create_issue_comment_returns_error_when_github_errors() ->
    Scope = deliv_scopes:'#new_common'(),

    IssueId = 1,
    Comment = <<"comment">>,
    Body = {[{<<"body">>, Comment}]},
    Route = ["/issues/", chef_utils:to_bin(IssueId), "/comments"],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, post, Route, Body]),
                      ?andReturn({ok, 404, ignored, ignored}))),

    Actual = deliv_github_api:create_issue_comment(Scope, IssueId, Comment),
    ?assertEqual({error, create_commit_comment_failed}, Actual),
    ?verifyAll.

update_commit_comment_returns_ok_on_success() ->
    Scope = deliv_scopes:'#new_common'(),

    Id = 1,
    Comment = <<"comment">>,
    Body = {[{<<"body">>, Comment}]},
    ResponseBody = Body,
    Route = ["/comments/", Id],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, patch, Route, Body]),
                      ?andReturn({ok, 200, ignored, ResponseBody}))),

    Actual = deliv_github_api:update_commit_comment(Scope, Id, Comment),
    ?assertEqual({ok, ResponseBody}, Actual),
    ?verifyAll.

update_commit_comment_returns_error_when_github_errors() ->
    Scope = deliv_scopes:'#new_common'(),

    Id = 1,
    Comment = <<"comment">>,
    Body = {[{<<"body">>, Comment}]},
    Route = ["/comments/", Id],
    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Scope, patch, Route, Body]),
                      ?andReturn({ok, 404, ignored, ignored}))),

    Actual = deliv_github_api:update_commit_comment(Scope, Id, Comment),
    ?assertEqual({error, update_commit_comment_failed}, Actual),
    ?verifyAll.

%% get_token_for_enterprise/2
get_token_for_enterprise_returns_redirect_url_if_no_token() ->
    RedirectUrl = "http://github.example.com/oauth/authorize_url",
    OauthAppName = <<"github">>,

    EntName = <<"EntName">>,
    EntId = 4,
    Enterprise = deliv_enterprise:'#new'(),

    OauthToken = deliv_oauth_token:'#new'(),

    hoax:mock(deliv_enterprise, [
                                 ?expect(fetch,
                                         ?withArgs([EntName]),
                                         ?andReturn({ok, Enterprise})),
                                 ?expect(getval,
                                         ?withArgs([id, Enterprise]),
                                         ?andReturn(EntId))
                                ]),

    hoax:mock(deliv_oauth_token, [
                                  ?expect(initialize,
                                          ?withArgs([OauthAppName, enterprise, EntId]),
                                          ?andReturn({ok, OauthToken})),
                                  ?expect(authorize_url,
                                          ?withArgs([OauthToken]),
                                          ?andReturn(RedirectUrl))
                                 ]),

    ?assertEqual({ok, RedirectUrl},
                 deliv_github_api:get_token_for_enterprise(OauthAppName, EntName)),
    ?verifyAll.

get_token_for_enterprise_returns_error_msg_if_token_already_exists() ->
    OauthAppName = <<"github">>,

    EntName = <<"EntName">>,
    EntId = 4,
    Enterprise = deliv_enterprise:'#new'(),

    hoax:mock(deliv_enterprise, [
                                 ?expect(fetch,
                                         ?withArgs([EntName]),
                                         ?andReturn({ok, Enterprise})),
                                 ?expect(getval,
                                         ?withArgs([id, Enterprise]),
                                         ?andReturn(EntId))
                                ]),

    hoax:mock(deliv_oauth_token, [
                                  ?expect(initialize,
                                          ?withArgs([OauthAppName, enterprise, EntId]),
                                          ?andReturn({error, conflict}))
                                 ]),

    ?assertEqual({error, chef_utils:iodata_to_str(?GITHUB_TOKEN_EXIST_MSG(EntName))},
                 deliv_github_api:get_token_for_enterprise(OauthAppName, EntName)),
    ?verifyAll.

get_token_for_enterprise_returns_error_msg_if_enterprise_does_not_exist() ->
    OauthAppName = <<"github">>,

    EntName = <<"EntName">>,

    hoax:mock(deliv_enterprise, [
                                 ?expect(fetch,
                                         ?withArgs([EntName]),
                                         ?andReturn({error, not_found}))
                                ]),


    ?assertEqual({error, chef_utils:iodata_to_str(?GITHUB_ENT_EXIST_MSG(EntName))},
                 deliv_github_api:get_token_for_enterprise(OauthAppName, EntName)),
    ?verifyAll.

get_token_for_enterprise_returns_error_msg_if_oauth_app_does_not_exist() ->
    OauthAppName = <<"github">>,

    EntName = <<"EntName">>,
    EntId = 4,
    Enterprise = deliv_enterprise:'#new'(),

    hoax:mock(deliv_enterprise, [
                                 ?expect(fetch,
                                         ?withArgs([EntName]),
                                         ?andReturn({ok, Enterprise})),
                                 ?expect(getval,
                                         ?withArgs([id, Enterprise]),
                                         ?andReturn(EntId))
                                ]),

    hoax:mock(deliv_oauth_token, [
                                  ?expect(initialize,
                                          ?withArgs([OauthAppName, enterprise, EntId]),
                                          ?andReturn({error, not_found}))
                                 ]),

    ?assertEqual({error, chef_utils:iodata_to_str(?GITHUB_OAUTH_APP_EXIST_MSG(OauthAppName))},
                 deliv_github_api:get_token_for_enterprise(OauthAppName, EntName)),
    ?verifyAll.
