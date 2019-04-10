-module(deliv_github_pull_request_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

-define(PAYLOAD, ct_github:load_payload_from_disk("pull_request_opened.json")).

get_user_name_from_payload_test() ->
    ?assertEqual(<<"baxterthehacker">>, deliv_github_pull_request:user_name(?PAYLOAD)).

get_pipe_name_from_payload_test() ->
    ?assertEqual(<<"master">>, deliv_github_pull_request:pipe_name(?PAYLOAD)).

get_feature_branch_from_payload_test() ->
    ?assertEqual(<<"changes">>, deliv_github_pull_request:feature_branch(?PAYLOAD)).

get_commit_sha_from_payload_test() ->
    ?assertEqual(<<"0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c">>,
                 deliv_github_pull_request:commit_sha(?PAYLOAD)).

get_change_title_from_payload_test() ->
    ?assertEqual(<<"Update the README with new information">>,
                 deliv_github_pull_request:change_title(?PAYLOAD)).

get_change_desc_from_payload_test() ->
    ?assertEqual(<<"This is a pretty simple change that we need to pull into master.">>,
                 deliv_github_pull_request:change_desc(?PAYLOAD)).

quarantine_fixture_test_() ->
    hoax:fixture(?MODULE, "quarantine_").

quarantine_adds_label_to_pr() ->
    Scope = deliv_scopes:'#new_common'(),
    Payload = ct_github:load_payload_from_disk("pull_request_opened.json"),
    Label = <<"Review Required">>,
    Comment = <<"Hi. I'm the Delivery bot. Thanks for the pull request! I've alerted "
                "the maintainers of this project so that they can review the change.">>,
    ResponseBody = <<>>,

    hoax:mock(deliv_github_api, [
                                 ?expect(add_label_to_issue,
                                         ?withArgs([Scope, 1, Label]),
                                         ?andReturn(ok)),
                                 ?expect(create_issue_comment,
                                         ?withArgs([Scope, <<1>>, Comment]),
                                         ?andReturn({ok, ResponseBody}))
                                ]),

    Result = deliv_github_pull_request:quarantine(Payload, Scope),
    ?assertEqual({ok, ResponseBody}, Result),
    ?verifyAll.
