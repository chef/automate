-module(deliv_hand_change_merge_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

fixture_test_() ->
    hoax:fixture(?MODULE, "handle_").

handle_when_merge_conflict_returns_helpful_error_message() ->
    Req = req,
    EntName = <<"ent">>,
    UserName = <<"user">>,
    State = #handler{ent_name = EntName, user_name = UserName},
    ChangeId = <<"cid">>,
    Merger = <<"merger">>,
    ErrorMessage = <<"Automatic merge failed. Fix merge conflicts and resubmit for review.">>,

    hoax:expect(receive
                    cowboy_req:binding(change_id, Req) -> {ChangeId, req1};
                    deliv_user:fetch(EntName, UserName) -> {ok, Merger};
                    deliv_change:merge(ChangeId, Merger) -> {error, feature_branch_merge_failed};
                    deliv_web_utils:error_response(412, precondition_failed, ErrorMessage, req1, State) -> error_result
                end),

    ?assertEqual(error_result, deliv_hand_change_merge:handle(Req, State)),
    ?verifyAll.
