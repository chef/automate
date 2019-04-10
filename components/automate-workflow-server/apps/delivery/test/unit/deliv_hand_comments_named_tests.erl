-module(deliv_hand_comments_named_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

resource_exists_test_() ->
  hoax:fixture(?MODULE, resource_exists).

delete_resource_test_() ->
  hoax:fixture(?MODULE, delete_resource).

from_json_test_() ->
    hoax:fixture(?MODULE, from_json).

init_returns_standard_return_test() ->
  ?assertEqual({upgrade, protocol, cowboy_rest, req, state}, deliv_hand_comments_named:init(whatever, req, state)).

allowed_methods_allows_DELETE_and_PUT_test() ->
    ?assertEqual({[<<"DELETE">>, <<"PUT">>], req, state},
                 deliv_hand_comments_named:allowed_methods(req, state)).

content_types_accepted_provides_json_test() ->
    hoax:test(fun() ->
        hoax:mock(deliv_web_utils,
                    ?expect(content_type_json_map,
                          ?withArgs([from_json]),
                          ?andReturn(expected_map))),

        Actual = deliv_hand_comments_named:content_types_accepted(req, state),

        ?assertEqual({expected_map, req, state}, Actual),
        ?verifyAll
    end).

resource_exists_returns_false_if_no_comment_exists() ->
    CommentId = <<"1">>,
    Req = req,
    State = state,
    Req1 = req1,

    hoax:expect(receive
                      deliv_web_utils:extract_bindings([comment_id], Req) -> {[CommentId], Req1};
                      deliv_comment:fetch(list_to_integer(binary_to_list(CommentId))) -> {error, not_found}
                end),

    Result = deliv_hand_comments_named:resource_exists(Req, State),
    ?assertEqual({false, Req1, State}, Result),
    ?verifyAll.


resource_exists_returns_true_and_the_comment_if_comment_exists() ->
    CommentId = <<"1">>,
    Req = req,
    State = state,
    Req1 = req1,
    Comment = comment,

    hoax:expect(receive
                      deliv_web_utils:extract_bindings([comment_id], Req) -> {[CommentId], Req1};
                      deliv_comment:fetch(list_to_integer(binary_to_list(CommentId))) -> {ok, Comment}
                end),

    Result = deliv_hand_comments_named:resource_exists(Req, State),
    ?assertEqual({true, Req1, Comment}, Result),
    ?verifyAll.

delete_resource_if_no_child_comments_forwards_error_if_parent_delete_fails() ->
    Comment = comment,
    CommentId = 1,
    Req = req,
    DelivWebUtilsResp = error,

    hoax:expect(receive
                  deliv_comment:getval(id, Comment) -> CommentId;
                  deliv_comment:delete(CommentId) -> {error, whyyyy};
                  deliv_web_utils:error_response(500, internal_server_error, Req, Comment) -> DelivWebUtilsResp
            end),
    Result = deliv_hand_comments_named:delete_resource(Req, Comment),

    ?assertEqual(DelivWebUtilsResp, Result),
    ?verifyAll.

delete_resource_if_no_child_comments_deletes_comment_and_returns_true() ->
    Comment = comment,
    CommentId = 1,
    Req = req,

    hoax:expect(receive
                 deliv_comment:getval(id, Comment) -> CommentId;
                 deliv_comment:delete(CommentId) -> ok
            end),
    Result = deliv_hand_comments_named:delete_resource(Req, Comment),
    ?assertEqual({true, Req, []}, Result),
    ?verifyAll.

from_json_returns_400_bad_request_for_incomplete_json() ->
    Comment = comment,

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, update_comment) -> {{error, bad_request}, req1};
                    deliv_web_utils:error_response(400, bad_request, req1, Comment) -> {error, bad_request}
                end),

    Result = deliv_hand_comments_named:from_json(req, Comment),

    ?assertEqual({error, bad_request}, Result),
    ?verifyAll.

from_json_returns_500_if_update_fails() ->
    Comment = comment,
    NewContent = <<"this is the new content">>,
    NewComment = {[
                    {<<"id">>, <<1>>},
                    {<<"author">>, <<"whoever">>},
                    {<<"content">>, NewContent}
                  ]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, update_comment) -> {NewComment, req1};
                    deliv_comment:setvals([{content, NewContent}], Comment) -> updated_comment;
                    deliv_comment:update_content(updated_comment) -> {error, noooooo};
                    deliv_web_utils:error_response(500, internal_server_error, req1, updated_comment) -> {error, internal_server_error}
                end),

    Result = deliv_hand_comments_named:from_json(req, Comment),

    ?assertEqual({error, internal_server_error}, Result),
    ?verifyAll.

from_json_returns_200_on_success() ->
    Comment = comment,
    NewContent = <<"this is the new content">>,
    NewComment = {[
                    {<<"id">>, <<1>>},
                    {<<"author">>, <<"whoever">>},
                    {<<"content">>, NewContent}
                  ]},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, update_comment) -> {NewComment, req1};
                    deliv_comment:setvals([{content, NewContent}], Comment) -> updated_comment_obj;
                    deliv_comment:update_content(updated_comment_obj) -> {ok, updated_comment_rec}
                end),

    Result = deliv_hand_comments_named:from_json(req, Comment),

    ?assertEqual({true, req1, updated_comment_rec}, Result),
    ?verifyAll.
