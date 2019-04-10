-module(deliv_hand_verify_token_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

handle_fixture_test_() ->
    hoax:fixture(?MODULE, handle_).

handle_when_method_is_get_and_token_is_authorized_returns_ok() ->
    hoax:expect(receive
                    cowboy_req:method(req) -> {<<"GET">>, req1};
                    deliv_token:is_authorized(req1, state) -> {true, req_ignored, state_ignored};
                    cowboy_req:reply(200, req1) -> {ok, req2}
                end),
    Actual = deliv_hand_verify_token:handle(req, state),
    ?assertEqual({ok, req2, state}, Actual),
    ?verifyAll.

handle_when_method_is_head_and_token_is_authorized_returns_ok() ->
    hoax:expect(receive
                    cowboy_req:method(req) -> {<<"HEAD">>, req1};
                    deliv_token:is_authorized(req1, state) -> {true, req_ignored, state_ignored};
                    cowboy_req:reply(200, req1) -> {ok, req2}
                end),
    Actual = deliv_hand_verify_token:handle(req, state),
    ?assertEqual({ok, req2, state}, Actual),
    ?verifyAll.

handle_when_method_is_get_and_token_is_not_authorized_sends_401_reply_and_returns_ok() ->
    hoax:expect(receive
                    cowboy_req:method(req) -> {<<"GET">>, req1};
                    deliv_token:is_authorized(req1, state) -> {false, req_ignored, state_ignored};
                    cowboy_req:reply(401, req1) -> {ok, req2}
                end),
    Actual = deliv_hand_verify_token:handle(req, state),
    ?assertEqual({ok, req2, state}, Actual),
    ?verifyAll.

handle_when_method_is_head_and_token_is_not_authorized_sends_401_reply_and_returns_ok() ->
    hoax:expect(receive
                    cowboy_req:method(req) -> {<<"HEAD">>, req1};
                    deliv_token:is_authorized(req1, state) -> {false, req_ignored, state_ignored};
                    cowboy_req:reply(401, req1) -> {ok, req2}
                end),
    Actual = deliv_hand_verify_token:handle(req, state),
    ?assertEqual({ok, req2, state}, Actual),
    ?verifyAll.

handle_when_method_is_anything_else_sends_405_reply_and_returns_ok() ->
    hoax:expect(receive
                    cowboy_req:method(req) -> {<<"DELETE">>, req1};
                    cowboy_req:reply(405, req1) -> {ok, req2}
                end),
    Actual = deliv_hand_verify_token:handle(req, state),
    ?assertEqual({ok, req2, state}, Actual),
    ?verifyAll.
