%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
-module(deliv_hand_change_sse_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("deliv_types.hrl").

-compile(export_all).

format_event_fixture_test_() ->
    hoax:fixture(?MODULE, "format_event").

format_event_with_pipeline_blocked_true_returns_promotion_caution() ->
    Req = req,
    State = state,
    Status = true,
    PipeId = 1,
    Ejson = {[
        {<<"promotion">>, {[
            {<<"status">>, <<"caution">>},
            {<<"reason">>, <<"pipeline_union_failure">>}
        ]}}
    ]},

    hoax:mock(deliv_web_sse,
              ?expect(format_event,
                      ?withArgs([?any, <<"promotion_status_update">>, {ejson, Ejson}]),
                      ?andReturn(sse_iodata))),

    Expected = {keep_open, Req, State, sse_iodata},
    Result = deliv_hand_change_sse:format_event({pipeline_blocked, PipeId}, Status,
        Req, State),
    ?verifyAll,
    ?assertEqual(Expected, Result).

format_event_with_pipeline_blocked_false_returns_promotion_succeed() ->
    Req = req,
    State = state,
    Status = false,
    PipeId = 1,
    Ejson = {[
        {<<"promotion">>, {[
            {<<"status">>, <<"proceed">>}
        ]}}
    ]},

    hoax:mock(deliv_web_sse,
              ?expect(format_event,
                      ?withArgs([?any, <<"promotion_status_update">>, {ejson, Ejson}]),
                      ?andReturn(sse_iodata))),

    Expected = {keep_open, Req, State, sse_iodata},
    Result = deliv_hand_change_sse:format_event({pipeline_blocked, PipeId}, Status,
        Req, State),
    ?verifyAll,
    ?assertEqual(Expected, Result).

format_event_with_change_superseded_returns_promotion_disabled() ->
    Req = req,
    State = state,
    ChangeId = <<"123-456-789">>,
    Change = deliv_change:fromlist([{id, ChangeId}]),
    Ejson = {[
        {<<"promotion">>, {[
            {<<"status">>, <<"disabled">>},
            {<<"reason">>, <<"change_superseded">>}
        ]}}
    ]},

    hoax:mock(deliv_web_sse,
              ?expect(format_event,
                      ?withArgs([?any, <<"promotion_status_update">>, {ejson, Ejson}]),
                      ?andReturn(sse_iodata))),

    Expected = {close, Req, State, sse_iodata},
    Result = deliv_hand_change_sse:format_event({change_superseded, ChangeId}, Change,
        Req, State),
    ?verifyAll,
    ?assertEqual(Expected, Result).
