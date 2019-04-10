-module(deliv_web_sse_tests).

-include("deliv_types.hrl").

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

init_fixture_test_() ->
    hoax:fixture(?MODULE, "init_").

init_only_applies_init_event_stream_when_sse_module_is_deliv_hand_license_sse() ->
    State = #handler{},
    RespHeaders = [{<<"content-type">>, <<"text/event-stream">>},
                   {<<"cache-control">>, <<"no-cache">>}],
    hoax:mock(deliv_token,
              ?expect(is_authorized,
                      ?withArgs([req, State]),
                      ?times(0))
              ),
    hoax:mock(deliv_authz,
              ?expect(forbidden,
                      ?withArgs([req, State]),
                      ?times(0))
              ),
    hoax:expect(receive
                    cowboy_req:chunked_reply(200, RespHeaders, req) -> {ok, req};
                    cowboy_req:set_resp_header(<<"connection">>, <<"close">>, req) -> req;
                    deliv_event:subscribe([license]) -> true
                end),
    %% handler_wrapper record.
    HandlerWrapper = {handler_wrapper, #handler{},
                      [license],
                     deliv_hand_license_sse},
    ?assertEqual({loop, req, HandlerWrapper, 60000}, deliv_web_sse:init(deliv_hand_license_sse, req, State)),
    ?verifyAll.

init_applies_auth_and_init_event_stream_when_sse_module_is_anything_else() ->
    State = #handler{},
    RespHeaders = [{<<"content-type">>, <<"text/event-stream">>},
                   {<<"cache-control">>, <<"no-cache">>}],
    hoax:expect(receive
                    deliv_token:is_authorized(req, State) -> {true, req, State};
                    deliv_authz:forbidden_for_change_action(req, State) -> {false, req, State};
                    cowboy_req:chunked_reply(200, RespHeaders, req) -> {ok, req};
                    cowboy_req:set_resp_header(<<"connection">>, <<"close">>, req) -> req;
                    deliv_hand_change_sse:events(req, State) -> {req, State, [event]};
                    deliv_hand_change_sse:forbidden_callback() -> forbidden_for_change_action;
                    deliv_event:subscribe([event]) -> true
                end),
    %% handler_wrapper record.
    HandlerWrapper = {handler_wrapper, #handler{}, [event], deliv_hand_change_sse},
    ?assertEqual({loop, req, HandlerWrapper, 60000}, deliv_web_sse:init(deliv_hand_change_sse, req, State)),
    ?verifyAll.


format_event_test_() ->
    [
     {"with some iodata",
      ?_assertEqual([<<"id: ">>, "fake_id", $\n,
                    <<"event: ">>, "fake_event", $\n,
                    <<"data: ">>, "fake_data",
                    $\n, $\n],
                    deliv_web_sse:format_event("fake_id",
                                               "fake_event",
                                               {iodata, "fake_data"}))},
     {"with some json",
      ?_assertEqual([<<"id: ">>, "fake_id", $\n,
                    <<"event: ">>, "fake_event", $\n,
                    <<"data: ">>, <<"{\"key\":\"val\"}">>,
                    $\n, $\n],
                    deliv_web_sse:format_event("fake_id",
                                               "fake_event",
                                               {ejson, {[{<<"key">>, <<"val">>}]}}))}
    ].
