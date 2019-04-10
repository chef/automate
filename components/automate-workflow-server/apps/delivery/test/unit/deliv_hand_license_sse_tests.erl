-module(deliv_hand_license_sse_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("test_support/include/test_support.hrl").

-compile(export_all).

%% Fixtures
init_fixture_test_() ->
    hoax:fixture(?MODULE, "init_").

format_event_test_() ->
    hoax:fixture(?MODULE, "format_event_").

%% Tests
behaviour_test() ->
    ModuleAttributes = deliv_hand_license_sse:module_info(attributes),
    ?assertEqual([deliv_web_sse], proplists:get_value(behaviour, ModuleAttributes)).

mixin_exports_present_test() ->
    HandleFun = fun deliv_hand_license_sse:handle/2,
    InfoFun = fun deliv_hand_license_sse:info/3,
    TermFun = fun deliv_hand_license_sse:terminate/3,
    ?assertFunctionExported(HandleFun),
    ?assertFunctionExported(InfoFun),
    ?assertFunctionExported(TermFun).

%% Ensure we ask to subscribe to license events.
events_returns_event_array_containing_subscribed_events_test() ->
    ?assertEqual({req, state, [license]}, deliv_hand_license_sse:events(req, state)).

%% Fixture Tests
init_calls_deliv_web_sse_init3() ->
    hoax:expect(receive
                    deliv_web_sse:init(deliv_hand_license_sse, req, state) -> ignored
                end),
    ?assertEqual(ignored, deliv_hand_license_sse:init(transport, req, state)),
    ?verifyAll.

format_event_calls_deliv_web_sse_format_event3_and_returns_valid_license_keep_alive_with_license_data_when_license_is_valid() ->
    LicenseData = {[{<<"id">>, <<"01234-56789AB-CDEF">>}]},
    ExpectedJson = {[{<<"status">>, valid},
                     {<<"license_data">>, LicenseData}]},
    hoax:expect(receive
                    chef_utils:random_hex_string(16) -> <<"9f8ede1806b45df2c76b9fe4d3eb8612">>;
                    deliv_web_sse:format_event(<<"9f8ede1806b45df2c76b9fe4d3eb8612">>,
                                               <<"license">>,
                                               {ejson, ExpectedJson}) -> iodata
                end),
    ?assertEqual({keep_open, req, state, iodata},
                 deliv_hand_license_sse:format_event(license, {valid, LicenseData}, req, state)),
    ?verifyAll.

format_event_calls_deliv_web_sse_format_event3_and_returns_expired_license_keep_alive_with_license_data_when_license_is_expired() ->
    LicenseData = {[{<<"id">>, <<"01234-56789AB-CDEF">>}]},
    ExpectedJson = {[{<<"status">>, expired},
                     {<<"license_data">>, LicenseData}]},
    hoax:expect(receive
                    chef_utils:random_hex_string(16) -> <<"9f8ede1806b45df2c76b9fe4d3eb8612">>;
                    deliv_web_sse:format_event(<<"9f8ede1806b45df2c76b9fe4d3eb8612">>,
                                               <<"license">>,
                                               {ejson, ExpectedJson}) -> iodata
                end),
    ?assertEqual({keep_open, req, state, iodata},
                 deliv_hand_license_sse:format_event(license, {expired, LicenseData}, req, state)),
    ?verifyAll.

format_event_calls_deliv_web_sse_format_event3_and_returns_invalid_type_license_keep_alive_with_license_data_when_license_has_invalid_type() ->
    LicenseData = {[{<<"id">>, <<"01234-56789AB-CDEF">>}]},
    ExpectedJson = {[{<<"status">>, invalid_type},
                     {<<"license_data">>, LicenseData}]},
    hoax:expect(receive
                    chef_utils:random_hex_string(16) -> <<"9f8ede1806b45df2c76b9fe4d3eb8612">>;
                    deliv_web_sse:format_event(<<"9f8ede1806b45df2c76b9fe4d3eb8612">>,
                                               <<"license">>,
                                               {ejson, ExpectedJson}) -> iodata
                end),
    ?assertEqual({keep_open, req, state, iodata},
                 deliv_hand_license_sse:format_event(license, {invalid_type, LicenseData}, req, state)),
    ?verifyAll.

format_event_calls_deliv_web_sse_format_event3_and_returns_invalid_license_keep_alive_when_license_is_invalid() ->
    LicenseData = undefined,
    ExpectedJson = {[{<<"status">>, invalid},
                     {<<"license_data">>, LicenseData}]},
    hoax:expect(receive
                    chef_utils:random_hex_string(16) -> <<"9f8ede1806b45df2c76b9fe4d3eb8612">>;
                    deliv_web_sse:format_event(<<"9f8ede1806b45df2c76b9fe4d3eb8612">>,
                                               <<"license">>,
                                               {ejson, ExpectedJson}) -> iodata
                end),
    ?assertEqual({keep_open, req, state, iodata},
                 deliv_hand_license_sse:format_event(license, {invalid, LicenseData}, req, state)),
    ?verifyAll.

format_event_calls_deliv_web_sse_format_event3_and_returns_missing_license_keep_alive_when_license_is_missing() ->
    LicenseData = undefined,
    ExpectedJson = {[{<<"status">>, missing},
                     {<<"license_data">>, LicenseData}]},
    hoax:expect(receive
                    chef_utils:random_hex_string(16) -> <<"9f8ede1806b45df2c76b9fe4d3eb8612">>;
                    deliv_web_sse:format_event(<<"9f8ede1806b45df2c76b9fe4d3eb8612">>,
                                               <<"license">>,
                                               {ejson, ExpectedJson}) -> iodata
                end),
    ?assertEqual({keep_open, req, state, iodata},
                 deliv_hand_license_sse:format_event(license, {missing, LicenseData}, req, state)),
    ?verifyAll.
