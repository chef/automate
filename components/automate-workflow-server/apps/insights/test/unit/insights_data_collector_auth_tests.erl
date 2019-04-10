-module(insights_data_collector_auth_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

insights_auth_check_fixture_test_() ->
    hoax:fixture(?MODULE, insights_auth_check).

insights_auth_check_and_publish_returns_204_when_header_includes_valid_token() ->
    Token = <<"valid_token">>,
    JsonBody = <<"{\"fake\":\"json\"}">>,
    BunnyServer = bunny_server,

    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([insights, data_collector_token]),
                      ?andReturn({ok, Token}))),
    hoax:mock(cowboy_req,
              ?expect(header,
                      ?withArgs([<<"x-data-collector-token">>, req]),
                      ?andReturn({Token, req2}))),
    hoax:mock(deliv_web_utils,
              ?expect(read_body,
                      ?withArgs([req2]),
                      ?andReturn({JsonBody, req3}))),
    hoax:mock(insights_ingester,
              ?expect(publish,
                      ?withArgs([JsonBody, <<"data-collector">>, BunnyServer]))),

    Actual = insights_data_collector_auth:insights_auth_check_and_publish(req, state, BunnyServer),
    ?assertEqual({halt, req3, state}, Actual),
    ?verifyAll.

insights_auth_check_and_publish_returns_401_when_header_includes_invalid_token() ->
    ValidToken = <<"valid_token">>,
    InvalidToken = <<"invalid_token">>,

    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([insights, data_collector_token]),
                      ?andReturn({ok, ValidToken}))),
    hoax:mock(cowboy_req,
              ?expect(header,
                      ?withArgs([<<"x-data-collector-token">>, req]),
                      ?andReturn({InvalidToken, req2}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([401, not_authorized, req2, state]),
                      ?andReturn(error))),

    Actual = insights_data_collector_auth:insights_auth_check_and_publish(req, state, bunny_server),
    ?assertEqual(error, Actual),
    ?verifyAll.

insights_auth_check_and_publish_returns_401_when_header_does_not_include_token() ->
    Token = <<"valid_token">>,

    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([insights, data_collector_token]),
                      ?andReturn({ok, Token}))),
    hoax:mock(cowboy_req,
              ?expect(header,
                      ?withArgs([<<"x-data-collector-token">>, req]),
                      ?andReturn({undefined, req}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([401, not_authorized, req, state]),
                      ?andReturn(error))),

    Actual = insights_data_collector_auth:insights_auth_check_and_publish(req, state, bunny_server),
    ?assertEqual(error, Actual),
    ?verifyAll.

insights_auth_check_and_publish_returns_500_when_read_body_fails() ->
    Token = <<"valid_token">>,

    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([insights, data_collector_token]),
                      ?andReturn({ok, Token}))),
    hoax:mock(cowboy_req,
              ?expect(header,
                      ?withArgs([<<"x-data-collector-token">>, req]),
                      ?andReturn({Token, req2}))),
    hoax:mock(deliv_web_utils,
              ?expect(read_body,
                      ?withArgs([req2]),
                      ?andReturn({error, foobar}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req2, state]),
                      ?andReturn(error))),

    Actual = insights_data_collector_auth:insights_auth_check_and_publish(req, state, bunny_server),
    ?assertEqual(error, Actual),
    ?verifyAll.
