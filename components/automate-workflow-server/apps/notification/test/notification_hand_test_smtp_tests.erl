-module(notification_hand_test_smtp_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

from_json_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "from_json", setup, teardown).

setup() ->
    #handler{user_name = <<"hankventure">>, ent_name = <<"HankCo">>}.

teardown(_) ->
    ok.

allowed_methods_allows_POST_test() ->
    ?assertEqual({[<<"POST">>], req, #handler{}},
                 notification_hand_test_smtp:allowed_methods(req, #handler{})).

content_types_accepted_provides_json_test() ->
    hoax:test(fun() ->
        hoax:mock(deliv_web_utils,
                    ?expect(content_type_json_map,
                          ?withArgs([from_json]),
                          ?andReturn(expected_map))),

        Actual = notification_hand_test_smtp:content_types_accepted(req, #handler{}),

        ?assertEqual({expected_map, req, #handler{}}, Actual),
        ?verifyAll
    end).

from_json_returns_200_on_success(#handler{ent_name = EntName,
                                          user_name = UserName} = State) ->

    hoax:mock(deliv_web_utils,
              ?expect(parse_json_req,
                      ?withArgs([req, notification_smtp_config]),
                      ?andReturn({smtp_config, req}))),

    hoax:mock(notification_smtp_test,
              ?expect(send,
                      ?withArgs([EntName, UserName, smtp_config]),
                      ?andReturn(<<"Great success\r\n">>))),

    Result = notification_hand_test_smtp:from_json(req, State),

    ?assertEqual({true, req, State}, Result),
    ?verifyAll.

from_json_returns_400_bad_request_for_incomplete_json(State) ->
    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, notification_smtp_config]),
                      ?andReturn({{error, bad_request}, req1})),
              ?expect(error_response,
                      ?withArgs([400, bad_request, req1, State]),
                      ?andReturn({error, bad_request}))]),

    Result = notification_hand_test_smtp:from_json(req, State),

    ?assertEqual({error, bad_request}, Result),
    ?verifyAll.

from_json_returns_412_when_user_not_found(
        #handler{ent_name = EntName, user_name = UserName} = State) ->

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, notification_smtp_config]),
                      ?andReturn({smtp_config, req1})),
              ?expect(error_response,
                      ?withArgs([412, precondition_failed, req1, State]),
                      ?andReturn({halt, req2, State}))
              ]),

    hoax:mock(notification_smtp_test,
              ?expect(send,
                      ?withArgs([EntName, UserName, smtp_config]),
                      ?andReturn({error, user_not_found}))),

    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([notification_smtp_test, send,
                                 [EntName, UserName, smtp_config],
                                 {error, user_not_found}]))),

    Result = notification_hand_test_smtp:from_json(req, State),

    ?assertEqual({halt, req2, State}, Result),
    ?verifyAll.

from_json_returns_412_when_user_fetch_fails(
        #handler{ent_name = EntName, user_name = UserName} = State) ->

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, notification_smtp_config]),
                      ?andReturn({smtp_config, req1})),
              ?expect(error_response,
                      ?withArgs([412, precondition_failed, req1, State]),
                      ?andReturn({halt, req2, State}))
              ]),

    hoax:mock(notification_smtp_test,
              ?expect(send,
                      ?withArgs([EntName, UserName, smtp_config]),
                      ?andReturn({error, user_not_found, oops}))),

    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([notification_smtp_test, send,
                                 [EntName, UserName, smtp_config],
                                 {error, user_not_found, oops}]))),

    Result = notification_hand_test_smtp:from_json(req, State),

    ?assertEqual({halt, req2, State}, Result),
    ?verifyAll.

from_json_returns_412_when_user_email_is_missing(
        #handler{ent_name = EntName, user_name = UserName} = State) ->

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, notification_smtp_config]),
                      ?andReturn({smtp_config, req1})),
              ?expect(error_response,
                      ?withArgs([412, precondition_failed, req1, State]),
                      ?andReturn({halt, req2, State}))
              ]),

    hoax:mock(notification_smtp_test,
              ?expect(send,
                      ?withArgs([EntName, UserName, smtp_config]),
                      ?andReturn({error, no_user_email}))),

    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([notification_smtp_test, send,
                                 [EntName, UserName, smtp_config],
                                 {error, no_user_email}]))),

    Result = notification_hand_test_smtp:from_json(req, State),

    ?assertEqual({halt, req2, State}, Result),
    ?verifyAll.

from_json_returns_502_when_a_connection_cannot_be_established(
        #handler{ent_name = EntName, user_name = UserName} = State) ->

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, notification_smtp_config]),
                      ?andReturn({smtp_config, req1})),
              ?expect(error_response,
                      ?withArgs([502, bad_gateway, req1, State]),
                      ?andReturn({halt, req2, State}))
              ]),

    hoax:mock(notification_smtp_test,
              ?expect(send,
                      ?withArgs([EntName, UserName, smtp_config]),
                      ?andReturn({error, error_type, error_message}))),

    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([notification_smtp_test, send,
                                 [EntName, UserName, smtp_config],
                                 {error, error_type, error_message}]))),

    Result = notification_hand_test_smtp:from_json(req, State),

    ?assertEqual({halt, req2, State}, Result),
    ?verifyAll.


from_json_returns_500_when_a_message_cannot_be_sent(
        #handler{ent_name = EntName, user_name = UserName} = State) ->

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, notification_smtp_config]),
                      ?andReturn({smtp_config, req1})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req1, State]),
                      ?andReturn({halt, req2, State}))
              ]),

    hoax:mock(notification_smtp_test,
              ?expect(send,
                      ?withArgs([EntName, UserName, smtp_config]),
                      ?andReturn({error, uhoh}))),

    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([notification_smtp_test, send,
                                 [EntName, UserName, smtp_config],
                                 {error, uhoh}]))),

    Result = notification_hand_test_smtp:from_json(req, State),

    ?assertEqual({halt, req2, State}, Result),
    ?verifyAll.
