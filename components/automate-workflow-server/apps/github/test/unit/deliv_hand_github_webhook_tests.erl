-module(deliv_hand_github_webhook_tests).

-include_lib("delivery/include/deliv_types.hrl").
-include("../../src/deliv_github_event_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

init_returns_correct_tuple_test() ->
    Actual = deliv_hand_github_webhook:init(ignored, req, state),

    Expected = {upgrade, protocol, cowboy_rest, req, state},

    ?assertEqual(Expected, Actual).

rest_init_returns_correct_tuple_test() ->
    Actual = deliv_hand_github_webhook:rest_init(req, state),

    Expected = {ok, req, state},

    ?assertEqual(Expected, Actual).

allowed_methods_allows_only_POST_test() ->
    ?assertEqual({[<<"POST">>], req, state},
                 deliv_hand_github_webhook:allowed_methods(req, state)).

content_types_accepted_accepts_json_test() ->
    hoax:test(fun() ->
                      hoax:mock(deliv_web_utils,
                                ?expect(content_type_json_map,
                                        ?withArgs([handle]),
                                        ?andReturn(expected_map))),

                      Actual = deliv_hand_github_webhook:content_types_accepted(req, state),

                      ?assertEqual({expected_map, req, state}, Actual),

                      ?verifyAll
              end).

handle_fixture_test_() ->
    hoax:fixture(?MODULE, "handle_").

handle_returns_422_when_no_ghe_header() ->
    hoax:mock(cowboy_req,
              ?expect(header,
                      ?withArgs([<<"x-github-event">>, req]),
                      ?andReturn({undefined, req1}))),

    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([422, missing_header, req1, state]),
                      ?andReturn(expected_response))),

    Actual = deliv_hand_github_webhook:handle(req, state),

    ?assertEqual(expected_response, Actual),

    ?verifyAll.


handle_returns_422_when_json_is_invalid() ->
    hoax:mock(cowboy_req, [
                           ?expect(header,
                                   ?withArgs([<<"x-github-event">>, req]),
                                   ?andReturn({<<"pull_request">>, req1}))
                          ]),
    hoax:mock(chef_json,
              ?expect(decode,
                      ?withArgs([request_body]),
                      ?andReturn({error, invalid_json}))),

    hoax:mock(deliv_web_utils, [
              ?expect(error_response,
                      ?withArgs([422, invalid_json, req2, state]),
                      ?andReturn(expected_response)),
              ?expect(read_body,
                      ?withArgs([?any]),
                      ?andReturn({request_body, req2}))
              ]),

    Actual = deliv_hand_github_webhook:handle(req, state),

    ?assertEqual(expected_response, Actual),
    ?verifyAll.


handle_returns_422_when_event_unrecognized() ->
    hoax:mock(ej,
              ?expect(get,
                      ?withArgs([[<<"action">>], json_payload]),
                      ?andReturn(<<"event_action">>))),

    hoax:mock(cowboy_req, [
                           ?expect(header,
                                   ?withArgs([<<"x-github-event">>, req]),
                                   ?andReturn({<<"totally_unknown">>, req1}))
                          ]),

    hoax:mock(chef_json,
              ?expect(decode,
                      ?withArgs([request_body]),
                      ?andReturn(json_payload))),

    hoax:mock(deliv_web_utils, [
                                ?expect(extract_proj_coordinates,
                                        ?withArgs([out_req]),
                                        ?andReturn({#proj_coordinates{ent_name = url_ent_name,
                                                                      org_name = url_org_name,
                                                                      proj_name = url_proj_name},
                                                    req2})),
                                ?expect(error_response,
                                        ?withArgs([422, undefined_event_type, req2, state]),
                                        ?andReturn(expected_response)),
                                ?expect(read_body,
                                     ?withArgs([req1]),
                                     ?andReturn({request_body, out_req}))
                               ]),

    Actual = deliv_hand_github_webhook:handle(req, state),

    ?assertEqual(expected_response, Actual),
    ?verifyAll.

-define(COORDINATES,#proj_coordinates{ent_name = url_ent_name, org_name = url_org_name, proj_name = url_proj_name}).

when_event_type_is_pull_request_test_() ->
    hoax:fixture(?MODULE, when_event_type_is_pull_request, when_event_type_is_pull_request_setup, when_event_type_is_pull_request_teardown).

when_event_type_is_pull_request_setup() ->
    hoax:mock(deliv_web_utils,
              ?expect(extract_proj_coordinates,
                     ?withArgs([out_req]),
                     ?andReturn({?COORDINATES, req2}))),
    hoax:mock(cowboy_req, [
                           ?expect(header,
                                   ?withArgs([<<"x-github-event">>, req]),
                                   ?andReturn({<<"pull_request">>, req1}))
                          ]),
    hoax:mock(deliv_web_utils, ?expect(read_body,
                                   ?withArgs([req1]),
                                   ?andReturn({request_body, out_req}))),
    hoax:mock(chef_json,
              ?expect(decode,
                      ?withArgs([request_body]),
                      ?andReturn(json_payload))),

    ok.

when_event_type_is_pull_request_teardown(_) ->
    ok.

when_event_type_is_pull_request_and_action_is_closed() ->
    Event = #github_pr{action=closed, proj_coordinates = ?COORDINATES, payload = json_payload},

    hoax:mock(ej,
              ?expect(get,
                      ?withArgs([[<<"action">>], json_payload]),
                      ?andReturn(<<"closed">>))),

    hoax:mock(deliv_github_event,
              ?expect(handle,
                      ?withArgs([Event]),
                      ?andReturn({true, req2, state}))),

    Actual = deliv_hand_github_webhook:handle(req, state),
    ?assertEqual({true, req2, state}, Actual),
    ?verifyAll.

when_event_type_is_pull_request_and_action_is_opened() ->
    Event = #github_pr{action=opened, proj_coordinates = ?COORDINATES, payload = json_payload},

    hoax:mock(ej,
              ?expect(get,
                      ?withArgs([[<<"action">>], json_payload]),
                      ?andReturn(<<"opened">>))),

    hoax:mock(deliv_github_event,
              ?expect(handle,
                      ?withArgs([Event]),
                      ?andReturn({true, req2, state}))),

    Actual = deliv_hand_github_webhook:handle(req, state),
    ?assertEqual({true, req2, state}, Actual),
    ?verifyAll.

when_event_type_is_pull_request_and_action_is_synchronize() ->
    Event = #github_pr{action=synchronized, proj_coordinates = ?COORDINATES, payload = json_payload},

    hoax:mock(ej,
              ?expect(get,
                      ?withArgs([[<<"action">>], json_payload]),
                      ?andReturn(<<"synchronize">>))),

    hoax:mock(deliv_github_event,
              ?expect(handle,
                      ?withArgs([Event]),
                      ?andReturn({true, req2, state}))),

    Actual = deliv_hand_github_webhook:handle(req, state),
    ?assertEqual({true, req2, state}, Actual),
    ?verifyAll.

when_event_type_is_pull_request_and_action_is_anything_else_event_is_ignored() ->
    hoax:mock(ej,
              ?expect(get,
                      ?withArgs([[<<"action">>], json_payload]),
                      ?andReturn(<<"labeled">>))),

    hoax:mock(deliv_github_event, expect_no_interactions),

    Actual = deliv_hand_github_webhook:handle(req, state),
    ?assertEqual({true, req2, state}, Actual),
    ?verifyAll.

handle_ignores_ping() ->
    hoax:mock(ej,
              ?expect(get,
                      ?withArgs([[<<"action">>], json_payload]),
                      ?andReturn(undefined))),

    hoax:mock(deliv_web_utils, [
              ?expect(extract_proj_coordinates,
                     ?withArgs([out_req]),
                     ?andReturn({?COORDINATES, req2})),
              ?expect(read_body,
                      ?withArgs([req1]),
                      ?andReturn({request_body, out_req}))]),

    hoax:mock(cowboy_req, [
                           ?expect(header,
                                   ?withArgs([<<"x-github-event">>, req]),
                                   ?andReturn({<<"ping">>, req1}))
                          ]),

    hoax:mock(chef_json,
              ?expect(decode,
                      ?withArgs([request_body]),
                      ?andReturn(json_payload))),

    hoax:mock(deliv_github_event, expect_no_interactions),

    Actual = deliv_hand_github_webhook:handle(req, state),
    ?assertEqual({true, req2, state}, Actual),
    ?verifyAll.
