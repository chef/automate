-module(deliv_hand_phase_run_log_objects_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").
-include("deliv_phase_run_log.hrl").

-compile([export_all]).

from_json_fixture_test_() ->
    hoax:fixture(?MODULE, "from_json").

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, "to_json").

allowed_methods_allows_GET_and_POST_test() ->
    ?assertEqual({[<<"GET">>, <<"POST">>], req, state},
                 deliv_hand_phase_run_log_objects:allowed_methods(req, state)).

content_types_accepted_accepts_map_test() ->
    hoax:test(fun() ->
        hoax:mock(deliv_web_utils,
                  ?expect(content_type_json_or_any_map,
                          ?withArgs([from_json]),
                          ?andReturn(expected_map))),

        Actual = deliv_hand_phase_run_log_objects:content_types_accepted(req, state),

        ?assertEqual({expected_map, req, state}, Actual),
        ?verifyAll
    end).

content_types_provided_provides_json_test() ->
    ExpectedContentType = {[{{<<"application">>,<<"json">>,'*'},to_json}, {<<"*/*">>, to_json}], request, state},
    ?assertEqual(ExpectedContentType, deliv_hand_projects:content_types_provided(request, state)).

from_json_returns_400_bad_request_for_incomplete_data() ->
    hoax:mock(deliv_web_utils, [
              ?expect(read_body,
                      ?withArgs([req]),
                      ?andReturn({error, bad_request})),
              ?expect(error_response,
                      ?withArgs([400, bad_request, req, state]),
                      ?andReturn({halt, req1, state}))]),

    Actual = deliv_hand_phase_run_log_objects:from_json(req, state),

    ?assertEqual({halt, req1, state}, Actual),
    ?verifyAll.

from_json_returns_a_500_when_data_is_valid_but_there_is_an_error_saving_data() ->
    RunId = 1,
    Data = {[
        {<<"user">>, <<"someone">>}
        ]},

    hoax:mock(deliv_web_utils, [
              ?expect(read_body,
                      ?withArgs([req]),
                      ?andReturn({Data, req1})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, why, req2, state]),
                      ?andReturn({halt, req3, state}))]),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([run_id, req1]),
                      ?andReturn({RunId, req2}))),
    hoax:mock(deliv_phase_run_log,
              ?expect(save,
                      ?withArgs([RunId, Data]),
                      ?andReturn({error, why}))),

    Actual = deliv_hand_phase_run_log_objects:from_json(req, state),

    ?verifyAll,
    ?assertEqual({halt, req3, state}, Actual).

from_json_returns_a_201_when_saving_to_table_succeeds() ->
    RunId = 1,
    Data = {[
        {<<"user">>, <<"someone">>}
        ]},

    hoax:mock(deliv_web_utils, [
              ?expect(read_body,
                      ?withArgs([req]),
                      ?andReturn({Data, req1}))]),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([run_id, req1]),
                      ?andReturn({RunId, req2}))),
    hoax:mock(deliv_phase_run_log,
              ?expect(save,
                      ?withArgs([RunId, Data]),
                      ?andReturn({ok, deliv_phase_run_log_record}))),

    Actual = deliv_hand_phase_run_log_objects:from_json(req, state),

    ?assertEqual({true, req2, state}, Actual),
    ?verifyAll.

to_json_returns_500_error_when_fetch_log_fails() ->
    RunId = 1,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([run_id, req]),
                      ?andReturn({RunId, req1}))),
    hoax:mock(deliv_phase_run_log,
              ?expect(fetch,
                      ?withArgs([RunId]),
                      ?andReturn({error, why}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, why, req1, state]),
                      ?andReturn({halt, req2, state}))),

    Result = deliv_hand_phase_run_log_objects:handle(req, state),

    ?assertEqual({halt, req2, state}, Result),
    ?verifyAll.

to_json_returns_404_error_when_record_not_found() ->
    RunId = 1,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([run_id, req]),
                      ?andReturn({RunId, req1}))),
    hoax:mock(deliv_phase_run_log,
              ?expect(fetch,
                      ?withArgs([RunId]),
                      ?andReturn({ok, []}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([404, log_not_found, req1, state]),
                      ?andReturn({halt, req2, state}))),

    Result = deliv_hand_phase_run_log_objects:handle(req, state),

    ?assertEqual({halt, req2, state}, Result),
    ?verifyAll.

to_json_returns_ejson_if_record_is_found() ->
    Data = '{"profiles":"compliance"}',
    RunId = <<"1">>,
    Log = #deliv_phase_run_log{run_id = 1, data = Data},
    Ejson = [{[
              {<<"run_id">>, RunId},
              {<<"data">>, Data}
            ]}],
    hoax:mock(cowboy_req,
          ?expect(binding,
                  ?withArgs([run_id, req]),
                  ?andReturn({RunId, req1}))),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Ejson, req1, state]),
                      ?andReturn({body, req2, state}))),
    hoax:mock(deliv_phase_run_log,
              ?expect(fetch,
                      ?withArgs([RunId]),
                      ?andReturn({ok, [Log]}))),

    Result = deliv_hand_phase_run_log_objects:handle(req, state),

    ?assertEqual({body, req2, state}, Result),
    ?verifyAll.
