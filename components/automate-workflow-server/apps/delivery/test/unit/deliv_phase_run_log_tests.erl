-module(deliv_phase_run_log_tests).

-include_lib("hoax/include/hoax.hrl").
-include("deliv_types.hrl").
-include("deliv_phase_run_log.hrl").

-compile(export_all).

fixture_test_() ->
    hoax:fixture(?MODULE).

save_returns_an_error_when_db_call_fails() ->
    RunId = <<"98">>,
    Data = '{"profiles":"compliance"}',

    hoax:mock(deliv_db,
      ?expect(insert,
              ?withArgs([#deliv_phase_run_log{run_id=98, data=Data}]),
              ?andReturn({error, why}))),

    ?assertEqual({error, why}, deliv_phase_run_log:save(RunId, Data)),
    ?verifyAll.

save_returns_db_op_result() ->
    RunId = <<"98">>,
    Data = '{"profiles":"compliance"}',

    InsertedRec = [{id, 54}, {run_id, RunId}, {data, Data}],

    hoax:mock(deliv_db,
      ?expect(insert,
              ?withArgs([#deliv_phase_run_log{run_id=98, data=Data}]),
              ?andReturn([InsertedRec]))),

    ?assertEqual([InsertedRec], deliv_phase_run_log:save(RunId, Data)),
    ?verifyAll.

fetch_returns_an_error_when_db_call_fails() ->
    RunId = <<"98">>,
    RunId2 = 98,

    hoax:mock(deliv_db,
              ?expect(fetch2,
                    ?withArgs([deliv_phase_run_log, run_id, RunId2]),
                    ?andReturn({error, why}))),

    ?assertEqual({error, why}, deliv_phase_run_log:fetch(RunId)),
    ?verifyAll.

fetch_returns_list_of_objects_with_matching_run_id_given_a_run_id() ->
    RunId = <<"98">>,
    RunId2 = 98,
    PhaseRunLog = [],

    hoax:mock(deliv_db,
              ?expect(fetch2,
                    ?withArgs([deliv_phase_run_log, run_id, RunId2]),
                    ?andReturn({ok, [PhaseRunLog]}))),

    ?assertEqual({ok, [PhaseRunLog]}, deliv_phase_run_log:fetch(RunId)),
    ?verifyAll.
