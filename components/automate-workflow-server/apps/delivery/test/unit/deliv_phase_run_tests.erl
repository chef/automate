-module(deliv_phase_run_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("deliv_coordinates.hrl").

-compile([export_all]).

finish_with_log_append_test_() ->
   hoax:fixture(?MODULE, "finish_with_log_append_").

build_filename_returns_filename_for_phase_run_test() ->
    hoax:test(fun() ->
        Coords = #proj_coordinates{
                    ent_name = <<"testEnt">>,
                    org_name = <<"testOrg">>,
                    proj_name = <<"testProj">>
                  },
        Pipe = <<"testPipe">>,
        StageRun = deliv_stage_run:'#new'(),
        PhaseRun = deliv_phase_run:fromlist([
          {id, 123}, {phase, <<"unit">>}, {stage_run_id, 321}
        ]),

        hoax:mock(deliv_stage_run, [
                   ?expect(fetch,
                      ?withArgs([321]),
                      ?andReturn({ok, StageRun})),
                   ?expect(getval,
                      ?withArgs([stage, StageRun]),
                      ?andReturn(<<"verify">>))
                   ]),

        Filename = deliv_phase_run:build_filename(Coords, Pipe, PhaseRun),
        ExpectedFilename = <<"testOrg-testProj-testPipe-verify-unit-123.txt">>,
        ?assertEqual(Filename, ExpectedFilename),
        ?verifyAll
    end).

finish_with_log_append_returns_new_log_when_log_is_empty() ->
    NewLog = <<"My new message\n">>,
    PhaseRun = deliv_phase_run:fromlist([{id, phase_run_id}, {run_log, <<>>}]),

    hoax:expect(receive
                    deliv_db:fetch_by_id(deliv_phase_run, phase_run_id) -> {ok, PhaseRun};
                    calendar:universal_time() -> {{2017,4,18},{12,22,23}};
                    deliv_db:update([{finished, true},
                                     {finished_at, <<"2017-04-18T12:22:23Z">>},
                                     {status, <<"failed">>},
                                     {run_log, erlang:iolist_to_binary([<<"\n">>, NewLog])}],
                                    PhaseRun) -> db_response
                end),

    Actual = deliv_phase_run:finish_with_log_append(NewLog, PhaseRun),
    ?assertEqual(db_response, Actual),
    ?verifyAll.

finish_with_log_append_retrieves_existing_logs_and_appends_to_end_of_log() ->
    OldLog = <<"Old message\n">>,
    NewLog = <<"My new message\n">>,
    ConcatLog = erlang:iolist_to_binary([OldLog, <<"\n">>, NewLog]),
    PhaseRun = deliv_phase_run:fromlist([{id, phase_run_id}, {run_log, OldLog}]),

    hoax:expect(receive
                    deliv_db:fetch_by_id(deliv_phase_run, phase_run_id) -> {ok, PhaseRun};
                    calendar:universal_time() -> {{2017,4,18},{12,22,23}};
                    deliv_db:update([{finished, true},
                                     {finished_at, <<"2017-04-18T12:22:23Z">>},
                                     {status, <<"failed">>},
                                     {run_log, ConcatLog}],
                                    PhaseRun) -> db_response
                end),

    Actual = deliv_phase_run:finish_with_log_append(NewLog, PhaseRun),
    Expected = db_response,
    ?assertEqual(Expected, Actual),
    ?verifyAll.

finish_with_log_append_when_fetch_fails() ->
    NewLog = <<"My new message\n">>,
    PhaseRun = deliv_phase_run:fromlist([{id, phase_run_id}, {run_log, <<>>}]),

    hoax:expect(receive
                    deliv_db:fetch_by_id(deliv_phase_run, phase_run_id) -> {error, reason};
                    chef_log:error("Unable to fetch deliv_phase_run with: ~p", [reason]) -> ignored
                end),

    Actual = deliv_phase_run:finish_with_log_append(NewLog, PhaseRun),
    ?assertEqual({error, reason}, Actual),
    ?verifyAll.
