-module(deliv_hand_phase_runs_named_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-compile(export_all).

fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, setup, teardown).

setup_change(Enterprise, Organization, Project, Pipeline, User, FeatureBranch) ->
    ChangePatchset = eu_data:create_patchset(Enterprise, User, Organization,
                                             Project, Pipeline, FeatureBranch),
    Change = eu_data:change_from_patchset(ChangePatchset),
    SubmittedAtChange = deliv_change:getval(submitted_at, Change),
    UpdatedChange = deliv_change:setvals([{submitted_at,
                                           chef_utils:trunc_timestamp(SubmittedAtChange)}], Change),
    UpdatedChange.

setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup(),
    eu_data:with_enterprise(<<"deliv_phase_test_enterprise">>,
      eu_data:with_organization(<<"deliv_phase_test_organization">>,
        eu_data:with_project(<<"deliv_phase_test_project">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_change_eunit_user">>),
                Change = setup_change(Enterprise, Organization, Project, Pipeline, User, "not_master"),
                [StageRun] = deliv_stage_run:insert([
                            {change_id, deliv_change:getval(id, Change)},
                            {stage, <<"build">>},
                            {status, <<"idle">>}]),
                StageRunId = deliv_stage_run:getval(id, StageRun),
                [PhaseRun] = deliv_phase_run:insert([{stage_run_id, StageRunId}]),

                PhaseRun
            end)))).

teardown(_) ->
    application:stop(gproc),
    eu_database:teardown(),
    error_logger:tty(true),
    ok.

handle_get_request_returns_a_tuple_with_the_return_req_and_state_if_handled_correctly(PhaseRun) ->
    PhaseRunIdBin = chef_utils:to_bin(deliv_phase_run:getval(id, PhaseRun)),
    StageRunIdBin = chef_utils:to_bin(deliv_phase_run:getval(stage_run_id, PhaseRun)),
    CreatedAtDbBin = chef_utils:format_db_timestamp(deliv_phase_run:getval(created_at, PhaseRun)),

    ExpectedReturn =  chef_utils:join_binaries([
                        <<"{\"id\":">>,
                        PhaseRunIdBin,
                        <<",\"stage_run_id\":">>,
                        StageRunIdBin, <<",\"phase\":null,\"status\":null,\"finished\":false,\"run_success\":null,\"run_log\":null,\"run_status\":null,\"build_node\":null,\"search_query\":null,\"search_description\":null,\"description\":null,\"created_at\":\"">>,
                        CreatedAtDbBin,
                        <<"\",\"started_at\":null,\"finished_at\":null}">>], <<>>),

    hoax:expect(receive
        cowboy_req:binding(run_id, req) -> {PhaseRunIdBin, req1};
        cowboy_req:method(req1) -> {<<"GET">>, req2}
                end),

    Result = deliv_hand_phase_runs_named:handle(req, state),
    ?assertEqual({ExpectedReturn, req2, state}, Result),
    ?verifyAll.

handle_post_request_returns_a_tuple_with_the_return_req_and_state_if_handled_correctly(PhaseRun) ->
    PhaseRunIdBin = chef_utils:to_bin(deliv_phase_run:getval(id, PhaseRun)),
    StageRunIdBin = chef_utils:to_bin(deliv_phase_run:getval(stage_run_id, PhaseRun)),
    CreatedAtDbBin = chef_utils:format_db_timestamp(deliv_phase_run:getval(created_at, PhaseRun)),

    Json = {[
                {<<"id">>,deliv_phase_run:getval(id, PhaseRun)},
                {<<"stage_run_id">>,deliv_phase_run:getval(stage_run_id, PhaseRun)},
                {<<"phase">>,undefined},
                {<<"status">>,<<"running">>},
                {<<"finished">>,false},
                {<<"run_success">>,undefined},
                {<<"run_log">>,undefined},
                {<<"run_status">>,undefined},
                {<<"build_node">>,undefined},
                {<<"search_query">>,undefined},
                {<<"search_description">>,undefined},
                {<<"description">>,undefined},
                {<<"created_at">>,CreatedAtDbBin},
                {<<"started_at">>,undefined},
                {<<"finished_at">>,undefined}
          ]},

    EncodedJson = chef_json:encode(Json),

    Data = chef_utils:join_binaries([
                        <<"{\"id\":">>,
                        PhaseRunIdBin,
                        <<",\"stage_run_id\":">>,
                        StageRunIdBin, <<",\"phase\":null,\"status\":null,\"finished\":false,\"run_success\":null,\"run_log\":null,\"run_status\":null,\"build_node\":null,\"search_query\":null,\"search_description\":null,\"description\":null,\"created_at\":\"">>,
                        CreatedAtDbBin,
                        <<"\",\"started_at\":null,\"finished_at\":null}">>], <<>>),

    Schema = {[{<<"properties">>, {[{<<"run_success">>, {[]}},
                                        {<<"run_log">>, {[]}},
                                        {<<"run_status">>, {[]}},
                                        {<<"run_complete">>, {[]}}
                                       ]}
                                     }]},

    hoax:expect(receive
        cowboy_req:binding(run_id, req) -> {PhaseRunIdBin, req1};
        cowboy_req:binding(ent_name, req3) -> {"deliv_phase_test_enterprise", req4};
        cowboy_req:binding(org_name, req4) -> {"deliv_phase_test_organization", req5};
        cowboy_req:binding(proj_name, req5) -> {"deliv_phase_test_project", req6};
        cowboy_req:binding(pipe_name, req6) -> {"master", req7};
        cowboy_req:method(req1) -> {<<"POST">>, req2};
        cowboy_req:body(req2) -> {ok, Data, req3};
        cowboy_req:set_resp_body(EncodedJson, req3) -> req4;
        jesse_database:load("phase_run") -> Schema
                end),

    Result = deliv_hand_phase_runs_named:handle(req, state),
    ?assertEqual({true, req4, state}, Result),
    ?verifyAll.
