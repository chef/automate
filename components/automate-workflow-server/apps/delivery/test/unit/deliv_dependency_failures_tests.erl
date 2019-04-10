-module(deliv_dependency_failures_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

%% Above each test is an example of the tested behaviour. Capital letters
%% represent pipeline ids. Sets of pipeline ids are grouped with (). Failing
%% pipelines are indicated with x (e.g., Ax means pipeline with id A failed).

handle_call_get_blocked_project_names_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "handle_call_get_blocked_project_names_", setup, teardown).

basic_setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup().

setup() ->
    basic_setup(),
    Pipeline1 = eu_data:with_enterprise(<<"test_enterprise">>,
       eu_data:with_organization(<<"test_organization">>,
        eu_data:with_project(<<"test_project_1">>,
          eu_data:with_pipeline(<<"master">>,
            fun(_Enterprise, _Organization, _Project, Pipeline) ->
                Pipeline
            end)))),
    Pipeline2 = eu_data:with_enterprise(<<"test_enterprise">>,
       eu_data:with_organization(<<"test_organization">>,
        eu_data:with_project(<<"test_project_2">>,
          eu_data:with_pipeline(<<"master">>,
            fun(_Enterprise, _Organization, _Project, Pipeline) ->
                Pipeline
            end)))),
    Pipeline3 = eu_data:with_enterprise(<<"test_enterprise">>,
       eu_data:with_organization(<<"test_organization">>,
        eu_data:with_project(<<"test_project_3">>,
          eu_data:with_pipeline(<<"master">>,
            fun(_Enterprise, _Organization, _Project, Pipeline) ->
                Pipeline
            end)))),
    {Pipeline1, Pipeline2, Pipeline3}.

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

deliv_dependency_failure_start_link_test() ->
    error_logger:tty(false),
    application:start(gproc),
    {ok, Pid} = deliv_dependency_failures:start_link(),
    unlink(Pid),
    timer:sleep(10),
    ?assertEqual(true, erlang:is_process_alive(Pid)),
    erlang:exit(Pid, shutdown),
    application:stop(gproc),
    error_logger:tty(true).

expect_pipeline_blocked_event(PipeId, Status) ->
    hoax:mock(deliv_event,
              ?expect(publish,
                      ?withArgs([{pipeline_blocked, PipeId}, Status]))
    ).

handle_call_is_blocked_delegates_to_impl_test() ->
    hoax:test(fun() ->
        BlockedMaps = [ #{1 => failed, 2 => passed}, #{4 => failed}],
        hoax:mock(deliv_pipeline,
                  ?expect(scoping_names,
                        ?withArgs([3]),
                        ?andReturn([ent_name, org_name, proj_name]))),
        hoax:mock(deliv_dependency_failures_db,
                  ?expect(fetch, ?withArgs([ent_name]),
                          ?andReturn({ok, BlockedMaps}))),
        hoax:mock(deliv_dependency_failures_impl,
                  ?expect(is_pipeline_blocked, ?withArgs([3, BlockedMaps]),
                  ?andReturn(true))),

        Actual = deliv_dependency_failures:handle_call({is_blocked, 3}, self(), no_state),

        ?assertEqual({reply, true, no_state}, Actual),
        ?verifyAll
    end).

handle_call_get_blocked_project_names_returns_error_not_found_for_bad_ent_name(_) ->
    EntName = <<"noop_enterprise">>,
    Actual = deliv_dependency_failures:handle_call({get_blocked_project_names, EntName}, self(), no_state),
    ?assertEqual({reply, {error, not_found}, no_state}, Actual),
    ?verifyAll.

handle_call_get_blocked_project_names_returns_list_of_binaries_found({Pipeline1, Pipeline2, Pipeline3}) ->
  Pipeline1Id = deliv_pipeline:getval(id, Pipeline1),
  Pipeline2Id = deliv_pipeline:getval(id, Pipeline2),
  Pipeline3Id = deliv_pipeline:getval(id, Pipeline3),
  BlockedMaps = [ #{Pipeline1Id => failed, Pipeline2Id => passed}, #{Pipeline3Id => failed} ],
  hoax:mock(deliv_dependency_failures_db,
            ?expect(fetch, ?withArgs([<<"test_enterprise">>]),
                    ?andReturn({ok, BlockedMaps}))),
  hoax:mock(deliv_dependency_failures_impl,
            ?expect(get_all_blocked_pipeline_ids, ?withArgs([BlockedMaps]),
            ?andReturn([Pipeline1Id, Pipeline2Id, Pipeline3Id]))),

  Actual = deliv_dependency_failures:handle_call({get_blocked_project_names, <<"test_enterprise">>}, self(), no_state),
  ?assertEqual({reply, {ok, [<<"test_project_3">>, <<"test_project_2">>, <<"test_project_1">>]}, no_state}, Actual).

handle_call_get_blocked_project_names_returns_only_valid_projects_when_pipeline_does_not_exist({Pipeline1, Pipeline2, Pipeline3}) ->
    Pipeline1Id = deliv_pipeline:getval(id, Pipeline1),
    Pipeline2Id = deliv_pipeline:getval(id, Pipeline2),
    Pipeline3Id = deliv_pipeline:getval(id, Pipeline3),
    BlockedMaps = [ #{Pipeline1Id => failed, Pipeline2Id => passed}, #{Pipeline3Id => failed} ],

    % remove pipeline 2
    ok = deliv_pipeline:delete(Pipeline2),

    hoax:mock(deliv_dependency_failures_db,
            ?expect(fetch, ?withArgs([<<"test_enterprise">>]),
                    ?andReturn({ok, BlockedMaps}))),
    hoax:mock(deliv_dependency_failures_impl,
            ?expect(get_all_blocked_pipeline_ids, ?withArgs([BlockedMaps]),
            ?andReturn([Pipeline1Id, Pipeline2Id, Pipeline3Id]))),

    Actual = deliv_dependency_failures:handle_call({get_blocked_project_names, <<"test_enterprise">>}, self(), no_state),
    %% Does not return project 2 since it's pipeline was deleted.
    ?assertEqual({reply, {ok, [<<"test_project_3">>, <<"test_project_1">>]}, no_state}, Actual),
    ?verifyAll.

handle_call_get_blocked_project_names_returns_only_valid_projects_when_project_does_not_exist({Pipeline1, Pipeline2, Pipeline3}) ->
    Pipeline1Id = deliv_pipeline:getval(id, Pipeline1),
    Pipeline2Id = deliv_pipeline:getval(id, Pipeline2),
    Pipeline3Id = deliv_pipeline:getval(id, Pipeline3),
    BlockedMaps = [ #{Pipeline1Id => failed, Pipeline2Id => passed}, #{Pipeline3Id => failed} ],

    % remove project 2
    ok = deliv_db:delete(deliv_project, [<<"test_enterprise">>, <<"test_organization">>], <<"test_project_2">>),

    hoax:mock(deliv_dependency_failures_db,
            ?expect(fetch, ?withArgs([<<"test_enterprise">>]),
                    ?andReturn({ok, BlockedMaps}))),
    hoax:mock(deliv_dependency_failures_impl,
            ?expect(get_all_blocked_pipeline_ids, ?withArgs([BlockedMaps]),
            ?andReturn([Pipeline1Id, Pipeline2Id, Pipeline3Id]))),

    Actual = deliv_dependency_failures:handle_call({get_blocked_project_names, <<"test_enterprise">>}, self(), no_state),
    %% Does not return project 2 since it does not exist.
    ?assertEqual({reply, {ok, [<<"test_project_3">>, <<"test_project_1">>]}, no_state}, Actual),
    ?verifyAll.

handle_info_union_finished_fixture_test_() ->
    hoax:fixture(?MODULE, handle_info_union_finished).

handle_info_union_finished_when_merged_are_dependencies_blocked_publishes_event_with_true() ->
    A = 1,
    B = 2,
    C = 3,
    New = #{A => passed},
    Current = [#{A => failed, B => failed}, #{C => failed}],
    Merged = #{A => passed, B => failed},
    Disjoint = [#{C => failed}],
    IsMergedSetBlocked = true,
    hoax:mock(deliv_pipeline,
              ?expect(scoping_names,
                     ?withArgs([A]),
                     ?andReturn([ent_name, org_name, proj_name]))),
    hoax:mock(deliv_dependency_failures_impl, [
              ?expect(merge_overlapping_sets,
                      ?withArgs([New, Current]),
                      ?andReturn({Merged, Disjoint})),
              ?expect(are_dependencies_blocked,
                      ?withArgs([Merged]),
                      ?andReturn(IsMergedSetBlocked))
    ]),
    expect_pipeline_blocked_event(A, IsMergedSetBlocked),
    expect_pipeline_blocked_event(B, IsMergedSetBlocked),
    hoax:mock(deliv_dependency_failures_db, [
              ?expect(fetch, ?withArgs([ent_name]),
                      ?andReturn({ok, Current})),
              ?expect(insert, ?withArgs([ ent_name, [Merged | Disjoint] ]))
    ]),

    {noreply, no_state} = deliv_dependency_failures:handle_info({self(), union_finished, New}, no_state),

    ?verifyAll.

handle_info_union_finished_when_merged_dependencies_are_not_blocked_publishes_event_with_false_and_drops_merged_set() ->
    A = 1,
    B = 2,
    C = 3,
    New = #{A => passed, B => passed},
    Current = [#{A => failed, B => failed}, #{C => failed}],
    Merged = #{A => passed, B => passed},
    Disjoint = [#{C => failed}],
    IsMergedSetBlocked = false,
    hoax:mock(deliv_pipeline,
              ?expect(scoping_names,
                     ?withArgs([A]),
                     ?andReturn([ent_name, org_name, proj_name]))),
    hoax:mock(deliv_dependency_failures_impl, [
              ?expect(merge_overlapping_sets,
                      ?withArgs([New, Current]),
                      ?andReturn({Merged, Disjoint})),
              ?expect(are_dependencies_blocked,
                      ?withArgs([Merged]),
                      ?andReturn(IsMergedSetBlocked))
    ]),
    expect_pipeline_blocked_event(A, IsMergedSetBlocked),
    expect_pipeline_blocked_event(B, IsMergedSetBlocked),
    hoax:mock(deliv_dependency_failures_db, [
              ?expect(fetch, ?withArgs([ent_name]),
                      ?andReturn({ok, Current})),
              ?expect(insert, ?withArgs([ent_name, Disjoint ]))
    ]),

    {noreply, no_state} = deliv_dependency_failures:handle_info({self(), union_finished, New}, no_state),

    ?verifyAll.
