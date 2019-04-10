-module (deliv_dependency_failures_impl_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% Above each test is an example of the tested behaviour. Capital letters
%% represent pipeline ids. Sets of pipeline ids are grouped with (). Failing
%% pipelines are indicated with x (e.g., Ax means pipeline with id A failed).

%% (Ax)
merge_overlapping_sets_when_current_is_empty_test() ->
    New = #{1 => failed},
    Current = [],

    {New, Current} = deliv_dependency_failures_impl:merge_overlapping_sets(New, Current).

%% Input-> Set Result
%% (Ax) -> (Ax)
%% (A)  -> ()
merge_overlapping_sets_when_pipeline_failure_is_remediated_test() ->
    New = #{1 => passed},
    Current = [ #{1 => failed} ],

    {ActualMerged, ActualDisjoint} = deliv_dependency_failures_impl:merge_overlapping_sets(New, Current),

    ?assertEqual(#{1 => passed}, ActualMerged),
    ?assertEqual([], ActualDisjoint).

%% Input    -> Set Result
%% (Ax, Bx) -> (Ax, Bx)
%% (Cx, Dx) -> (Ax, Bx), (Cx, Dx)
%% (C, D)   -> (Ax, Bx) : test A and B still blocked
merge_overlapping_sets_when_disjoint_blocked_set_clears_test() ->
    A = 1,
    B = 2,
    C = 3,
    D = 4,
    New = #{C => passed, D => passed},
    Current = [ #{A => failed, B => failed}, #{C => failed, D => failed} ],

    {ActualMerged, ActualDisjoint} = deliv_dependency_failures_impl:merge_overlapping_sets(New, Current),

    ?assertEqual(New, ActualMerged),
    ?assertEqual([#{A => failed,  B => failed}], ActualDisjoint).

%% Input   -> Set Result
%% (A, Bx) -> (A, Bx)
%% (B, Cx) -> (A, B, Cx)
merge_overlapping_sets_when_blocked_set_expanded_test() ->
    A = 1,
    B = 2,
    C = 3,
    New = #{B => passed, C => failed},
    Current = [ #{A => passed, B => failed} ],

    {ActualMerged, ActualDisjoint} = deliv_dependency_failures_impl:merge_overlapping_sets(New, Current),

    ?assertEqual(#{A => passed, B => passed, C => failed}, ActualMerged),
    ?assertEqual([], ActualDisjoint).

%% Input   -> Set Result
%% (A, Bx) -> (A, Bx)
%% (B, Cx) -> (A, B, Cx)
%% (C)     -> () : test no pipelines in blocked set
merge_overlapping_sets_when_entire_blocked_set_clears_test() ->
    A = 1,
    B = 2,
    C = 3,
    New = #{C => passed},
    Current = [ #{A => passed, B => passed, C => failed} ],

    {ActualMerged, ActualDisjoint} = deliv_dependency_failures_impl:merge_overlapping_sets(New, Current),

    ?assertEqual(#{A => passed, B => passed, C => passed}, ActualMerged),
    ?assertEqual([], ActualDisjoint).

%% Input   -> Set Result
%% (A, Bx) -> (A, Bx)
%% (C, Dx) -> (A, Bx), (C, Dx)
%% (Bx, Dx) -> (A, Bx, C, Dx)
merge_overlapping_sets_when_merged_set_test() ->
    A = 1,
    B = 2,
    C = 3,
    D = 4,
    New = #{B => failed, D => failed},
    Current = [#{A => passed, B => failed}, #{C => passed, D => failed}],

    {ActualMerged, ActualDisjoint} = deliv_dependency_failures_impl:merge_overlapping_sets(New, Current),

    ?assertEqual(#{A => passed, B => failed, C => passed, D => failed}, ActualMerged),
    ?assertEqual([], ActualDisjoint).

%% Input   -> Set Result
%% (A, Bx) -> (A, Bx)
%% (C, Dx) -> (A, Bx), (C, Dx)
%% (Bx, Dx) -> (A, Bx, C, Dx)
%% (A, B)  -> (A, B, C, Dx)
merge_overlapping_sets_when_set_partially_clears_test() ->
    A = 1,
    B = 2,
    C = 3,
    D = 4,
    New = #{A => passed, B => passed},
    Current = [#{A => passed, B => failed, C => passed, D => failed}],

    {ActualMerged, ActualDisjoint} = deliv_dependency_failures_impl:merge_overlapping_sets(New, Current),

    ?assertEqual(#{A => passed, B => passed, C => passed, D => failed}, ActualMerged),
    ?assertEqual([], ActualDisjoint).

are_dependencies_blocked_returns_true_when_any_element_status_is_failed_test() ->
    Result = deliv_dependency_failures_impl:are_dependencies_blocked(#{1 => passed, 2 => failed}),
    ?assertEqual(true, Result).

are_dependencies_blocked_returns_true_when_any_element_status_is_unknown_test() ->
    Result = deliv_dependency_failures_impl:are_dependencies_blocked(#{1 => passed, 2 => unknown}),
    ?assertEqual(true, Result).

are_dependencies_blocked_returns_false_when_all_element_statuses_are_passed_test() ->
    Result = deliv_dependency_failures_impl:are_dependencies_blocked(#{1 => passed, 2 => passed}),
    ?assertEqual(false, Result).

is_pipeline_blocked_returns_true_when_pipeline_id_is_in_a_blocked_map_test() ->
    BlockedMaps = [ #{1 => failed, 2 => passed}, #{3 => unknown, 4 => failed}],

    Actual = deliv_dependency_failures_impl:is_pipeline_blocked(3, BlockedMaps),

    ?assertEqual(true, Actual).

is_pipeline_blocked_returns_false_when_pipeline_id_is_not_in_a_blocked_map_test() ->
    BlockedMaps = [ #{1 => failed, 2 => passed}, #{8 => unknown, 4 => failed}],

    Actual = deliv_dependency_failures_impl:is_pipeline_blocked(3, BlockedMaps),

    ?assertEqual(false, Actual).

is_pipeline_blocked_returns_false_when_no_blocked_sets_exist_test() ->
    BlockedMaps = [],

    Actual = deliv_dependency_failures_impl:is_pipeline_blocked(3, BlockedMaps),

    ?assertEqual(false, Actual).

get_all_blocked_pipeline_ids_when_single_set_test() ->
  BlockedMaps = [ #{1 => failed, 2 => passed} ],

  Actual = deliv_dependency_failures_impl:get_all_blocked_pipeline_ids(BlockedMaps),
  ?assertEqual([1,2], lists:sort(Actual)).

get_all_blocked_pipeline_ids_when_multiple_sets_test() ->
  BlockedMaps = [ #{1 => failed, 2 => passed},
                  #{3 => failed, 4 => passed}],

  Actual = deliv_dependency_failures_impl:get_all_blocked_pipeline_ids(BlockedMaps),
  ?assertEqual([1,2,3,4], lists:sort(Actual)).

get_all_blocked_pipeline_ids_when_empty_set() ->
  BlockedMaps = [ ],

  Actual = deliv_dependency_failures_impl:get_all_blocked_pipeline_ids(BlockedMaps),
  ?assertEqual([], Actual).
