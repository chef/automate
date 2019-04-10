-module(vis_hand_timeseries_converge_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

vis_hand_node_state_count_test_() ->
    [
     eunit_sugar:fixture(?MODULE, "input_json_validation", setup, teardown)
    ].

setup() ->
    vis_json:init_schemas().

teardown(_) ->
    ok.

input_json_validation_fails_with_empty_request_json() ->
    RequestJson = {[]},
    Actual = chef_json:validate(filter_group_by_status_range_with_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, missing_required_property, _, _}]}, Actual).

input_json_validation_passes_with_range_specified() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "range.json"),
    Actual = chef_json:validate(filter_group_by_status_range_with_interval, RequestJson),
    ?assertEqual(ok, Actual).

input_json_validation_fails_with_range_partially_specified() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "range_partial.json"),
    Actual = chef_json:validate(filter_group_by_status_range_with_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, missing_required_property, _, _}]}, Actual).

input_json_validation_passes_with_fully_populated_request() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "range_filter_and_group_by.json"),
    Actual = chef_json:validate(filter_group_by_status_range_with_interval, RequestJson),
    ?assertEqual(ok, Actual).
