-module(vis_hand_count_cookbooks_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

vis_hand_node_state_count_test_() ->
    [
     eunit_sugar:fixture(?MODULE, "input_json_validation", setup, teardown),
     eunit_sugar:fixture(?MODULE, "allowed_methods_"),
     eunit_sugar:fixture(?MODULE, "content_types_accepted_")
    ].

setup() ->
    vis_json:init_schemas().

teardown(_) ->
    ok.

allowed_methods_is_post() ->
    ?assertEqual({[<<"POST">>], req, state}, vis_hand_count_nodes:allowed_methods(req, state)).

content_types_accepted_accepts_json() ->
    Actual = vis_hand_count_nodes:content_types_accepted(req, state),
    Expected = {[{{<<"application">>,<<"json">>,'*'}, from_json}], req, state},

    ?assertEqual(Expected, Actual).

input_json_validation_passes_with_empty_request_json() ->
    RequestJson = {[]},
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertEqual(ok, Actual).

input_json_validation_passes_with_empty_filters() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "empty_filters.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertEqual(ok, Actual).

input_json_validation_passes_with_empty_must_filter_array() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "empty_must_filter.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertEqual(ok, Actual).

input_json_validation_fails_when_must_is_an_object() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "must_is_an_object.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, wrong_type, _, _}]}, Actual).

input_json_validation_passes_when_must_contains_valid_data() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "must_contains_valid_data.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch(ok, Actual).

input_json_validation_passes_when_must_contains_empty_object() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "must_contains_empty_object.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch(ok, Actual).

input_json_validation_fails_when_must_contains_invalid_data() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "must_contains_invalid_data.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, wrong_type, _, _}]}, Actual).

input_json_validation_fails_when_must_contains_invalid_key() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "must_contains_invalid_key.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, no_extra_properties_allowed, _, _}]}, Actual).

input_json_validation_passes_with_empty_should_filter_array() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "empty_should_filter.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertEqual(ok, Actual).

input_json_validation_fails_when_should_is_an_object() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "should_is_an_object.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, wrong_type, _, _}]}, Actual).

input_json_validation_passes_when_should_contains_valid_data() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "should_contains_valid_data.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch(ok, Actual).

input_json_validation_passes_when_should_contains_empty_object() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "should_contains_empty_object.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch(ok, Actual).

input_json_validation_fails_when_should_contains_invalid_data() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "should_contains_invalid_data.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, wrong_type, _, _}]}, Actual).

input_json_validation_fails_when_should_contains_invalid_key() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "should_contains_invalid_key.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, no_extra_properties_allowed, _, _}]}, Actual).

input_json_validation_passes_when_must_and_should_contains_valid_data() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "must_and_should_contains_valid_data.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertEqual(ok, Actual).

input_json_validation_fails_when_must_contains_valid_data_but_should_does_not() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "must_valid_should_invalid.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, wrong_type, _, _}]}, Actual).

input_json_validation_passes_when_must_is_an_empty_array_and_should_contains_valid_data() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "must_empty_should_valid.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch(ok, Actual).

input_json_validation_valid_range_returns_ok() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "range_without_interval.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch(ok, Actual).

input_json_validation_error_when_lte_is_object() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "range_without_interval_value_is_object.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, wrong_type, _, _}]}, Actual).

input_json_validation_range_error_with_additional_key() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "range_without_interval_additional_key.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, no_extra_properties_allowed, _, _}]}, Actual).

input_json_validation_range_error_with_missing_key() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "range_without_interval_missing_key.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, wrong_type, _, _}]}, Actual).

input_json_validation_range_is_valid_with_other_filters() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "range_without_interval_with_must_and_should.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch(ok, Actual).

input_json_validation_fail_when_group_by_is_set_to_valid_key_and_no_filters() ->
    RequestJson = vis_test_helpers:open_json_fixture("schema", "group_by_with_filters.json"),
    Actual = chef_json:validate(filter_range_without_interval, RequestJson),
    ?assertMatch({error, [{data_invalid, _, no_extra_properties_allowed, _, _}]}, Actual).
