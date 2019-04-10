-module(vis_query_filter_builder_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

vis_query_filter_builder_test_() ->
    [
     eunit_sugar:fixture(?MODULE, "build_filter_json_"),
     eunit_sugar:fixture(?MODULE, "add_minimum_should_match_if_should_filter_")
    ].

build_filter_json_when_json_is_empty_the_es_json_should_not_be_modified() ->
    InitialESJson = vis_test_helpers:open_json_fixture("query_filter", "standard_es.json"),
    RequestJson = {[]},
    ?assertEqual(InitialESJson,
                 vis_query_filter_builder:append_to_existing_bool_filters(RequestJson,
                                                                          InitialESJson)).

build_filter_json_when_json_has_single_raw() ->
    InitialESJson = vis_test_helpers:open_json_fixture("query_filter", "standard_es.json"),
    RequestJson = vis_test_helpers:open_json_fixture("query_filter", "must_request.json"),
    ResultJson = vis_test_helpers:open_json_fixture("query_filter", "must_result.json"),
    ?assertEqual(ResultJson,
                 vis_query_filter_builder:append_to_existing_bool_filters(RequestJson,
                                                                          InitialESJson)).

build_filter_json_when_json_has_multiple_raw() ->
    InitialESJson = vis_test_helpers:open_json_fixture("query_filter", "standard_es.json"),
    RequestJson = vis_test_helpers:open_json_fixture("query_filter", "must_multiple_request.json"),
    ResultJson = vis_test_helpers:open_json_fixture("query_filter", "must_multiple_result.json"),
    ?assertEqual(ResultJson,
                 vis_query_filter_builder:append_to_existing_bool_filters(RequestJson,
                                                                          InitialESJson)).

build_filter_json_when_json_has_multiple_must_and_should_raw_and_filter() ->
    InitialESJson = vis_test_helpers:open_json_fixture("query_filter", "standard_es.json"),
    RequestJson = vis_test_helpers:open_json_fixture("query_filter", "must_and_should_request.json"),
    ResultJson = vis_test_helpers:open_json_fixture("query_filter", "must_and_should_result.json"),
    ?assertEqual(ResultJson,
                 vis_query_filter_builder:append_to_existing_bool_filters(RequestJson,
                                                                          InitialESJson)).

add_minimum_should_match_if_should_filter_when_no_should_filter_no_minimum_should_match() ->
    ?assertEqual(json,
                 vis_query_filter_builder:add_minimum_should_match_if_should_filter(undefined,
                                                                          json)).

add_minimum_should_match_if_should_filter_when_empty_should_filter_no_minimum_should_match() ->
  ?assertEqual(json,
               vis_query_filter_builder:add_minimum_should_match_if_should_filter([],
                                                                        json)).

add_minimum_should_match_if_should_filter_when_should_filter_no_minimum_should_match() ->
    WithMinShouldMatch = vis_test_helpers:open_json_fixture("query_filter", "must_and_should_result.json"),
    WithoutMinShouldMatch = vis_test_helpers:open_json_fixture("query_filter", "must_and_should_without_minimum_should_match.json"),
    ShouldFilter = [{[{<<"term">>,
                        {[{<<"cookbooks.raw">>,
                            <<"cookbook_name_2">>}]}}]}],
    ?assertEqual(WithMinShouldMatch,
                 vis_query_filter_builder:add_minimum_should_match_if_should_filter(ShouldFilter,
                                                                          WithoutMinShouldMatch)).
