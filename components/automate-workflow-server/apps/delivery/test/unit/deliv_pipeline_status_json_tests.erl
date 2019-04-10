-module(deliv_pipeline_status_json_tests).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

-define(REQUIRED_LIST,[
                       {<<"id">>, id},
                       {<<"title">>, title},
                       {<<"org">>, org},
                       {<<"project">>, project},
                       {<<"stage">>, stage},
                       {<<"stage_status">>, stage_status}
                      ]).

create_body_test_() ->
  hoax:fixture(?MODULE, create_body).

create_body_should_allow_undefined_fields_test() ->
  OptionalProplistKeys = [<<"submitter">>, <<"submitted_at">>,
                          <<"approved_by">>, <<"delivered_by">>
                         ],
  OptionalEmptyList = [{<<"includes">>, []}],
  ExpectedEjson = [{?REQUIRED_LIST ++ [{Key, undefined} ||
                                      Key <- OptionalProplistKeys]
                                  ++ OptionalEmptyList}],
  ?assertEqual(ExpectedEjson,
               deliv_pipeline_status_json:create_body([?REQUIRED_LIST], [])).

create_body_should_allow_null_timestamps_test() ->
  OptionalProplistKeys = [<<"submitter">>, <<"submitted_at">>,
                          <<"approved_by">>, <<"delivered_by">>
                         ],
  OptionalEmptyList = [{<<"includes">>, []}],
  Input = [{<<"submitted_at">>, null} | ?REQUIRED_LIST],
  ExpectedEjson = [{?REQUIRED_LIST ++ [{Key, undefined} ||
                                      Key <- OptionalProplistKeys]
                                  ++ OptionalEmptyList}],
  ?assertEqual(ExpectedEjson,
               deliv_pipeline_status_json:create_body([Input], [])).

create_body_should_fail_when_missing_required_fields_test() ->
  InputFieldsLists = [ {Key, proplists:delete(Key, ?REQUIRED_LIST)}
                       || {Key, _} <- ?REQUIRED_LIST],
  [
     ?assertError(
        {badarg, {missing_required_field, Key}},
        deliv_pipeline_status_json:create_body([InputList], []))
  || {Key, InputList} <- InputFieldsLists].


create_body_should_parse_time_test() ->
  OptionalProplistKeys = [
                          {<<"submitter">>, submitter},
                          {<<"submitted_at">>,{{2015,2,12},{21,26,48.943438}}},
                          {<<"approved_by">>, approved_by},
                          {<<"delivered_by">>, delivered_by}
                         ],
  ResultOptionalProplist = [
                          {<<"submitter">>, submitter},
                          {<<"submitted_at">>, <<"2015-02-12 21:26:48">>},
                          {<<"approved_by">>, approved_by},
                          {<<"delivered_by">>, delivered_by},
                          {<<"includes">>, []}
                         ],
  ExpectedEjson = [{?REQUIRED_LIST ++ ResultOptionalProplist}],
  InputProplist = ?REQUIRED_LIST ++ OptionalProplistKeys,
  ?assertEqual(ExpectedEjson,
               deliv_pipeline_status_json:create_body([InputProplist], [])
              ).

create_body_should_fail_with_bad_timestamp() ->
  OptionalProplistKeys = [
                          {<<"submitter">>, submitter},
                          {<<"submitted_at">>, invalid_timestamp},
                          {<<"approved_by">>, approved_by},
                          {<<"delivered_by">>, delivered_by}
                         ],
  InputProplist = ?REQUIRED_LIST ++ OptionalProplistKeys,
  ?assertError({badarg, {invalid_timestamp, invalid_timestamp}},
               deliv_pipeline_status_json:create_body([InputProplist], [])).

create_body_should_ignore_order_of_input_list_test() ->
  OptionalProplistKeys = [<<"submitter">>, <<"submitted_at">>,
                          <<"approved_by">>, <<"delivered_by">>
                         ],
  OptionalEmptyList = [{<<"includes">>, []}],
  ExpectedEjson = [{?REQUIRED_LIST ++ [{Key, undefined} ||
                                      Key <- OptionalProplistKeys]
                                  ++ OptionalEmptyList}],
  ?assertEqual(ExpectedEjson,
               deliv_pipeline_status_json:create_body([
                 lists:reverse(?REQUIRED_LIST)], []
                )
              ).

create_body_should_obey_empty_included_changes_test() ->
  OptionalProplistKeys = [<<"submitter">>, <<"submitted_at">>,
                          <<"approved_by">>, <<"delivered_by">>
                         ],
  OptionalEmptyList = [{<<"includes">>, []}],
  ExpectedEjson = [{?REQUIRED_LIST ++ [{Key, undefined} ||
                                      Key <- OptionalProplistKeys]
                                  ++ OptionalEmptyList}],
  ?assertEqual(ExpectedEjson,
               deliv_pipeline_status_json:create_body(
                 [lists:reverse(?REQUIRED_LIST)], []
                )
              ).

create_body_should_obey_included_changes_test() ->
  OptionalProplistKeys = [<<"submitter">>, <<"submitted_at">>,
                          <<"approved_by">>, <<"delivered_by">>
                         ],
  OptionalEjson = [{Key, undefined} || Key <- OptionalProplistKeys],
  IncludedChange = ?REQUIRED_LIST ++ OptionalEjson,
  ExpectedEjson = [{
                    ?REQUIRED_LIST ++
                    OptionalEjson ++
                    [{
                      <<"includes">>, [{IncludedChange}]
                     }]
                  }],

  ?assertEqual(ExpectedEjson,
               deliv_pipeline_status_json:create_body(
                 [lists:reverse(?REQUIRED_LIST)], [IncludedChange]
                )
              ).

create_body_should_obey_included_changes_mapped_to_changeset_id_test() ->
  OptionalProplistKeys = [<<"submitter">>, <<"submitted_at">>,
                          <<"approved_by">>, <<"delivered_by">>
                         ],
  OptionalEjson = [{Key, undefined} || Key <- OptionalProplistKeys],

  FirstChangePropList = ?REQUIRED_LIST ++ OptionalEjson ++ [{<<"changeset_id">>, <<"1">>}],
  SecondChangePropList = ?REQUIRED_LIST ++ OptionalEjson ++ [{<<"changeset_id">>, <<"1">>}],

  ExpectedEjson = [{
                        ?REQUIRED_LIST ++ OptionalEjson ++
                        [{
                          <<"includes">>, [{?REQUIRED_LIST ++ OptionalEjson}]
                        }]
                      }],

  ?assertEqual(ExpectedEjson,
               deliv_pipeline_status_json:create_body(
                 [FirstChangePropList], [SecondChangePropList]
                )
              ).

create_body_should_obey_heterogeneous_changes_test() ->
  OptionalProplistKeys = [<<"submitter">>, <<"submitted_at">>,
                          <<"approved_by">>, <<"delivered_by">>
                         ],
  OptionalEjson = [{Key, undefined} || Key <- OptionalProplistKeys],

  FirstChangePropList = ?REQUIRED_LIST ++ OptionalEjson ++ [{<<"changeset_id">>, <<"1">>}],
  SecondChangePropList = ?REQUIRED_LIST ++ OptionalEjson ++ [{<<"changeset_id">>, <<"1">>}],
  ThirdChangePropList = ?REQUIRED_LIST ++ OptionalEjson ++ [{<<"changeset_id">>, <<"2">>}],

  ExpectedFirstEjson = [{
                        ?REQUIRED_LIST ++ OptionalEjson ++
                        [{
                          <<"includes">>, [{?REQUIRED_LIST ++ OptionalEjson}]
                        }]
                      }],

  ExpectedSecondEjson = [{
                        ?REQUIRED_LIST ++ OptionalEjson ++
                        [{
                          <<"includes">>, []
                        }]
                      }],
  ExpectedEjson = ExpectedFirstEjson ++ ExpectedSecondEjson,

  ?assertEqual(ExpectedEjson,
               deliv_pipeline_status_json:create_body(
                 [FirstChangePropList, ThirdChangePropList], [SecondChangePropList]
                )
              ).
