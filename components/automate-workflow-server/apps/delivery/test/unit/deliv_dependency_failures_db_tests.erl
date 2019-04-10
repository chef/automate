-module(deliv_dependency_failures_db_tests).

-include_lib("hoax/include/hoax.hrl").
-include("deliv_coordinates.hrl").

-compile(export_all).

insert_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "insert_", setup, teardown).

fetch_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "fetch_", setup, teardown).

basic_setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup().

setup() ->
    basic_setup(),
    Pipeline1 = eu_data:with_enterprise(<<"test_enterprise1">>,
       eu_data:with_organization(<<"test_organization">>,
        eu_data:with_project(<<"test_project_1">>,
          eu_data:with_pipeline(<<"master">>,
            fun(_Enterprise, _Organization, _Project, Pipeline) ->
                Pipeline
            end)))),
    Pipeline2 = eu_data:with_enterprise(<<"test_enterprise2">>,
       eu_data:with_organization(<<"test_organization">>,
        eu_data:with_project(<<"test_project_2">>,
          eu_data:with_pipeline(<<"master">>,
            fun(_Enterprise, _Organization, _Project, Pipeline) ->
                Pipeline
            end)))),
    eu_data:with_enterprise(<<"test_enterprise3">>,
       fun(_) ->
           ok
       end),
    {Pipeline1, Pipeline2}.

teardown(_) ->
    eu_application:teardown(),
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

insert_then_fetch_returns_dependency_failures({Pipeline1, Pipeline2}) ->
  Pipeline1Id = deliv_pipeline:getval(id, Pipeline1),
  Pipeline2Id = deliv_pipeline:getval(id, Pipeline2),
  deliv_dependency_failures_db:insert(<<"test_enterprise1">>, [#{Pipeline1Id => failed}, #{Pipeline2Id => failed}]),
  Actual = deliv_dependency_failures_db:fetch(<<"test_enterprise1">>),
  ?assertEqual({ok, [#{Pipeline1Id => failed}, #{Pipeline2Id => failed}]}, Actual).

insert_then_fetch_when_existing_state_overwrites({Pipeline1, Pipeline2}) ->
  Pipeline1Id = deliv_pipeline:getval(id, Pipeline1),
  deliv_dependency_failures_db:insert(<<"test_enterprise1">>, [#{Pipeline1Id => failed}]),
  Pipeline2Id = deliv_pipeline:getval(id, Pipeline2),
  deliv_dependency_failures_db:insert(<<"test_enterprise1">>, [#{Pipeline2Id => failed}]),
  Actual = deliv_dependency_failures_db:fetch(<<"test_enterprise1">>),
  ?assertEqual({ok, [#{Pipeline2Id => failed}]}, Actual).

insert_then_fetch_when_second_enterprise_fetches_only_enterprise({Pipeline1, Pipeline2}) ->
  Pipeline1Id = deliv_pipeline:getval(id, Pipeline1),
  deliv_dependency_failures_db:insert(<<"test_enterprise1">>, [#{Pipeline1Id => failed}]),
  Pipeline2Id = deliv_pipeline:getval(id, Pipeline2),
  deliv_dependency_failures_db:insert(<<"test_enterprise2">>, [#{Pipeline2Id => failed}]),
  Actual = deliv_dependency_failures_db:fetch(<<"test_enterprise2">>),
  ?assertEqual({ok, [#{Pipeline2Id => failed}]}, Actual).

insert_when_empty_dependency_failures_clears_dependency_failures({Pipeline1, _}) ->
  Pipeline1Id = deliv_pipeline:getval(id, Pipeline1),
  deliv_dependency_failures_db:insert(<<"test_enterprise1">>, [#{Pipeline1Id => failed}]),
  deliv_dependency_failures_db:insert(<<"test_enterprise1">>, []),

  ?assertEqual({ok, []}, deliv_dependency_failures_db:fetch(<<"test_enterprise1">>)).

fetch_when_no_data_should_return_empty_list(_) ->
  ?assertEqual({ok, []}, deliv_dependency_failures_db:fetch(<<"test_enterprise3">>)).

 fetch_when_ent_does_not_exist(_) ->
     Actual = deliv_dependency_failures_db:fetch(<<"missing_enterprise">>),
     ?assertEqual({error, not_found}, Actual).
