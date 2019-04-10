-module(deliv_enterprise_default_search_tests).

-include_lib("hoax/include/hoax.hrl").
-include("deliv_types.hrl").

-compile(export_all).

persistence_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "persistence", setup, teardown).

setup() ->
    error_logger:tty(false),
    eu_database:setup(),
    deliv_enterprise:getval(id, eu_data:with_enterprise(<<"deliv_change_test_enterprise">>,
                            fun(Enterprise) ->
                                    Enterprise
                            end)).

teardown(_) ->
    eu_database:teardown(),
    error_logger:tty(true).

persistence_fetch_should_return_empty_list_when_no_default_query_exists(EntId) ->
    ?assertEqual([], deliv_enterprise_default_search:fetch(
                       EntId
                      )).

persistence_upsert_then_fetch_should_return_stored_search(EntId) ->
    deliv_enterprise_default_search:upsert(EntId, "search"),
    [Result] = deliv_enterprise_default_search:fetch(EntId),
    ?assertEqual(EntId,Result#deliv_enterprise_default_search.ent_id),
    ?assertEqual(<<"search">>, Result#deliv_enterprise_default_search.search).

persistence_upsert_then_upsert_then_fetch_should_return_updated_search(EntId) ->
    deliv_enterprise_default_search:upsert(EntId, "search"),
    deliv_enterprise_default_search:upsert(EntId, "search2"),
    [Result] = deliv_enterprise_default_search:fetch(EntId),
    ?assertEqual(EntId,Result#deliv_enterprise_default_search.ent_id),
    ?assertEqual(<<"search2">>, Result#deliv_enterprise_default_search.search).

persistence_delete_when_no_records_fetch_should_return_empty(EntId) ->
    deliv_enterprise_default_search:delete(EntId),

    ?assertEqual([], deliv_enterprise_default_search:fetch(EntId)).

persistence_delete_then_fetch_should_return_empty(EntId) ->
    deliv_enterprise_default_search:upsert(EntId, "search"),

    deliv_enterprise_default_search:delete(EntId),

    ?assertEqual([], deliv_enterprise_default_search:fetch(EntId)).

persistence_delete_then_fetch_when_two_enterprises_should_return_empty(EntId) ->
    EntId2 = eu_data:with_enterprise("enterprise2", fun(Enterprise) ->
                                             deliv_enterprise:getval(id, Enterprise)
                                           end),
    deliv_enterprise_default_search:upsert(EntId, "search"),
    deliv_enterprise_default_search:upsert(EntId2, "search2"),

    deliv_enterprise_default_search:delete(EntId),

    ?assertEqual([], deliv_enterprise_default_search:fetch(EntId)),
    [Enterprise2DefaultSearch] = deliv_enterprise_default_search:fetch(EntId2),
    ?assertEqual(<<"search2">>, Enterprise2DefaultSearch#deliv_enterprise_default_search.search).
