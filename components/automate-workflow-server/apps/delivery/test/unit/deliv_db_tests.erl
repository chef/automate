-module(deliv_db_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

fixture_test_() ->
    hoax:fixture(?MODULE).

delete_unique_by_scoping_params_returns_if_record_not_found() ->
    Mod = deliv_db,
    Ent = <<"Ent">>,
    Org = <<"Org">>,
    Proj = <<"Proj">>,

    hoax:mock(sqerl_rec,
             ?expect(cquery,
                    ?withArgs([Mod, delete_by_scoping_params, [Ent, Org, Proj]]),
                    ?andReturn({ok, 0}))),

    ?assertEqual({ok, 0}, deliv_db:delete_unique_by_scoping_params(Mod, [Ent, Org], Proj)),
    ?verifyAll.

delete_unique_by_scoping_params_returns_if_record_was_deleted() ->
    Mod = deliv_db,
    Ent = <<"Ent">>,
    Org = <<"Org">>,
    Proj = <<"Proj">>,

    hoax:mock(sqerl_rec,
             ?expect(cquery,
                    ?withArgs([Mod, delete_by_scoping_params, [Ent, Org, Proj]]),
                    ?andReturn({ok, 1}))),

    ?assertEqual({ok, 1}, deliv_db:delete_unique_by_scoping_params(Mod, [Ent, Org], Proj)),
    ?verifyAll.

delete_unique_by_scoping_params_returns_if_error() ->
    Mod = deliv_db,
    Ent = <<"Ent">>,
    Org = <<"Org">>,
    Proj = <<"Proj">>,

    hoax:mock(sqerl_rec,
             ?expect(cquery,
                    ?withArgs([Mod, delete_by_scoping_params, [Ent, Org, Proj]]),
                    ?andReturn({error, why}))),

    ?assertEqual({error, why}, deliv_db:delete_unique_by_scoping_params(Mod, [Ent, Org], Proj)),
    ?verifyAll.

select_conflict() ->
    hoax:mock(sqerl,
              ?expect(select,
                      ?withArgs([fake_mod_insert_a_thing,
                                 [1, 2], identity, []]),
                      ?andReturn({conflict, <<"why">>}))),
    Mod = fake_mod,
    Query = insert_a_thing,
    Params = [1, 2],
    ?assertEqual({error, conflict},
                 deliv_db:select(Mod, Query, Params)),
    ?verifyAll.

select_no_rows() ->
    hoax:mock(sqerl,
              ?expect(select,
                      ?withArgs([fake_mod_select_nothing,
                                 [1], identity, []]),
                      ?andReturn({ok, none}))),

    ?assertEqual({ok, []}, deliv_db:select(fake_mod, select_nothing, [1])),
    ?verifyAll.

select_rows_returned() ->
    hoax:mock(sqerl,
              ?expect(select,
                      ?withArgs([fake_mod_select_rows_returned,
                                 [1], identity, []]),
                      ?andReturn({ok, [[1], [2], [3]]}))),

    ?assertEqual({ok, [[1], [2], [3]]},
                 deliv_db:select(fake_mod, select_rows_returned, [1])),
    ?verifyAll.

select_rows_with_count() ->
    hoax:mock(sqerl,
              ?expect(select,
                      ?withArgs([fake_mod_select_rows_returned,
                                 [1], identity, []]),
                      ?andReturn({ok, 1, [[1], [2], [3]]}))),

    ?assertEqual({ok, [[1], [2], [3]]},
                 deliv_db:select(fake_mod, select_rows_returned, [1])),
    ?verifyAll.

select_error() ->
    hoax:mock(sqerl,
              ?expect(select,
                      ?withArgs([fake_mod_error,
                                 [1], identity, []]),
                      ?andReturn({error, <<"why">>}))),
        ?assertEqual({error, <<"why">>},
                     deliv_db:select(fake_mod, error, [1])),
    ?verifyAll.

select_typed_error() ->
    hoax:mock(sqerl,
              ?expect(select,
                      ?withArgs([fake_mod_error,
                                 [1], identity, []]),
                      ?andReturn({error, {<<"1234">>, <<"why">>}}))),
        ?assertEqual({error, <<"1234">>},
                     deliv_db:select(fake_mod, error, [1])),
    ?verifyAll.

handle_op_result_test_() ->
    %% {Input, Expected} Since the `Type' and `Item' arguments are
    %% only used for logging, and we aren't testing the logging logic
    %% here, constant values for these arguments are used to simplify
    %% test input.
    Tests = [{{error, {conflict, why}}, {error, conflict}},
             {{error, {a, b, c}}, {error, {a, b, c}}},
             {{ok, none}, {ok, []}},
             {{ok, 2, [1, 2]}, {ok, [1, 2]}},
             {{ok, [1, 2]}, {ok, [1, 2]}},
             {[{a_rec, 1, 2}], [{a_rec, 1, 2}]}],
    [ begin
          TName = to_name(Result),
          {TName, ?_assertEqual(Expected,
                                deliv_db:handle_op_result(op_type,
                                                          Result, item))}
      end
      || {Result, Expected} <- Tests ].

to_name(X) ->
    erlang:iolist_to_binary(io_lib:format("~p", [X])).
