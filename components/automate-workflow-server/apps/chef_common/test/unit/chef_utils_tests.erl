-module(chef_utils_tests).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

fixture_test_() ->
    [eunit_sugar:fixture(?MODULE, priv_dir_for_mod),
     eunit_sugar:fixture(?MODULE, merge_proplists),
     eunit_sugar:fixture(?MODULE, format_timestamp),
     eunit_sugar:fixture(?MODULE, format_db_timestamp)].

priv_dir_for_mod_find_priv_dir_for_delivery() ->
    Dir = chef_utils:priv_dir_for_mod(deliv_authz),
    AuthzFile = filename:join([Dir, "authz_rules"]),
    ?assert(filelib:is_file(AuthzFile)).

priv_dir_for_mod_finds_erlware_commons() ->
    Dir = chef_utils:priv_dir_for_mod(ec_date),
    PegFile = filename:join([Dir, "ec_semver_parser.peg"]),
    ?assert(filelib:is_file(PegFile)).

priv_dir_for_mod_finds_crypto() ->
    Dir = chef_utils:priv_dir_for_mod(crypto),
    Index = string:str(Dir, "/priv"),
    ?assert(Index > 0),
    ?assert(filelib:is_dir(Dir)).

priv_dir_for_mod_fails_on_non_existing() ->
    ?assertEqual({error, bad_name},
                 chef_utils:priv_dir_for_mod(no_such_mod)).

merge_proplists_merges_empty() ->
    ?assertEqual([], chef_utils:merge_proplists([], [])).

merge_proplists_merges_simple() ->
    PL1 = [{a,1}, {b,2}],
    PL2 = [{c,3}],
    Expect = [{a,1}, {b,2}, {c,3}],
    ?assertEqual(Expect, chef_utils:merge_proplists(PL1, PL2)).

merge_proplists_merges_dups_but_favors_first_arg() ->
    PL1 = [{a,1}, {b,2}],
    PL2 = [{a,3}, {c,4}],
    Expect = [{a,1}, {b,2}, {c,4}],
    ?assertEqual(Expect, chef_utils:merge_proplists(PL1, PL2)).

format_timestamp_formats_a_timestamp() ->
    Timestamp = {{1981, 2, 25}, {8, 31, 20}},
    ?assertEqual(<<"1981-02-25 08:31:20">>, chef_utils:format_timestamp(Timestamp)).

format_timestamp_formats_a_timestamp_with_float_seconds() ->
    Timestamp = {{1981, 2, 25}, {8, 31, 20.99}},
    ?assertEqual(<<"1981-02-25 08:31:20">>, chef_utils:format_timestamp(Timestamp)).

format_db_timestamp_formats_a_timestamp_for_postgres() ->
    Timestamp = {{1981, 2, 25}, {8, 31, 20}},
    ?assertEqual(<<"1981-02-25T8:31:20Z">>, chef_utils:format_db_timestamp(Timestamp)).

format_db_timestamp_formats_a_timestamp_with_float_seconds_for_postgres() ->
    Timestamp = {{1981, 2, 25}, {8, 31, 20.99}},
    ?assertEqual(<<"1981-02-25T8:31:20Z">>, chef_utils:format_db_timestamp(Timestamp)).

%% Leaving this as meck because I'm not sure the best way to mock
%% using hoax.
caching_wrapper_test_() ->
    {
        setup,
        fun() ->
            %% we want to count how many times the cached fun is called
            meck:new(caching_wrapper, [non_strict]),
            meck:expect(caching_wrapper, outer_cached_fun, fun caching_wrapper_test_fun/1),
            ok
        end,
        fun(_) ->
            %% we don't use `ct_meck:unload/1' here because this module
            %% `caching_wrapper' doesn't actually exist
            ok = meck:unload(caching_wrapper)
        end,
        fun(_) ->
            %% first let's make some random list of args
            ArgList = [random:uniform(3) || _ <- lists:seq(1,50)],

            %% then let's compute the expected result
            ExpectedResult = [caching_wrapper_test_fun(Arg) || Arg <- ArgList],

            %% and finally the result from the wrapped fun
            WrappedFun = chef_utils:caching_wrapper(fun caching_wrapper:outer_cached_fun/1),
            {_, Result} = lists:foldl(
                fun(Arg, {Wrapper, Acc}) ->
                    {NewWrapper, FunRes} = Wrapper(Arg),
                    {NewWrapper, [FunRes | Acc]}
                end,
                {WrappedFun, []},
                ArgList
            ),

            NumCalls = fun(Arg) -> meck:num_calls(caching_wrapper, outer_cached_fun, [Arg]) end,
            %% both the wrapped fun and the raw one should return the same things
            [?_assertEqual(ExpectedResult, lists:reverse(Result)),
             %% the original fun should have been called at most once with 1 and 3 as args
             %% since those calls are successful
             ?_assertEqual(lists:min([1, count_elem(1, ArgList)]), NumCalls(1)),
             ?_assertEqual(lists:min([1, count_elem(3, ArgList)]), NumCalls(3)),
             %% calling the original fun with 2, however, causes an error, so the result
             %% shouldn't have been cached at all
             ?_assertEqual(count_elem(2, ArgList), NumCalls(2))]
        end
    }.

to_str_test_() ->
    [
     ?_assertEqual("123", chef_utils:to_str(123)),
     ?_assertEqual("123", chef_utils:to_str(<<"123">>))
    ].

capitalize_test_() ->
    [
     ?_assertEqual(<<"Test">>, chef_utils:capitalize_str(test)),
     ?_assertEqual(<<"Test">>, chef_utils:capitalize_str(<<"test">>))
    ].

iodata_strify_test_() ->
    [
     ?_assertEqual("123", chef_utils:iodata_strify("123")),
     ?_assertEqual("123", chef_utils:iodata_strify(<<"123">>)),
     ?_assertEqual(["abc","123"], chef_utils:iodata_strify(["abc", 123])),
     ?_assertEqual(["one","two",["t",["h","ree"]]],
                    chef_utils:iodata_strify([<<"one">>, "two", [$t, ["h","ree"]]]))
    ].

%% @private
%% @doc Used by `caching_wrapper_test_/0' above
caching_wrapper_test_fun(2) -> {error, yikes};
caching_wrapper_test_fun(N) -> {ok, N * 2}.

%% @private
%% @doc A helper fun to count the number of occurences of an item in a list
count_elem(Elem, List) ->
    erlang:length(lists:filter(fun(I) -> I =:= Elem end, List)).
