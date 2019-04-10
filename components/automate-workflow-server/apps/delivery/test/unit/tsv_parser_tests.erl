%% Copyright 2014 Opscode, Inc. All Rights Reserved.

-module(tsv_parser_tests).

-include_lib("eunit/include/eunit.hrl").

split_empty_line_test_() ->
    %% returns `empty_line' if the line is empty
    [
        ?_assertEqual(
            empty_line,
            tsv_parser:split_line(E, [], 5)
        ) || E <- ["", <<"">>]
    ].

%% basic functionality, both for lists and binaries
split_line_test_() ->
    TestsBuilder = fun(CastFun, ReOptions) ->
        [
            ?_assertEqual(
                {line, lists:map(
                    fun(undefined) -> undefined;
                       (X) -> CastFun(X)
                    end,
                    Expected
                )},
                tsv_parser:split_line(CastFun(Raw), ReOptions, 2)
            ) || {Expected, Raw} <- [
                {["a, ", "b", "c"], "a, \tb\tc"},
                {["a", "b"], "a\tb"},
                {["a", undefined], "a"},
                {["a", undefined, undefined, undefined, "b", undefined, "c"], "a\t\t\t\tb\t\tc"}
            ]
        ]
    end,
    ListsTests = TestsBuilder(fun(X) -> X end, [{return,list}]),
    BinaryTests = TestsBuilder(fun(X) -> erlang:iolist_to_binary(X) end, []),
    [ListsTests, BinaryTests].

%% tests everything else
process_tsv_string_with_test_() ->
    Input = "field1\nf2, \tf3\n\n4\t\t,\tcoucou",
    FunBuilder = fun(CastFun) ->
        fun(Line, {Counter, Tests}) ->
            {
                Counter + 1,
                [
                    ?_assertEqual(
                        case Counter of
                            0 -> {line, [CastFun("field1"), undefined]};
                            1 -> {line, [CastFun("f2, "), CastFun("f3")]};
                            2 -> empty_line;
                            3 -> {line, [CastFun("4"), undefined, CastFun(","), CastFun("coucou")]};
                            4 -> eof
                        end,
                        Line)
                    | Tests
                ]
            }
        end
    end,
    %% tests for lists
    {ListsCounter, ListsTests} = tsv_parser:process_tsv_string_with(Input, FunBuilder(fun(X) -> X end), {0, []}, 2),
    %% and tests for binaries
    {BinaryCounter, BinaryTests} = tsv_parser:process_tsv_string_with(erlang:iolist_to_binary(Input), FunBuilder(fun(X) -> erlang:iolist_to_binary(X) end), {0, []}, 2),
    [
        ListsTests,
        BinaryTests,
        [?_assertEqual(5, Counter) || Counter <- [ListsCounter, BinaryCounter]]
    ].
