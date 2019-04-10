%% Copyright 2014 Opscode, Inc. All Rights Reserved.

%% @doc Provides functions to parse a TSV input.
%% Pretty simple format: each line should be separated by a new-line character (\n)
%% On each line, fields are separated by tabs (\t).
%% Fields cannot contain tabs.
%% Accepts strings as either lists or binaries (and passes them as such to the `fun' provided).
-module(tsv_parser).

-export([process_tsv_string_with/3,
         process_tsv_string_with/4]).

-include("deliv_types.hrl").

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(LINE_DELIMITER, "\n").
-define(FIELD_DELIMITER, "\t").
-define(UNDEFINED_VALUE, undefined).

%% @doc Parses the TSV-compatible string, and calls `Fun' for each line.
%% `Fun' should take 2 arguments:
%%  - the 1st one is either `empty_line', or `eof' or `{line, Line}'
%%    where `Line' is the list of the fields found
%%  - the 2nd is the accumulator passed from call to call.
%% Missing fields are replaced by `undefined' atoms
%% The first call to `Fun' is made with Acc0
%% Returns whatever `Fun' returns on its last call (with `eof')
-spec process_tsv_string_with(String, Fun, _Acc0) -> Acc when
    String :: str_or_binary(),
    Fun :: fun((empty_line | {line, _Line}, Acc) -> Acc),
    Acc :: any().
process_tsv_string_with(String, Fun, Acc0) ->
    process_tsv_string_with(String, Fun, Acc0, 0).

%% @doc Pretty much the same as `process_tsv_string_with/3', except that you can
%% specify how many fields should be on each line at the minimum, and if there are
%% lines with less fields, they get padded with `undefined' atoms to reach the
%% specified `MinLength'
-spec process_tsv_string_with(String, Fun, _Acc0, MinLength) -> Acc when
    String :: str_or_binary(),
    Fun :: fun((empty_line | {line, _Line}, Acc) -> Acc),
    Acc :: any(),
    MinLength :: integer().
process_tsv_string_with(String, Fun, Acc0, MinLength) when MinLength >= 0 ->
    ReOptions = case erlang:is_list(String) of
        false -> [];
        true -> [{return,list}]
    end,
    Fun(
        eof,
        lists:foldl(
            fun(Line, Acc) -> Fun(split_line(Line, ReOptions, MinLength), Acc) end,
            Acc0,
            split_input(String, ReOptions)
        )
    ).

%% @private
%% Parses a line
split_line(Empty, _ReOptions, _MinLength) when Empty =:= ""; Empty =:= <<"">> -> empty_line;
split_line(Line, ReOptions, MinLength) ->
    Fields = lists:map(
        fun(Empty) when Empty =:= ""; Empty =:= <<"">> -> ?UNDEFINED_VALUE;
           (Field) -> Field
        end,
        re:split(Line, ?FIELD_DELIMITER, ReOptions)
    ),
    {line, Fields ++ lists:duplicate(max(0, MinLength - length(Fields)), ?UNDEFINED_VALUE)}.

%% @private
%% Splits input in lines
split_input(Input, ReOptions) ->
    re:split(Input, ?LINE_DELIMITER, ReOptions).
