%% @doc Misc helper functions for CT tests that don't belong anywhere else
%% and that shouldn't be used anywhere else than tests
-module(ct_utils).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("delivery/include/deliv_types.hrl").

-export([
         assert_same_elements/2,
         shuffle_list/1,
         unique_string/0,
         get_config/2,
         replace_config/3
        ]).

%% @doc Returns a value from the config, making sure it's defined
-spec get_config(atom(), proplist(atom(), any())) -> any().
get_config(Key, Config) ->
    Value = ?config(Key, Config),
    case Value =:= undefined of
        true ->
            ct:pal("You need to define the ~p in your CT config", [Key]),
            ?assertNotEqual(undefined, Value);
        false ->
            Value
    end.

%% @doc Replaces a value in the config
-spec replace_config(atom(), proplist(atom(), any()), any())
        -> proplist(atom(), any()).
replace_config(Key, Config, NewValue) ->
    lists:keystore(Key, 1, Config, {Key, NewValue}).

%% @doc Shuffles a list
%% Not super efficient or anything, just does the job
-spec shuffle_list(List :: list()) -> list().
shuffle_list(L) when erlang:is_list(L) ->
    [X || {_,X} <- lists:sort([ {random:uniform(), I} || I <- L])].

%% @doc Generates a unique string that's somewhat human-friendly
-spec unique_string() -> binary().
unique_string() ->
    {{_YY,MO,DD},{HH,MI,SS}} = calendar:now_to_datetime(os:timestamp()),
    TS = io_lib:format("~2..0B~2..0B~2..0B~2..0B~2..0B",
                       [MO, DD, HH, MI, SS]),
    Rand = chef_utils:random_string(3),
    erlang:iolist_to_binary([TS, "-", Rand]).

%% @doc Checks two list are equal, modulo the order; i.e.
%% that they contain the same elements
assert_same_elements(Expected, Actual) ->
    ?assertEqual(lists:sort(Expected), lists:sort(Actual)).
