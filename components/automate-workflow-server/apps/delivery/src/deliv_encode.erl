%% @doc In the places when we need our data to be guaranteed to not have
%% any potentitally dangerous character (typically quotes, spaces and so on)
%% we can use those functions to encode/decode it to a safe form
%%
%% We use URL-encoding for now, but we could change if needed!
%% TODO: actually, that is not great : URL-encoding will turn
%% any non-ASCII character into '?', which means that 2 different names
%% might map to the same encoded name.
%% We need to find another encoding that satisfies the following conditions:
%%  1. maps any ASCII char to itself (to keep nice and simple input data identical)
%%  2. bijectivity (that's where URL-encoding falls short)
%%  3. be defined over any unicode character
%%  4. maps anything to a 'safe' subset of ASCII (by which we mean ASCII minus
%%     the following characters: space '"\/&|;\n
%% In any case, when we find such an encoding, we need only change this file!
-module(deliv_encode).

-export([
    encode/1,
    decode/1
]).

-include("deliv_types.hrl").

%% @doc Encodes outside world's data into something safe
%% for us to play with
-spec encode(str_or_binary()) -> str_or_binary().
encode(Str) when erlang:is_list(Str) ->
    http_uri:encode(Str);
encode(Bin) when erlang:is_binary(Bin) ->
    erlang:iolist_to_binary(encode(erlang:binary_to_list(Bin))).

%% @doc Reverse operation for `encode/1' above.
-spec decode(str_or_binary()) -> str_or_binary().
decode(Str) when erlang:is_list(Str) ->
    http_uri:decode(Str);
decode(Bin) when erlang:is_binary(Bin) ->
    erlang:iolist_to_binary(decode(erlang:binary_to_list(Bin))).
