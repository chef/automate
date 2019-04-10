-module(deliv_db_replication_status).

-include("deliv_types.hrl").

-export([
          ping/1
        ]).

%% @doc Run secondary postgresql replication status check to determine if primary
%% or secondary. If primary run primary replication status check and return status.
%% Otherwise, return fail.
%%
-spec ping(atom()) -> {pong | degraded, [{binary(), binary()}]}.
ping(cold_standby) ->
    query_secondary();
ping(primary) ->
    query_primary();
ping(standalone) ->
    {pong, []}.

query_secondary() ->
    case sqerl:execute(<<"SELECT pg_last_xlog_receive_location()">>) of
        {ok, [Result]} ->
            {pong, Result};
        {error, Reason} ->
            chef_log:log(error, "Secondary Location Query failed: ~p", [Reason]),
            {degraded, [{<<"replication">>, <<"fail">>}]}
    end.

query_primary() ->
    case sqerl:execute(<<"SELECT client_addr, pg_current_xlog_location FROM pg_stat_repl, pg_current_xlog_location()">>) of
        {ok, [[{<<"client_addr">>, ClientInet}, {<<"pg_current_xlog_location">>, CurrentLocation}]]} ->
            {pong, [{<<"standby_ip_address">>, inet_to_bin(ClientInet)}, {<<"pg_current_xlog_location">>, CurrentLocation}]};
        {ok, []} ->
            chef_log:log(error, "Primary Replication Query failed: no results"),
            replication_failure_response();
        {error, Reason} ->
            chef_log:log(error, "Primary Replication Query failed: ~p", [Reason]),
            replication_failure_response()
    end.

replication_failure_response() ->
    {degraded, [{<<"replication">>, <<"fail">>},
     {<<"description">>, <<"Replication is not running. Check your configuration.">>}]}.

inet_to_bin(Inet) ->
    AList = [integer_to_list(element(I, Inet)) || I <- lists:seq(1,4)],
    list_to_binary(string:join(AList, ".")).
