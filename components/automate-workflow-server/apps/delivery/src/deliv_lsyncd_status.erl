-module(deliv_lsyncd_status).

-include("deliv_types.hrl").

-export([
          ping/1
        ]).

%% @doc check the stat file to see if lsyncd is running
-spec ping(atom()) -> #status_metadata{}.
ping(DisasterRecoveryMode) ->
    Stat = get_stat(),
    process_status(DisasterRecoveryMode, Stat).

process_status(primary, run) ->
    Latency = get_latency(),
    #status_metadata{service = <<"lsyncd">>, status = pong, additional_attributes = [{<<"latency">>, Latency}]};
process_status(primary, _Stat) ->
    #status_metadata{service = <<"lsyncd">>, status = fail};
process_status(standalone, run) ->
    #status_metadata{service = <<"lsyncd">>, status = pong, additional_attributes = [{<<"description">>, <<"No backup server configured.">>}]};
process_status(standalone, _Stat) ->
    #status_metadata{service = <<"lsyncd">>, status = not_running};
process_status(cold_standby, null) ->
    #status_metadata{service = <<"lsyncd">>, status = not_running};
process_status(cold_standby, _Stat) ->
    #status_metadata{service = <<"lsyncd">>, status = pong, additional_attributes = [{<<"description">>, <<"Server configured as cold standby. Lsyncd should only be run on the primary.">>}]}.

get_stat() ->
    Path = delivery_app:get_env(lsyncd_stat_path),
    case file:read_file(Path) of
        {ok, <<"run\n">>} ->
            run;
        _ ->
            null
    end.

get_latency() ->
    Path = delivery_app:get_env(lsyncd_log_file_path),
    File = file:open(Path, [read, raw, read_ahead]),
    get_latency(File).

get_latency({ok, File}) ->
    Latency = count_latency(File, 0),
    file:close(File),
    chef_utils:to_bin(Latency);
get_latency(_) ->
    chef_log:error("Could not read lsyncd status log."),
    <<"Could not read status log.">>.

count_latency(File, LatencySum) ->
    case file:read_line(File) of
        {ok, Line} ->
            count_latency(File, LatencySum + get_count(Line));
        _ ->
            LatencySum
    end.

get_count(Line) ->
    Pattern = <<"There are (\\d+) delays\n">>,
    Options = [ungreedy, {capture, all_but_first, binary}],

    case re:run(Line, Pattern, Options) of
        {match, [Delay]} ->
            chef_utils:to_int(Delay);
        _ ->
            0
    end.
