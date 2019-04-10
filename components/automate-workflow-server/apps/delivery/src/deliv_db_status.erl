-module(deliv_db_status).

-include("deliv_types.hrl").

-export([
         ping/1
        ]).

%% @doc run a query to see if postgres is responding
-spec ping(atom()) -> #status_metadata{}.
ping(DisasterRecoveryMode) ->
    {Status, Attributes} = try
        verify_postgres_running(),
        deliv_db_replication_status:ping(DisasterRecoveryMode)
    catch
        _:Why ->
            chef_log:log(error, "PING FAILED: ~p", [Why]),
            {fail, []}
    end,
    #status_metadata{service = <<"postgres">>, status = Status, additional_attributes=Attributes}.

verify_postgres_running() ->
    case sqerl:execute(<<"SELECT 'pong' as ping LIMIT 1">>) of
        {ok, [[{<<"ping">>, <<"pong">>}]]} ->
            pong;
        Failure ->
            throw(Failure)
    end.
