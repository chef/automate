-module(deliv_phase_run_log).

-include("deliv_types.hrl").
-include("deliv_phase_run_log.hrl").

-compile({parse_transform, sqerl_gobot}).

-export([save/2, fetch/1]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

'#insert_fields'() -> [run_id, data].

'#update_fields'() -> [run_id, data].

'#statements'() ->
    [default,
     {fetch_by_run_id,
     sqerl_rec:gen_fetch(?MODULE, [run_id])}
    ].

'#table_name'() -> "phase_run_logs".

-spec save(binary(), binary()) -> db_op_result(any()).
save(RunId, Data) ->
    RunId2 = erlang:binary_to_integer(RunId),
    deliv_db:insert(#deliv_phase_run_log{run_id=RunId2, data=Data}).

-spec fetch(binary()) -> {ok, [d_log_object()]} | {error, _Why}.
fetch(RunId) ->
    RunId2 = erlang:binary_to_integer(RunId),
    deliv_db:fetch2(?MODULE, run_id, RunId2).
