-module(deliv_hand_phase_runs_named_deserializer).

-export([from_json/1]).

%% The values contained in the JSON come from delivery-cmd in automate-ctl.
-spec from_json({[{binary(), binary() | true | false | undefined}]})
    -> [{atom(), binary() | true | false}].
from_json(Json) ->
    RunComplete = ej:get([<<"run_complete">>], Json),
    RunLog = ej:get([<<"run_log">>], Json),
    RunStatus = ej:get([<<"run_status">>], Json),
    RunSuccess = ej:get([<<"run_success">>], Json),

    Finished = determine_finished(RunComplete),
    [{finished, Finished},
     {finished_at, determine_finished_at(Finished)},
     {run_log, RunLog},
     {run_status, RunStatus},
     {run_success, RunSuccess},
     {status, determine_status(RunComplete, RunSuccess)}].

determine_status(Complete, Success) ->
    case {Complete, Success} of
        {true, true} ->
            <<"passed">>;
        {C, _} when C == undefined; C == false ->
            <<"running">>;
        {true, false} ->
            <<"failed">>
    end.

determine_finished(undefined) ->
    false;
determine_finished(Finished) ->
    Finished.

-spec determine_finished_at(any()) -> undefined | binary().
determine_finished_at(true) ->
    chef_utils:db_now();
determine_finished_at(_) ->
    undefined.
