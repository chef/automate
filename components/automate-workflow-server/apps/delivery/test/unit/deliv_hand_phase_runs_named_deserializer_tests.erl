-module(deliv_hand_phase_runs_named_deserializer_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

fixture_test_() ->
    hoax:fixture(?MODULE).

from_json_returns_correct_values_for_running_json() ->
    Json = {[{<<"run_success">>, false},
             {<<"run_log">>, <<"LOG">>},
             {<<"run_status">>, <<"running">>},
             {<<"run_complete">>, false}]},

    Expected = [{finished, false},
                {finished_at, undefined},
                {run_log, <<"LOG">>},
                {run_status, <<"running">>},
                {run_success, false},
                {status, <<"running">>}],
    Values = deliv_hand_phase_runs_named_deserializer:from_json(Json),

    ?assertEqual(Expected, Values).

from_json_returns_correct_values_for_failing_json() ->
    Json = {[{<<"run_success">>, false},
             {<<"run_log">>, <<"LOG">>},
             {<<"run_status">>, <<"failed">>},
             {<<"run_complete">>, true}]},

    hoax:expect(receive
                  calendar:universal_time() -> {{2017,4,18},{12,22,23}}
                end),

    Expected = [{finished, true},
                {finished_at, <<"2017-04-18T12:22:23Z">>},
                {run_log, <<"LOG">>},
                {run_status, <<"failed">>},
                {run_success, false},
                {status, <<"failed">>}],
    Values = deliv_hand_phase_runs_named_deserializer:from_json(Json),

    ?assertEqual(Expected, Values).

from_json_returns_correct_values_for_successful_json() ->
    Json = {[{<<"run_success">>, true},
             {<<"run_log">>, <<"LOG">>},
             {<<"run_status">>, <<"finished">>},
             {<<"run_complete">>, true}]},

    hoax:expect(receive
                  calendar:universal_time() -> {{2017,4,18},{12,22,23}}
                end),

    Expected = [{finished, true},
                {finished_at, <<"2017-04-18T12:22:23Z">>},
                {run_log, <<"LOG">>},
                {run_status, <<"finished">>},
                {run_success, true},
                {status, <<"passed">>}],
    Values = deliv_hand_phase_runs_named_deserializer:from_json(Json),

    ?assertEqual(Expected, Values).
