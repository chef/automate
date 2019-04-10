-module(insights_rabbitmq_utils_tests).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

habitat_queue_atom_is_returned_when_habitat_binary_is_recieved_test() ->
    Actual = insights_rabbitmq_utils:exchange_map_lookup(<<"habitat">>),
    ?assertEqual({ok, habitat_queue}, Actual).

chef_queue_atom_is_returned_when_chef_binary_is_recieved_test() ->
    Actual = insights_rabbitmq_utils:exchange_map_lookup(<<"chef">>),
    ?assertEqual({ok, insights_queue}, Actual).

chef_queue_atom_is_returned_when_undefined_is_recieved_test() ->
    Actual = insights_rabbitmq_utils:exchange_map_lookup(undefined),
    ?assertEqual({ok, insights_queue}, Actual).

error_atom_is_returned_when_unknown_input_is_recieved_test() ->
    Actual = insights_rabbitmq_utils:exchange_map_lookup(<<"not_chef">>),
    ?assertEqual({error, unknown_queue}, Actual).
