-module(insights_rabbitmq_utils).

-export([exchange_map_lookup/1]).

exchange_map_lookup(<<"habitat">>) -> {ok, habitat_queue};
exchange_map_lookup(<<"chef">>) -> {ok, insights_queue};
exchange_map_lookup(undefined) -> {ok, insights_queue};
exchange_map_lookup(_) -> {error, unknown_queue}.
