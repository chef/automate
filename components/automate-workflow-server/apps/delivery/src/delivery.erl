-module(delivery).

-export([main/1]).

main(_Args) ->
    io:format("Starting delivery..."),
    delivery_app:start().
