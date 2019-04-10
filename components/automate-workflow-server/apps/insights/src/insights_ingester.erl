%% @doc Receiver of ingest requests. Publishes message to rabbit queue
-module(insights_ingester).

%% API
-export([
         publish/1,
         publish/2,
         publish/3
        ]).

-spec publish(Data :: binary()) -> ok.
publish(Data) ->
    publish(Data, <<"delivery">>).

publish(Data, Topic) ->
    publish(Data, Topic, insights_queue).

publish(Data, Topic, Server) ->
    AMQPMsg = bunny_util:new_message(Data),
    PersistentData = bunny_util:set_delivery_mode(AMQPMsg, 2),
    ok = bunnyc:publish(Server, Topic, PersistentData).
