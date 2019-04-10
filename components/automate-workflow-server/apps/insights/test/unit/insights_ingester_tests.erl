-module(insights_ingester_tests).

-compile([export_all]).

-include_lib("hoax/include/hoax.hrl").

fixture_test_() ->
    hoax:fixture(?MODULE).

publish_writes_to_rabbitmq() ->
    InsightsJson = <<"json">>,
    hoax:mock(bunny_util,[
              ?expect(new_message, ?withArgs([InsightsJson]), ?andReturn(amqp_record)),
              ?expect(set_delivery_mode, ?withArgs([amqp_record, 2]), ?andReturn(persistent_data))]),
    hoax:mock(bunnyc,
                ?expect(publish, ?withArgs([insights_queue, <<"delivery">>, persistent_data]), ?andReturn(ok))),

    Expected = ok,
    Actual = insights_ingester:publish(InsightsJson),
    ?assertEqual(Expected, Actual),
    ?verifyAll.

publish_with_topic_writes_to_rabbitmq() ->
    InsightsJson = <<"json">>,
    Topic = <<"topic">>,
    hoax:mock(bunny_util,[
              ?expect(new_message, ?withArgs([InsightsJson]), ?andReturn(amqp_record)),
              ?expect(set_delivery_mode, ?withArgs([amqp_record, 2]), ?andReturn(persistent_data))]),
    hoax:mock(bunnyc,
                ?expect(publish, ?withArgs([insights_queue, <<"topic">>, persistent_data]), ?andReturn(ok))),

    Expected = ok,
    Actual = insights_ingester:publish(InsightsJson, Topic),
    ?assertEqual(Expected, Actual),
    ?verifyAll.
