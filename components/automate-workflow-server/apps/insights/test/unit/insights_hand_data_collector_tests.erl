-module(insights_hand_data_collector_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

content_types_accepted_accepts_json_test() ->
    ExpectedContentType = {[{{<<"application">>,<<"json">>,'*'},from_json}], request, state},
    ?assertEqual(ExpectedContentType, insights_hand_data_collector:content_types_accepted(request, state)).

content_types_provided_provides_json_test() ->
    ExpectedContentType = {[{{<<"application">>,<<"json">>,'*'},to_json},{<<"*/*">>,to_json}], request, state},
    ?assertEqual(ExpectedContentType, insights_hand_data_collector:content_types_provided(request, state)).

allowed_methods_returns_get_test() ->
    ?assertEqual({[<<"GET">>, <<"POST">>], request, state}, insights_hand_data_collector:allowed_methods(request, state)).

from_json_fixture_test_() ->
    hoax:fixture(?MODULE, from_json).

from_json_performs_auth_and_publish_if_exchange_is_valid() ->
    Exchange = <<"chef">>,
    hoax:mock(cowboy_req,
                ?expect(binding,
                    ?withArgs([exchange, req]),
                    ?andReturn({Exchange, chef}))),

    hoax:mock(insights_rabbitmq_utils,
                ?expect(exchange_map_lookup,
                    ?withArgs([Exchange]),
                    ?andReturn({ok, insights_exchange}))),

    hoax:mock(insights_data_collector_auth,
                ?expect(insights_auth_check_and_publish,
                    ?withArgs([req, state, insights_exchange]),
                    ?andReturn({halt, req, state}))),

    Actual = insights_hand_data_collector:from_json(req, state),
    ?assertEqual({halt, req, state}, Actual),
    ?verifyAll.

from_json_returns_404_if_exchange_is_not_valid() ->
    Exchange = <<"chef_foo">>,

    hoax:mock(cowboy_req,
                ?expect(binding,
                    ?withArgs([exchange, req]),
                    ?andReturn({Exchange, chef}))),

    hoax:mock(insights_rabbitmq_utils,
              ?expect(exchange_map_lookup,
                      ?withArgs([Exchange]),
                      ?andReturn({error, unknown_exchange}))),

    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([404, exchange_not_found, req, state]),
                      ?andReturn(error))),

    Actual = insights_hand_data_collector:from_json(req, state),
    ?assertEqual(error, Actual),
    ?verifyAll.

to_json_returns_pongs() ->
    Body = {[
             {<<"status">>, <<"pong">>},
             {<<"rabbitmq">>, {[
                 {<<"vhost_aliveness">>, {[
                     {<<"status">>, <<"pong">>}
                 ]}},
                 {<<"node_health">>, {[
                     {<<"status">>, <<"pong">>}
                 ]}}
             ]}}
           ]},

    Encoded = chef_json:encode(Body),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Body, request, state]),
                      ?andReturn({Encoded, request2, state2}))),
    hoax:mock(insights_rabbitmq_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"rabbitmq">>,
                                                  status = pong,
                                                  additional_attributes =
                                                     [{<<"rabbitmq">>, {[
                                                         {<<"vhost_aliveness">>, {[{<<"status">>, <<"pong">>}]}},
                                                         {<<"node_health">>, {[{<<"status">>, <<"pong">>}]}}]}}]
                                                 }))),

    ?assertEqual({Encoded, request2, state2}, insights_hand_data_collector:to_json(request, state)),
    ?verifyAll.

to_json_rabbitmq_vhost_down() ->
    Body = {[
             {<<"status">>, <<"fail">>},
             {<<"rabbitmq">>, {[
                 {<<"vhost_aliveness">>, {[
                     {<<"status">>, <<"fail">>}
                 ]}},
                 {<<"node_health">>, {[
                     {<<"status">>, <<"pong">>}
                 ]}}
             ]}}
           ]},
    Encoded = chef_json:encode(Body),
    hoax:mock(cowboy_req,
              [
               ?expect(set_resp_header,
                       ?withArgs([<<"content-type">>, <<"application/json">>, request]),
                       ?andReturn(request2)),
               ?expect(reply,
                       ?withArgs([500, [], Encoded, request2]),
                       ?andReturn({request3, state3}))
              ]),
    hoax:mock(insights_rabbitmq_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"rabbitmq">>,
                                                  status = fail,
                                                  additional_attributes =
                                                     [{<<"rabbitmq">>, {[
                                                         {<<"vhost_aliveness">>, {[{<<"status">>, <<"fail">>}]}},
                                                         {<<"node_health">>, {[{<<"status">>, <<"pong">>}]}}]}}]
                                                 }))),
    ?assertEqual({request3, state3}, insights_hand_data_collector:to_json(request, state)),
    ?verifyAll.

to_json_insights_disabled() ->
    application:set_env(insights, enabled, false),
    Body = {[
             {<<"status">>, <<"fail">>},
             {<<"rabbitmq">>, {[
                 {<<"status">>, <<"not_running">>},
                 {<<"description">>, <<"RabbitMQ is disabled">>}
             ]}}
           ]},
    Encoded = chef_json:encode(Body),
    hoax:mock(cowboy_req,
              [
               ?expect(set_resp_header,
                       ?withArgs([<<"content-type">>, <<"application/json">>, request]),
                       ?andReturn(request2)),
               ?expect(reply,
                       ?withArgs([500, [], Encoded, request2]),
                       ?andReturn({request3, state3}))
              ]),

    ?assertEqual({request3, state3}, insights_hand_data_collector:to_json(request, state)),
    ?verifyAll.

to_json_rabbitmq_management_disabled() ->
    application:set_env(insights, enabled, true),
    application:set_env(insights, rabbitmq_management_enabled, false),
    Body = {[
             {<<"status">>, <<"fail">>},
             {<<"rabbitmq">>, {[
                 {<<"status">>, <<"not_running">>},
                 {<<"description">>, <<"rabbitmq_management plugin is disabled">>}
             ]}}
           ]},
    Encoded = chef_json:encode(Body),
    hoax:mock(cowboy_req,
              [
               ?expect(set_resp_header,
                       ?withArgs([<<"content-type">>, <<"application/json">>, request]),
                       ?andReturn(request2)),
               ?expect(reply,
                       ?withArgs([500, [], Encoded, request2]),
                       ?andReturn({request3, state3}))
              ]),
    ?assertEqual({request3, state3}, insights_hand_data_collector:to_json(request, state)),
    ?verifyAll.
