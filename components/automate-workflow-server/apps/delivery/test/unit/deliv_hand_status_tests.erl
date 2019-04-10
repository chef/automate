-module(deliv_hand_status_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

content_types_provided_accepts_json_test() ->
    ExpectedContentType = {[{{<<"application">>,<<"json">>,'*'},to_json}], request, state},
    ?assertEqual(ExpectedContentType, deliv_hand_status:content_types_provided(request, state)).

allowed_methods_returns_get_test() ->
    ?assertEqual({[<<"GET">>], request, state}, deliv_hand_status:allowed_methods(request, state)).

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, to_json).

to_json_returns_pongs() ->
    application:set_env(visibility, enabled, true),
    application:set_env(insights, enabled, true),
    application:set_env(delivery, a2_mode, false),
    Body = {[
             {<<"status">>, <<"pong">>},
             {<<"configuration_mode">>, <<"primary">>},
             {<<"fips_mode">>, <<"false">>},
             {<<"upstreams">>, [{[
                 {<<"postgres">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"lsyncd">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"elasticsearch">>, {[
                    {<<"status">>, <<"pong">>}
                  ]}},
                 {<<"rabbitmq">>, {[
                    {<<"status">>, <<"pong">>},
                    {<<"rabbitmq">>, {[
                        {<<"vhost_aliveness">>, {[
                            {<<"status">>, <<"pong">>}
                        ]}},
                        {<<"node_health">>, {[
                            {<<"status">>, <<"pong">>}
                        ]}}
                     ]}}
                  ]}}
              ]}]},
              {<<"a2_mode">>, <<"false">>}
            ]},
    Encoded = chef_json:encode(Body),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Body, request, state]),
                      ?andReturn({Encoded, request2, state2}))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, disaster_recovery_mode]),
                      ?andReturn({ok, primary}))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, deliv_fips_mode]),
                      ?andReturn(undefined))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, a2_mode]),
                      ?andReturn({ok, false}))),
    hoax:mock(deliv_db_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"postgres">>,
                                                  status = pong}))),
    hoax:mock(deliv_lsyncd_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"lsyncd">>,
                                                  status = pong}))),
    hoax:mock(insights_rabbitmq_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"rabbitmq">>,
                                                  status = pong,
                                                  additional_attributes =
                                                     [{<<"rabbitmq">>, {[
                                                         {<<"vhost_aliveness">>, {[{<<"status">>, <<"pong">>}]}},
                                                         {<<"node_health">>, {[{<<"status">>, <<"pong">>}]}}]}}]
                                                 }))),
    hoax:mock(vis_elasticsearch_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"elasticsearch">>,
                                                  status = pong,
                                                  additional_attributes = []}))),
    ?assertEqual({Encoded, request2, state2}, deliv_hand_status:to_json(request, state)),
    ?verifyAll.

to_json_returns_pongs_with_arbitrary_data() ->
    application:set_env(insights, enable, true),
    application:set_env(visibility, enable, true),
    application:set_env(delivery, a2_mode, false),
    Body = {[
             {<<"status">>, <<"pong">>},
             {<<"configuration_mode">>, <<"primary">>},
             {<<"fips_mode">>, <<"false">>},
             {<<"upstreams">>, [{[
                 {<<"postgres">>, {[
                    {<<"status">>, <<"pong">>},
                    {<<"postgres key">>, <<"postgres value">>}
                   ]}},
                 {<<"lsyncd">>, {[
                    {<<"status">>, <<"pong">>},
                    {<<"lsyncd key">>, <<"lsyncd value">>}
                   ]}},
                 {<<"elasticsearch">>, {[
                    {<<"status">>, <<"pong">>},
                    {<<"es key">>, <<"es value">>}
                  ]}},
                 {<<"rabbitmq">>, {[
                    {<<"status">>, <<"pong">>},
                    {<<"rabbitmq key">>, <<"rabbitmq value">>}
                  ]}}
                ]}]},
                {<<"a2_mode">>, <<"false">>}
            ]},
    Encoded = chef_json:encode(Body),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Body, request, state]),
                      ?andReturn({Encoded, request2, state2}))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, disaster_recovery_mode]),
                      ?andReturn({ok, primary}))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, deliv_fips_mode]),
                      ?andReturn(undefined))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, a2_mode]),
                      ?andReturn({ok, false}))),
    hoax:mock(deliv_db_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"postgres">>,
                                                  status = pong, additional_attributes =
                                                      [{<<"postgres key">>,
                                                        <<"postgres value">>}]}))),
    hoax:mock(deliv_lsyncd_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"lsyncd">>,
                                                  status = pong, additional_attributes =
                                                      [{<<"lsyncd key">>,
                                                        <<"lsyncd value">>}]}))),
    hoax:mock(insights_rabbitmq_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"rabbitmq">>,
                                                  status = pong, additional_attributes =
                                                      [{<<"rabbitmq key">>,
                                                        <<"rabbitmq value">>}]}))),
    hoax:mock(vis_elasticsearch_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"elasticsearch">>,
                                                  status = pong, additional_attributes =
                                                      [{<<"es key">>,
                                                        <<"es value">>}]}))),
    ?assertEqual({Encoded, request2, state2}, deliv_hand_status:to_json(request, state)),
    ?verifyAll.

to_json_postgres_down() ->
    application:set_env(delivery, a2_mode, false),
    Body = {[
             {<<"status">>, <<"fail">>},
             {<<"configuration_mode">>, <<"primary">>},
             {<<"fips_mode">>, <<"false">>},
             {<<"upstreams">>, [{[
                 {<<"postgres">>, {[
                    {<<"status">>, <<"fail">>}
                   ]}},
                 {<<"lsyncd">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"elasticsearch">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"rabbitmq">>, {[
                    {<<"status">>, <<"pong">>},
                    {<<"rabbitmq">>, {[
                        {<<"vhost_aliveness">>, {[
                            {<<"status">>, <<"pong">>}
                        ]}},
                        {<<"node_health">>, {[
                            {<<"status">>, <<"pong">>}
                        ]}}
                     ]}}
                  ]}}
                ]}]},
                {<<"a2_mode">>, <<"false">>}
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
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, disaster_recovery_mode]),
                      ?andReturn({ok, primary}))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, deliv_fips_mode]),
                      ?andReturn(undefined))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, a2_mode]),
                      ?andReturn({ok, false}))),
    hoax:mock(deliv_db_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"postgres">>,
                                                  status = fail}))),
    hoax:mock(deliv_lsyncd_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"lsyncd">>,
                                                  status = pong}))),
    hoax:mock(vis_elasticsearch_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"elasticsearch">>,
                                                  status = pong}))),
    hoax:mock(insights_rabbitmq_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"rabbitmq">>,
                                                  status = pong,
                                                  additional_attributes =
                                                     [{<<"rabbitmq">>, {[
                                                         {<<"vhost_aliveness">>, {[{<<"status">>, <<"pong">>}]}},
                                                         {<<"node_health">>, {[{<<"status">>, <<"pong">>}]}}]}}]
                                                 }))),
    ?assertEqual({request3, state3}, deliv_hand_status:to_json(request, state)),
    ?verifyAll.

to_json_lsync_down() ->
    application:set_env(delivery, a2_mode, false),
    Body = {[
             {<<"status">>, <<"fail">>},
             {<<"configuration_mode">>, <<"primary">>},
             {<<"fips_mode">>, <<"false">>},
             {<<"upstreams">>, [{[
                 {<<"postgres">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"lsyncd">>, {[
                    {<<"status">>, <<"fail">>}
                   ]}},
                 {<<"elasticsearch">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"rabbitmq">>, {[
                    {<<"status">>, <<"pong">>},
                    {<<"rabbitmq">>, {[
                        {<<"vhost_aliveness">>, {[
                            {<<"status">>, <<"pong">>}
                        ]}},
                        {<<"node_health">>, {[
                            {<<"status">>, <<"pong">>}
                        ]}}
                     ]}}
                  ]}}
                ]}]},
                {<<"a2_mode">>, <<"false">>}
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
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, disaster_recovery_mode]),
                      ?andReturn({ok, primary}))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, deliv_fips_mode]),
                      ?andReturn(undefined))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, a2_mode]),
                      ?andReturn({ok, false}))),
    hoax:mock(deliv_db_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"postgres">>,
                                                  status = pong}))),
    hoax:mock(deliv_lsyncd_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"lsyncd">>,
                                                  status = fail}))),
    hoax:mock(vis_elasticsearch_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"elasticsearch">>,
                                                  status = pong}))),
    hoax:mock(insights_rabbitmq_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"rabbitmq">>,
                                                  status = pong,
                                                  additional_attributes =
                                                     [{<<"rabbitmq">>, {[
                                                         {<<"vhost_aliveness">>, {[{<<"status">>, <<"pong">>}]}},
                                                         {<<"node_health">>, {[{<<"status">>, <<"pong">>}]}}]}}]
                                                 }))),
    ?assertEqual({request3, state3}, deliv_hand_status:to_json(request, state)),
    ?verifyAll.

to_json_not_primary_and_lsync_not_running() ->
    application:set_env(delivery, a2_mode, false),
    Body = {[
             {<<"status">>, <<"pong">>},
             {<<"configuration_mode">>, <<"cold_standby">>},
             {<<"fips_mode">>, <<"false">>},
             {<<"upstreams">>, [{[
                 {<<"postgres">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"lsyncd">>, {[
                    {<<"status">>, <<"not_running">>}
                   ]}},
                 {<<"elasticsearch">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"rabbitmq">>, {[
                    {<<"status">>, <<"pong">>},
                    {<<"rabbitmq">>, {[
                        {<<"vhost_aliveness">>, {[
                            {<<"status">>, <<"pong">>}
                        ]}},
                        {<<"node_health">>, {[
                            {<<"status">>, <<"pong">>}
                        ]}}
                     ]}}
                  ]}}
                ]}]},
                {<<"a2_mode">>, <<"false">>}
            ]},
    Encoded = chef_json:encode(Body),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Body, request, state]),
                      ?andReturn({Encoded, request2, state2}))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, disaster_recovery_mode]),
                      ?andReturn({ok, cold_standby}))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, deliv_fips_mode]),
                      ?andReturn(undefined))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, a2_mode]),
                      ?andReturn({ok, false}))),
    hoax:mock(deliv_db_status,
              ?expect(ping,
                      ?withArgs([cold_standby]),
                      ?andReturn(#status_metadata{service = <<"postgres">>,
                                                  status = pong}))),
    hoax:mock(deliv_lsyncd_status,
              ?expect(ping,
                      ?withArgs([cold_standby]),
                      ?andReturn(#status_metadata{service = <<"lsyncd">>,
                                                  status = not_running}))),
    hoax:mock(vis_elasticsearch_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"elasticsearch">>,
                                                  status = pong}))),
    hoax:mock(insights_rabbitmq_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"rabbitmq">>,
                                                  status = pong,
                                                  additional_attributes =
                                                     [{<<"rabbitmq">>, {[
                                                         {<<"vhost_aliveness">>, {[{<<"status">>, <<"pong">>}]}},
                                                         {<<"node_health">>, {[{<<"status">>, <<"pong">>}]}}]}}]
                                                 }))),
    ?assertEqual({Encoded, request2, state2}, deliv_hand_status:to_json(request, state)),
    ?verifyAll.

to_json_rabbitmq_down_insights_enabled() ->
    application:set_env(delivery, a2_mode, false),
    Body = {[
             {<<"status">>, <<"fail">>},
             {<<"configuration_mode">>, <<"primary">>},
             {<<"fips_mode">>, <<"false">>},
             {<<"upstreams">>, [{[
                 {<<"postgres">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"lsyncd">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"elasticsearch">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"rabbitmq">>, {[
                    {<<"status">>, <<"fail">>},
                    {<<"rabbitmq">>, {[
                        {<<"vhost_aliveness">>, {[
                            {<<"status">>, <<"fail">>}
                        ]}},
                        {<<"node_health">>, {[
                            {<<"status">>, <<"fail">>}
                        ]}}
                     ]}}
                  ]}}
                ]}]},
                {<<"a2_mode">>, <<"false">>}
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
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, disaster_recovery_mode]),
                      ?andReturn({ok, primary}))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, deliv_fips_mode]),
                      ?andReturn(undefined))),
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([delivery, a2_mode]),
                      ?andReturn({ok, false}))),
    hoax:mock(deliv_db_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"postgres">>,
                                                  status = pong}))),
    hoax:mock(deliv_lsyncd_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"lsyncd">>,
                                                  status = pong}))),
    hoax:mock(vis_elasticsearch_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"elasticsearch">>,
                                                  status = pong}))),
    hoax:mock(insights_rabbitmq_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"rabbitmq">>,
                                                  status = fail,
                                                  additional_attributes =
                                                     [{<<"rabbitmq">>, {[
                                                         {<<"vhost_aliveness">>, {[{<<"status">>, <<"fail">>}]}},
                                                         {<<"node_health">>, {[{<<"status">>, <<"fail">>}]}}]}}]
                                                 }))),
    ?assertEqual({request3, state3}, deliv_hand_status:to_json(request, state)),
    ?verifyAll.

to_json_rabbitmq_management_down_insights_enabled() ->
    application:set_env(insights, enabled, true),
    application:set_env(insights, rabbitmq_management_enabled, false),
    application:set_env(delivery, disaster_recovery_mode, primary),
    application:set_env(delivery, a2_mode, false),

    Body = {[
             {<<"status">>, <<"fail">>},
             {<<"configuration_mode">>, <<"primary">>},
             {<<"fips_mode">>, <<"false">>},
             {<<"upstreams">>, [{[
                 {<<"postgres">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"lsyncd">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"elasticsearch">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"rabbitmq">>, {[
                    {<<"status">>, <<"not_running">>},
                    {<<"rabbitmq_management">>, {[
                        {<<"status">>, <<"not_running">>},
                        {<<"description">>, <<"rabbitmq_management plugin is disabled">>}
                    ]}}
                  ]}}
                ]}]},
                {<<"a2_mode">>, <<"false">>}
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
    hoax:mock(deliv_db_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"postgres">>,
                                                  status = pong}))),
    hoax:mock(deliv_lsyncd_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"lsyncd">>,
                                                  status = pong}))),
    hoax:mock(vis_elasticsearch_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"elasticsearch">>,
                                                  status = pong}))),
    ?assertEqual({request3, state3}, deliv_hand_status:to_json(request, state)),
    ?verifyAll.

to_json_rabbitmq_down_insights_disabled() ->
    application:set_env(insights, enabled, false),
    application:set_env(delivery, disaster_recovery_mode, primary),
    application:set_env(delivery, a2_mode, false),
    Body = {[
             {<<"status">>, <<"pong">>},
             {<<"configuration_mode">>, <<"primary">>},
             {<<"fips_mode">>, <<"false">>},
             {<<"upstreams">>, [{[
                 {<<"postgres">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"lsyncd">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"elasticsearch">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"rabbitmq">>, {[
                    {<<"status">>, <<"not_running">>},
                    {<<"rabbitmq">>, {[
                        {<<"status">>, <<"not_running">>},
                        {<<"description">>, <<"RabbitMQ is disabled">>}
                    ]}}
                  ]}}
                ]}]},
                {<<"a2_mode">>, <<"false">>}
            ]},
    Encoded = chef_json:encode(Body),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Body, request, state]),
                      ?andReturn({Encoded, request2, state2}))),
    hoax:mock(deliv_db_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"postgres">>,
                                                  status = pong}))),
    hoax:mock(deliv_lsyncd_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"lsyncd">>,
                                                  status = pong}))),
    hoax:mock(vis_elasticsearch_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"elasticsearch">>,
                                                  status = pong}))),
    ?assertEqual({Encoded, request2, state2}, deliv_hand_status:to_json(request, state)),
    ?verifyAll.

to_json_visibility_insights_disabled() ->
    application:set_env(insights, enabled, false),
    application:set_env(visibility, enabled, false),
    application:set_env(delivery, disaster_recovery_mode, primary),
    application:set_env(delivery, a2_mode, false),
    Body = {[
             {<<"status">>, <<"pong">>},
             {<<"configuration_mode">>, <<"primary">>},
             {<<"fips_mode">>, <<"false">>},
             {<<"upstreams">>, [{[
                 {<<"postgres">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"lsyncd">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"elasticsearch">>, {[
                    {<<"status">>, <<"not_running">>},
                    {<<"description">>, <<"elasticsearch is disabled">>}
                   ]}},
                 {<<"rabbitmq">>, {[
                    {<<"status">>, <<"not_running">>},
                    {<<"rabbitmq">>, {[
                        {<<"status">>, <<"not_running">>},
                        {<<"description">>, <<"RabbitMQ is disabled">>}
                    ]}}
                  ]}}
                ]}]},
                {<<"a2_mode">>, <<"false">>}
            ]},
    Encoded = chef_json:encode(Body),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Body, request, state]),
                      ?andReturn({Encoded, request2, state2}))),
    hoax:mock(deliv_db_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"postgres">>,
                                                  status = pong}))),
    hoax:mock(deliv_lsyncd_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"lsyncd">>,
                                                  status = pong}))),
    ?assertEqual({Encoded, request2, state2}, deliv_hand_status:to_json(request, state)),
    ?verifyAll.

to_json_fips_mode_enabled() ->
    application:set_env(insights, enabled, false),
    application:set_env(delivery, disaster_recovery_mode, primary),
    application:set_env(delivery, deliv_fips_mode, true),
    application:set_env(delivery, a2_mode, false),
    Body = {[
             {<<"status">>, <<"pong">>},
             {<<"configuration_mode">>, <<"primary">>},
             {<<"fips_mode">>, <<"true">>},
             {<<"upstreams">>, [{[
                 {<<"postgres">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"lsyncd">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"elasticsearch">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"rabbitmq">>, {[
                    {<<"status">>, <<"not_running">>},
                    {<<"rabbitmq">>, {[
                        {<<"status">>, <<"not_running">>},
                        {<<"description">>, <<"RabbitMQ is disabled">>}
                    ]}}
                  ]}}
                ]}]},
                {<<"a2_mode">>, <<"false">>}
            ]},
    Encoded = chef_json:encode(Body),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Body, request, state]),
                      ?andReturn({Encoded, request2, state2}))),
    hoax:mock(deliv_db_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"postgres">>,
                                                  status = pong}))),
    hoax:mock(deliv_lsyncd_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"lsyncd">>,
                                                  status = pong}))),
    hoax:mock(vis_elasticsearch_status,
              ?expect(ping,
                      ?withArgs([]),
                      ?andReturn(#status_metadata{service = <<"elasticsearch">>,
                                                  status = pong}))),
    ?assertEqual({Encoded, request2, state2}, deliv_hand_status:to_json(request, state)),
    ?verifyAll.

    to_json_a2_mode() ->
    application:set_env(insights, enabled, false),
    application:set_env(visibility, enabled, false),
    application:set_env(delivery, deliv_fips_mode, false),
    application:set_env(delivery, a2_mode, true),
    Body = {[
             {<<"status">>, <<"pong">>},
             {<<"configuration_mode">>, <<"primary">>},
             {<<"fips_mode">>, <<"false">>},
             {<<"upstreams">>, [{[
                 {<<"postgres">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"lsyncd">>, {[
                    {<<"status">>, <<"pong">>}
                   ]}},
                 {<<"elasticsearch">>, {[
                    {<<"status">>, <<"not_running">>},
                    {<<"description">>, <<"elasticsearch is disabled">>}
                   ]}},
                 {<<"rabbitmq">>, {[
                    {<<"status">>, <<"not_running">>},
                    {<<"rabbitmq">>, {[
                        {<<"status">>, <<"not_running">>},
                        {<<"description">>, <<"RabbitMQ is disabled">>}
                    ]}}
                  ]}}
                ]}]},
                {<<"a2_mode">>, <<"true">>}
            ]},
    Encoded = chef_json:encode(Body),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Body, request, state]),
                      ?andReturn({Encoded, request2, state2}))),
    hoax:mock(deliv_db_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"postgres">>,
                                                  status = pong}))),
    hoax:mock(deliv_lsyncd_status,
              ?expect(ping,
                      ?withArgs([primary]),
                      ?andReturn(#status_metadata{service = <<"lsyncd">>,
                                                  status = pong}))),
    ?assertEqual({Encoded, request2, state2}, deliv_hand_status:to_json(request, state)),
    ?verifyAll.
