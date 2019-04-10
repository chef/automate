-module(insights_sup_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-compile(export_all).

insights_sup_test_() ->
    [
     hoax:fixture(?MODULE, init_)
    ].

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link_calls_supervisor_test() ->
    hoax:test(fun() ->
        Enabled = {ok, setting_from_sys_config},

        hoax:mock(application,
                  ?expect(get_env,
                          ?withArgs([insights, enabled]),
                          ?andReturn(Enabled))),

        hoax:mock(supervisor,
                  ?expect(start_link,
                          ?withArgs([{local, insights_sup}, insights_sup, Enabled]))),

        insights_sup:start_link(),

        ?verifyAll
    end).

init_returns_child_specs_with_procs_when_insights_is_enabled() ->
    HostIP = {127,0,0,1},
    Port = 5602,
    User = <<"Insights">>,
    Password = <<"chefrocks">>,
    Vhost = <<"/insights">>,
    Ssl = true,
    InsightsExchange = {#'exchange.declare'{
                           exchange= <<"insights">>,
                           type= <<"topic">>,
                           durable=true
                          }
                       },
    HabitatExchange = {#'exchange.declare'{
                          exchange= <<"habitat">>,
                          type= <<"topic">>,
                          durable=true
                         }
                      },
    hoax:expect(receive
                    envy_parse:host_to_ip(insights, rabbitmq_host) -> HostIP;
                    envy:get(insights, rabbitmq_port, non_neg_integer) -> Port;
                    envy:get(insights, rabbitmq_user, binary) -> User;
                    envy:get(insights, rabbitmq_password, binary) -> Password;
                    envy:get(insights, rabbitmq_vhost, binary) -> Vhost;
                    envy:get(insights, rabbitmq_ssl, boolean) -> Ssl
                end),

    Network = {network, #amqp_params{username = User,
                                     password = Password,
                                     virtual_host = Vhost,
                                     host = HostIP,
                                     port = Port,
                                     ssl_options = []}},
    Expected = {ok, { {one_for_one, 5, 10}, [
        {insights_queue, {bunnyc, start_link, [insights_queue, Network, InsightsExchange, []]},
            permanent, 5000, worker, dynamic},
        {habitat_queue, {bunnyc, start_link, [habitat_queue, Network, HabitatExchange, []]},
            permanent, 5000, worker, dynamic},
        ?CHILD(insights_stage_event_listener, worker),
        ?CHILD(insights_change_event_listener, worker),
        ?CHILD(insights_patchset_event_listener, worker),
        ?CHILD(insights_phase_event_listener, worker)

    ]}},

    Actual = insights_sup:init({ok, true}),
    ?assertEqual(Expected, Actual),
    ?verifyAll.

init_returns_child_specs_without_procs_when_insights_is_not_enabled() ->
    Return = insights_sup:init(anything_besides_ok_true),
    Expected = {ok, { {one_for_one, 5, 10}, []}},

    ?assertEqual(Expected, Return).
