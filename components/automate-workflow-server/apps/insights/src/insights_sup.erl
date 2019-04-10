-module(insights_sup).

-behaviour(supervisor).

-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    Enabled = application:get_env(insights, enabled),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Enabled).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({ok, true}) ->
    chef_log:info("Visibility Enabled - collecting events"),
    Procs = [ amqp_child_spec(insights_queue, <<"insights">>),
              amqp_child_spec(habitat_queue, <<"habitat">>),
              ?CHILD(insights_stage_event_listener, worker),
              ?CHILD(insights_change_event_listener, worker),
              ?CHILD(insights_patchset_event_listener, worker),
              ?CHILD(insights_phase_event_listener, worker)
    ],
    {ok, { {one_for_one, 5, 10}, Procs} };
init(_) ->
    chef_log:info("Visibility Disabled"),
    {ok, { {one_for_one, 5, 10}, []} }.

amqp_child_spec(ChildName, ExchgName) ->
    Host = envy_parse:host_to_ip(insights, rabbitmq_host),
    Port = envy:get(insights, rabbitmq_port, non_neg_integer),
    User = envy:get(insights, rabbitmq_user, binary),
    Password = envy:get(insights, rabbitmq_password, binary),
    SslOpts = case envy:get(insights, rabbitmq_ssl, boolean) of
                  true -> []; %% Enabled, but no custom options for now
                  false -> none
              end,
    VHost = envy:get(insights, rabbitmq_vhost, binary),

    Exchange = {#'exchange.declare'{exchange=ExchgName,
                                    type= <<"topic">>,
                                    durable=true
                                   }
               },

    Network = {network, #amqp_params{username = User,
                                     password = Password,
                                     virtual_host = VHost,
                                     host = Host,
                                     port = Port,
                                     ssl_options = SslOpts
                                    }
              },
    {ChildName, {bunnyc, start_link, [ChildName, Network, Exchange, []]},
      permanent, 5000, worker, dynamic}.
