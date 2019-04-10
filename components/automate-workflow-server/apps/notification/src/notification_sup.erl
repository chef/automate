-module(notification_sup).
-behaviour(supervisor).

-include("notification_macros.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Procs = [
        {
            notification_notifier_smtp,
            {notification_event_listener, start_link, [notification_notifier_smtp]},
            permanent,
            5000,
            worker,
            [notification_event_listener]
        },
        {
            notification_notifier_slack,
            {notification_event_listener, start_link, [notification_notifier_slack]},
            permanent,
            5000,
            worker,
            [notification_event_listener]
        }
    ],
    {ok, { {one_for_one, 5, 10}, Procs} }.
