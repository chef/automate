%% @doc A test implementation of the insights_listener behaviour.
-module(insights_listener_test_impl).

-include("insights.hrl").

-behaviour(insights_listener).

%% API functions
-export([start_link/0,
         subscribe_to_events/0,
         handle_event/2]).

-spec start_link() -> {ok, pid()}.
start_link() -> insights_listener:start_link(?MODULE).

-spec subscribe_to_events() -> boolean().
subscribe_to_events() -> true.

-spec handle_event(atom(), term()) -> #insights_event{} | {error, unhandled_event}.
handle_event(_Subject, _Msg) -> #insights_event{}.
