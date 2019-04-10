-module(deliv_event).

-export([
         publish/2,
         subscribe/1,
         unsubscribe/1,
         unsubscribe_all/0
        ]).

-type event_type() :: tuple() | atom().
-export_type [event_type/0].

%% @doc Publish message payload `Msg' with event type `EventType'. All
%% processes that subscribe to `EventType' events will receive the
%% message.
-spec publish(EventType, Msg) -> Msg when
      EventType :: event_type(),
      Msg       :: any().
publish(EventType, Msg) when is_tuple(EventType) orelse is_atom(EventType) ->
    gproc:send({p, l, EventType}, {self(), EventType, Msg}).

%% @doc Subscribe to messages published with the specified
%% `EventType'. You can provide a list of event types to subscribe to.
-spec subscribe(event_type() | [event_type()]) -> true.
subscribe(EventTypeList) when is_list(EventTypeList) ->
    KVs = [ {EventType, undefined} || EventType <- EventTypeList ],
    gproc:mreg(p, l, KVs);
subscribe(EventType) when is_tuple(EventType) orelse is_atom(EventType) ->
    gproc:reg({p, l, EventType}).

%% @doc Unsubscribe from an event type. You can also specify a list of
%% event types to unsubscribe from.
-spec unsubscribe(event_type() | [event_type()]) -> true.
unsubscribe(EventTypeList) when is_list(EventTypeList) ->
    gproc:munreg(p, l, EventTypeList);
unsubscribe(EventType) when is_tuple(EventType) orelse is_atom(EventType) ->
    gproc:unreg({p, l, EventType}).

%% @doc Unsubscribe the current process from all events. This is a
%% convenience and efficiency function. Note that it presently is just
%% a wrapper around `gproc:goodbye/0' and may be dangerous if you are
%% using gproc for more than event pub/sub.
-spec unsubscribe_all() -> ok.
unsubscribe_all() ->
    gproc:goodbye().
