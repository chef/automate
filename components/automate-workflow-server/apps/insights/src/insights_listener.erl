%% @doc Behaviour implementation for creating a gproc listener that will listen
%% events, convert them to ejson and forward them to our ingester.
-module(insights_listener).

-behaviour(gen_server).

-include("insights.hrl").

%% API functions
-export([start_link/1,
         new_event/3,
         ingest/3]).

%% gen_server callbacks
-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%% Give the implementation a chance to subscribe to events
-callback subscribe_to_events() -> boolean().

%% Handle each different event that we receive via handle_info
-callback handle_event(EventSubject :: atom(),
                       EventBody :: term()) -> #insights_event{} |
                                               {error, unhandled_event}.

-spec new_event(atom(), atom(), json()) -> #insights_event{}.
new_event(Type, Action, Ejson) ->
    {ok, Hostname} = application:get_env(delivery, hostname),
    #insights_event{
        source = delivery,
        source_fqdn = Hostname,
        type = Type,
        action = Action,
        ejson = Ejson
    }.

%% For all your direct ingestion needs
ingest(Type, Action, Ejson) ->
    insights_ingester:publish(chef_json:encode(event_to_ejson(new_event(Type, Action, Ejson)))).

-spec start_link(atom()) -> {ok, pid()}.
start_link(ImplModule) ->
    gen_server:start_link({local, ImplModule}, ?MODULE, {ImplModule}, []).

-spec init({atom()}) -> {ok, atom()}.
init({ImplModule}) ->
    ImplModule:subscribe_to_events(),
    {ok, ImplModule}.

code_change(_OldVsn, ImplModule, _Extra) ->
    {ok, ImplModule}.

handle_call(_Req, _From, ImplModule) ->
    {reply, ok, ImplModule}.

handle_cast(_Req, ImplModule) ->
    {noreply, ImplModule}.

%% @doc Listen for a generic event to be received. We expect the impl to
%% handle unsupported events.
handle_info({_Pid, EventType, EventBody}, ImplModule) ->
    case ImplModule:handle_event(EventType, EventBody) of
        #insights_event{} = Record ->
            insights_ingester:publish(chef_json:encode(event_to_ejson(Record)));
        {error, unhandled_event} ->
            chef_log:debug("unhandled_insights_event; module=~p; subject=~p; body=~p",
                            [ImplModule, EventType, EventBody])
    end,
    {noreply, ImplModule};
handle_info(_Info, ImplModule) ->
    {noreply, ImplModule}.

terminate(_Reason, _ImplModule) ->
    ok.

%% @private

%% @doc Convert insights_event record into ejson
event_to_ejson(#insights_event{ejson = Ejson} = Record) ->
    lists:foldl(
        fun({Key, Value}, EjsonAcc) ->
            ej:set({chef_utils:to_bin(Key)}, EjsonAcc, chef_utils:to_bin(Value))
        end,
        Ejson,
        insights_event_metadata(Record)
    ).


%% @doc Extract metadata values from Record and return as proplist
insights_event_metadata(#insights_event{source = Source, type = Type, action = Action}) ->
    [
     {event_type, Type},
     {event_action, Action},
     {source, Source}
    ].
