%% @doc This is a generic behavior for our endpoints that simply
%% stream erlang events to HTTP
%% Such endpoints should implement this behavior, make their `init/3'
%% look like this:
%%    init(_Transport, Req, State) ->
%%        deliv_web_sse:init(?MODULE, Req, State).
%% and should finally also mixin `handle/2', `info/3' and
%% `terminate/3'.
%%
%% Additionally, said modules can choose wether to export a few
%% other callbacks:
%%  * `timeout/0': the websocket's timeout (defaults to `?DEFAULT_TIMEOUT')
%%  * `forbidden_callback/0': the name of the `deliv_authz:forbidden*/2' fun
%%     to use to authZ requests (defaults to `forbidden')
%%  * `post_auth/2': called after a successful authZ/authN, and should
%%     implement the following spec:
%%        -spec post_auth(Req :: cowboy_req(), State :: #handler{})
%%                -> {ok | error, cowboy_req(), #handler{}}.
%%     Comes in handy to make some additional checks before starting to
%%     send a response.
%%
%% More doc at
%% http://ninenines.eu/docs/en/cowboy/1.0/guide/loop_handlers/

-module(deliv_web_sse).

-include("deliv_types.hrl").

%% Cowboy callbacks to be mixed-in
-export([handle/2,
         info/3,
         terminate/3]).

-export([init/3,
         format_event/3]).

%% @doc The events this endpoint is streaming (and that we will
%% subscribe to on init)
-callback events(cowboy_req(), #handler{}) -> {cowboy_req(), #handler{}, list(deliv_event:event_type())}.

%% @doc Should format the event into the data that will get streamed
%% to the HTTP client
%% The first atom says whether we should terminate the connection
%% after sending the current `Data'
-callback format_event(deliv_event:event_type(), Msg :: any(),
                       cowboy_req(), #handler{}) -> {keep_open | close, cowboy_req(), #handler{}, Data :: iodata() | no_data}.

%% 1 minute
-define(DEFAULT_TIMEOUT, 60000).

%% A wrapper record for the generic `handler' record used for all requests
%% (a.k.a let's not pollute everyone with our custom fields)
-record(handler_wrapper, {
          handler :: #handler{},
          %% List of keys for subscribed events.
          event_subscriptions = [] :: list(deliv_event:event_type()),
          %% name of the SSE module, needed for the callbacks
          sse_module :: atom()
         }).

%% @doc If we fail authN/Z we won't enter the loop handler and so
%% this callback needs to be present, but we will have already sent
%% the response.
handle(Req, State) ->
    {ok, Req, State}.

-spec init(atom(), cowboy_req(), #handler{}) ->
                  {loop, cowboy_req(), #handler_wrapper{}, Timeout :: non_neg_integer()}
                      | {ok, cowboy_req(), #handler_wrapper{}}.
init(deliv_hand_license_sse, Req, #handler{} = State) ->
    HandlerWrapper = #handler_wrapper{handler = State,
                                      sse_module = deliv_hand_license_sse},
    init_event_stream(Req, HandlerWrapper);
init(SseModule, Req, #handler{} = State) ->
    HandlerWrapper = #handler_wrapper{handler = State,
                                      sse_module = SseModule},
    lists:foldl(fun apply_fun/2,
                {continue, Req, HandlerWrapper},
                [
                 fun is_authorized_no_rest/2,
                 fun forbidden_no_rest/2,
                 fun post_auth/2,
                 fun init_event_stream/2
                ]).

%% @private
apply_fun(Fun, {continue, Req, HandlerWrapper}) ->
    Fun(Req, HandlerWrapper);
apply_fun(_, {halt, Req, HandlerWrapper}) ->
    %% This is the cowboy return tuple to indicate that we are
    %% done processing the request.
    {ok, Req, HandlerWrapper};
apply_fun(_, {ok, _Req, _HandlerWrapper} = RetVal) ->
    RetVal.

%% @private
is_authorized_no_rest(Req, #handler_wrapper{handler = State} = HandlerWrapper) ->
    case deliv_token:is_authorized(Req, State) of
        {{false, HeaderVal}, Req1, State1} ->
            Req2 = cowboy_req:set_resp_header(<<"www-authenticate">>,
                                              HeaderVal, Req1),
            {ok, Req3} = cowboy_req:reply(401, Req2),
            {halt, Req3, State1};
        {true, Req1, State1} ->
            {continue, Req1, HandlerWrapper#handler_wrapper{handler = State1}};
        {halt, _, _} = Result ->
            Result
    end.

%% @private
forbidden_no_rest(Req, #handler_wrapper{handler = State} = HandlerWrapper) ->
    ForbiddenCallbackName = forbidden_callback(HandlerWrapper),
    case deliv_authz:ForbiddenCallbackName(Req, State) of
        {false, Req1, State1} ->
            {continue, Req1, HandlerWrapper#handler_wrapper{handler = State1}};
        {true, Req1, State1} ->
            {ok, Req2} = cowboy_req:reply(403, Req1),
            {halt, Req2, State1};
        {halt, Req1, State1} ->
            {halt, Req1, State1}
    end.

%% @private
forbidden_callback(HandlerWrapper) ->
    call_if_exported0(HandlerWrapper, forbidden_callback, forbidden).

%% @private
post_auth(Req, #handler_wrapper{handler = State} = HandlerWrapper) ->
    case call_if_exported(HandlerWrapper, post_auth,
                          [Req, State], {ok, Req, State}) of
        {ok, Req1, State1} ->
            {continue, Req1, HandlerWrapper#handler_wrapper{handler = State1}};
        {error, Req1, State1} ->
            {halt, Req1, State1}
    end.

%% @private
init_event_stream(Req, #handler_wrapper{sse_module = SseModule} = HandlerWrapper) ->
    RespHeaders = [{<<"content-type">>, <<"text/event-stream">>},
                   {<<"cache-control">>, <<"no-cache">>}],
    %% a word on the chunking: SSE specs seem to advise against it
    %% see http://www.w3.org/TR/2009/WD-eventsource-20091029/ :
    %% "Authors are also cautioned that HTTP chunking can have unexpected
    %% negative effects on the reliability of this protocol. Where possible,
    %% chunking should be disabled for serving event streams unless the rate of
    %% messages is high enough for this not to matter."
    %% However, disabling the chunking seems to result in traffic
    %% unintelligible to any client but curl (and even then, with a warning)
    {ok, Req1} = cowboy_req:chunked_reply(200, RespHeaders, Req),
    {Req2, HandlerWrapper1} = subscribe(Req1, HandlerWrapper),
    Timeout = call_if_exported0(HandlerWrapper, timeout, ?DEFAULT_TIMEOUT),

    %% from the doc: "It is recommended that you set the connection header to
    %% `close' when replying, as this process may be reused for a subsequent
    %% request."
    Req3 = cowboy_req:set_resp_header(<<"connection">>, <<"close">>, Req2),

    Events = HandlerWrapper1#handler_wrapper.event_subscriptions,
    chef_log:debug("Initializing event stream for ~s, listening to events: ~p",
                   [SseModule, Events]),
    {loop, Req3, HandlerWrapper1, Timeout}.

%% @private
subscribe(Req, #handler_wrapper{handler = State,
                                sse_module = SseModule} = HandlerWrapper) ->
    {Req1, State1, EventKeys} = SseModule:events(Req, State),
    true = deliv_event:subscribe(EventKeys),
    {Req1, HandlerWrapper#handler_wrapper{handler = State1,
                                          event_subscriptions = EventKeys}}.

%% @doc Cowboy callback when a message is received
info({Pid, EventKey, Msg} = What, Req, #handler_wrapper{handler = State,
                                                        event_subscriptions = EventKeys,
                                                        sse_module = SseModule} = HandlerWrapper)
  when is_pid(Pid) andalso
       (is_atom(EventKey) orelse is_tuple(EventKey)) ->
    %% is this an event we're listening to?
    case lists:member(EventKey, EventKeys) of
        true ->
            chef_log:debug("EVENT ~p in ~s from ~p -> ~p\n", [EventKey, SseModule, Pid, Msg]),
            {Action, Req1, State1, IOData} = SseModule:format_event(EventKey, Msg, Req, State),
            ok = maybe_send_chunk(IOData, Req1),
            NewHandlerWrapper = HandlerWrapper#handler_wrapper{handler = State1},
            maybe_close_connection(Action, Req1, NewHandlerWrapper);
        false ->
            unexpected_message(What, Req, HandlerWrapper)
    end;
info(What, Req, HandlerWrapper) ->
    unexpected_message(What, Req, HandlerWrapper).

maybe_send_chunk(no_data, _Req) -> ok;
maybe_send_chunk(IOData, Req) -> cowboy_req:chunk(IOData, Req).

maybe_close_connection(keep_open, Req, HandlerWrapper) ->
    {loop, Req, HandlerWrapper};
maybe_close_connection(close, Req, HandlerWrapper) ->
    %% also, make sure the response buffer is empty
    ok = cowboy_req:ensure_response(Req, 200),
    {ok, Req, HandlerWrapper}.

unexpected_message(What, Req, #handler_wrapper{sse_module = SseModule} = HandlerWrapper) ->
    chef_log:info("Unexpected message in ~p: ~p", [What, SseModule]),
    {loop, Req, HandlerWrapper}.

terminate(_Reason, _Req, #handler{}) ->
    ok;
terminate(_Reason, _Req, #handler_wrapper{} = HandlerWrapper) ->
    ok = unsubscribe(HandlerWrapper).

unsubscribe(#handler_wrapper{event_subscriptions = EventKeys}) ->
    true = deliv_event:unsubscribe(EventKeys),
    ok.

%% @doc A helper fun to wrap a data's event
-spec format_event(iodata(), iodata(),
                   {iodata, iodata()} |
                   {ejson, json()}) -> iodata().
format_event(Id, EventType, {ejson, EJSON}) ->
    format_event(Id, EventType, {iodata, chef_json:encode(EJSON)});
format_event(Id, EventType, {iodata, Data}) ->
    %% don't delete the trailing spaces after the colons, the specs
    %% do require them
    [<<"id: ">>, Id, $\n,
     <<"event: ">>, EventType, $\n,
     <<"data: ">>, Data, $\n, $\n].

%% @private
call_if_exported(#handler_wrapper{sse_module = SseModule}, CallbackName, Args, Default) ->
    Arity = erlang:length(Args),
    case erlang:function_exported(SseModule, CallbackName, Arity) of
        true -> erlang:apply(SseModule, CallbackName, Args);
        false -> Default
    end.

call_if_exported0(HandlerWrapper, CallbackName, Default) ->
    call_if_exported(HandlerWrapper, CallbackName, [], Default).
