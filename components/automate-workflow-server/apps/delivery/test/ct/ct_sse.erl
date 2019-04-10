%% @doc Helpers for interacting with our SSE endpoints
-module(ct_sse).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("delivery/include/deliv_types.hrl").

-export([req/4,
         auth_req/5,
         auth_get/2,
         parse_event/3,
         wait_for_closing_sse_event/1,
         wait_for_single_sse_event/1,
         wait_for_sse_events/1]).

-type shotgun_event() :: map:map().

%% @doc Opens a connection with a SSE endpoint
%% Returns the PID of a shotgun connection
%% Same as `http_test_helpers:req', will encode any JSON body
-spec req(http_method(), iodata(),
          binary() | json(),
          http_headers(str_or_binary())) -> pid().
req(Method, Route, Json, ReqHeaders) when erlang:is_tuple(Json)
                                   orelse erlang:is_list(Json) ->
    NewHeaders = [{"content-type", "application/json"} | ReqHeaders ],
    req(Method, Route, chef_json:encode(Json), NewHeaders);
req(Method, Route, BinBody, ReqHeaders) ->
    %% start shotgun if necessary
    delivery_app:start_app_with_deps(shotgun),

    Host = app_test_helpers:host(),
    Port = app_test_helpers:listen_port(),
    {ok, Conn} = shotgun:open(Host, Port),
    ct:pal("SSE request with: ~p", [{Method, Route, ReqHeaders, BinBody}]),

    {ok, Ref} = shotgun:request(Conn,
                                Method,
                                chef_utils:iodata_to_str(Route),
                                shotgunize_headers(ReqHeaders),
                                BinBody,
                                default_shotgun_options()),

    case erlang:is_reference(Ref) of
        true ->
            Conn;
        false ->
            %% means that the returned status wasn't 200
            ct:pal("Unsuccessful SSE request: ~p", [Ref]),
            ?assert(false)
    end.

%% @private
default_shotgun_options() ->
    #{async => true,
      async_mode => sse}.

%% @private
%% @doc Shotgun expects its headers as a map
shotgunize_headers(Headers) ->
    List = [{chef_utils:to_str(K), chef_utils:to_str(V)} || {K, V} <- Headers],
    maps:from_list(List).

%% @doc Same as `req', but with authentication
-spec auth_req({d_user() | binary(), binary()}, http_method(),
               iodata(), binary() | json(),
               http_headers(str_or_binary())) -> pid().
auth_req({User, Token}, Method, Route, ReqBody, ReqHeaders) when erlang:is_tuple(User) ->
    auth_req({deliv_user:getval(name, User), Token}, Method, Route, ReqBody, ReqHeaders);
auth_req({UserName, Token}, Method, Route, ReqBody, ReqHeaders) when erlang:is_binary(UserName) ->
    req(Method, Route, ReqBody, http_test_helpers:add_auth_headers(UserName, Token, ReqHeaders)).

%% @doc An alis for `auth_req/5' for a GET request with no body and no headers beside
%% the auth ones
-spec auth_get({d_user() | binary()}, iodata()) -> pid().
auth_get(AuthData, Route) ->
    auth_req(AuthData, get, Route, <<>>, []).

%% @doc A helper to parse a shotgun event
%% Returns the JSON data
-spec parse_event(shotgun_event(), binary(), fin | nofin) -> json().
parse_event(SSEEvent, ExpectedEventType, ExpectedEOM) ->
    ct:pal("Parsing SSE event: ~p", [SSEEvent]),
    {EOM, _Pid, RawSSE} = SSEEvent,
    ?assertEqual({'connection_closed?', ExpectedEOM},
                 {'connection_closed?', EOM}),

    ParsedEvent = shotgun:parse_event(RawSSE),

    ?assertEqual(32, erlang:size(maps:get(id, ParsedEvent))),
    ?assertEqual({event_type, ExpectedEventType},
                 {event_type, maps:get(event, ParsedEvent)}),

    chef_json:decode(maps:get(data, ParsedEvent)).

%% @doc Waits for an event closing the connection
%% discards all others events it might receive
-spec wait_for_closing_sse_event(pid()) -> {ok, shotgun_event()} | {error, timed_out}.
wait_for_closing_sse_event(ShotgunPid) ->
    FilterFun = fun({EOM, _, _}) -> EOM =:= fin end,
    assert_single_event(wait_for_sse_events(ShotgunPid, FilterFun)).

%% @doc Waits for any SSE event
%% Crashes if more than one event comes in
-spec wait_for_single_sse_event(pid()) -> {ok, shotgun_event()} | {error, timed_out}.
wait_for_single_sse_event(ShotgunPid) ->
    assert_single_event(wait_for_sse_events(ShotgunPid)).

assert_single_event({error, _Why} = Error) ->
    Error;
assert_single_event([NewEvent]) ->
    {ok, NewEvent};
assert_single_event(MultipleEvents) when erlang:is_list(MultipleEvents) ->
    ct:pal("Unexpectedly received more than one SSE event: ~p", [MultipleEvents]),
    ?assert(false).

%% @doc Waits until it receives one or more event
-spec wait_for_sse_events(pid()) -> [shotgun_event()] | {error, timed_out}.
wait_for_sse_events(ShotgunPid) ->
    wait_for_sse_events(ShotgunPid, fun(_) -> true end).

wait_for_sse_events(ShotgunPid, FilterFun) ->
    wait_for_sse_events(ShotgunPid, 6, 250, FilterFun).

wait_for_sse_events(_ShotgunPid, 0, _RetryInterval, _FilterFun) ->
    ct:pal("Timed out while waiting for a SSE event"),
    {error, timed_out};
wait_for_sse_events(ShotgunPid, Retries, RetryInterval, FilterFun) ->
    timer:sleep(RetryInterval),
    NewEvents = shotgun:events(ShotgunPid),
    case lists:filter(FilterFun, NewEvents) of
        [] ->
            wait_for_sse_events(ShotgunPid, Retries - 1, RetryInterval, FilterFun);
        FilteredEvents ->
            FilteredEvents
    end.
