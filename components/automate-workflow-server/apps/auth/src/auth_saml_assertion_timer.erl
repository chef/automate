-module(auth_saml_assertion_timer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("auth_types.hrl").
-include_lib("esaml/include/esaml.hrl").

%% API functions
-export([check_known_id/1,
         check_and_set_digest/2,
         generate_and_track_unique_id/0,
         start_link/0,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-spec check_and_set_digest(binary(), non_neg_integer()) -> boolean().
check_and_set_digest(Digest, TimeoutMSecs) ->
    gen_server:call(?SERVER, {digest, Digest, TimeoutMSecs}).

-spec generate_and_track_unique_id() -> binary().
generate_and_track_unique_id() ->
    ID = esaml_util:unique_id(),
    gen_server:call(?SERVER, {track_id, ID, ?LOGIN_TIMEOUT_MILLIS}).

-spec check_known_id(binary()) -> boolean().
check_known_id(ID) ->
    gen_server:call(?SERVER, {check_id, ID}).

stop() ->
    gen_server:call(?SERVER, stop).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, sets:new()}.

handle_call({digest, Digest, TimeoutMSecs}, _From, State) ->
    case sets:is_element(Digest, State) of
        true ->
            {reply, true, State};
        _ ->
            erlang:send_after(TimeoutMSecs, self(), {delete, Digest}),
            {reply, false, sets:add_element(Digest, State)}
    end;
handle_call({track_id, ID, TimeoutMSecs}, _From, State) ->
    erlang:send_after(TimeoutMSecs, self(), {delete, ID}),
    {reply, ID, sets:add_element(ID, State)};
handle_call({check_id, ID}, _From, State) ->
    case sets:is_element(ID, State) of
        true ->
            erlang:send_after(0, self(), {delete, ID}),
            {reply, true, State};
        false ->
            {reply, false, State}
    end;
handle_call(stop, _From, State) ->
    {stop,normal,ok,State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({delete, Digest}, State) ->
    {noreply, sets:del_element(Digest, State)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
