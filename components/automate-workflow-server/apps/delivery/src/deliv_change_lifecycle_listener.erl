-module(deliv_change_lifecycle_listener).

-behaviour(gen_server).

-include("deliv_types.hrl").
-include("deliv_events.hrl").

%% Public API
-export([
         start_link/0
        ]).

%% Behaviour callbacks
-export([
         init/1,

         handle_call/3,
         handle_cast/2,
         handle_info/2,

         terminate/2,
         code_change/3
        ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    deliv_patchset:subscribe_patchset_events(),
    deliv_phase:subscribe_phase_events(started, all),
    {ok, stateless}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({_Pid, patchset_created, Patchset}, State) ->
    case deliv_patchset:getval(sequence_number, Patchset) of
        1 ->
            publish_event(deliv_change:fetch_by_id(deliv_patchset:getval(change_id, Patchset)));
        _ ->
            ok
    end,
    {noreply, State};
handle_info({_Pid, {{phase, started}, _Phase}, #phase_event{change = Change}}, State) ->
    ChangeId = deliv_change:getval(id, Change),
    %% TODO: We should look at enhancing the event modules so we don't have to
    %% publish two events here.
    deliv_event:publish({build_event_for_change, ChangeId}, Change),
    deliv_event:publish(build_event_for_change, Change),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private

%% @doc Publish the event only if we are able to successfully grab the change
publish_event({ok, Change}) ->
    deliv_event:publish(change_created, Change);
publish_event({error, Why}) ->
    chef_log:failed_call(?MODULE, publish_event, [], Why).
