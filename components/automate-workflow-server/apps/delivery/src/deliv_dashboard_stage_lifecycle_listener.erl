-module(deliv_dashboard_stage_lifecycle_listener).

-behaviour(gen_server).

-include("deliv_types.hrl").
-include("deliv_events.hrl").
-include("deliv_coordinates.hrl").

%% Public API
-export([
         start_link/0,
         subscribe/1,
         unsubscribe/1,
         ejson/1
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

-define(SUBSCRIBER_KEY(EntName), {dashboard_updated, EntName}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec subscribe(binary()) -> any().
subscribe(EntName) ->
    gen_server:call(?MODULE, {subscribe, EntName}).

unsubscribe(EntName) ->
    deliv_event:unsubscribe(?SUBSCRIBER_KEY(EntName)).

ejson(EntName) ->
    gen_server:call(?MODULE, {ejson, EntName}).

init([]) ->
    deliv_stage:subscribe_stage_events(),
    deliv_change:subscribe_change_events(),
    {ok, dict:new()}.

handle_call({subscribe, EntName}, {From, _FromRef}, State) ->
  %% We want to subscribe and then give_away our subscription
  %% to ensure that there's no timing issues in registration.
  %% If we subscribe in the process space of the caller, it is
  %% possible that a stage event will occur that will invalidate
  %% what we return from this fun head, creating dirty state in
  %% the caller. By subscribing/give_away here, we guarantee no
  %% new events can be generated during the subscription process
  %% and that the result of this fun head will be accurate at time
  %% of receipt.
  Key = ?SUBSCRIBER_KEY(EntName),
  deliv_event:subscribe(Key),
  gproc:give_away({p, l, Key}, From),
  {Ejson, UpdatedState} = find_or_update(EntName, State),
  {reply, {ok, Ejson}, UpdatedState};
handle_call({ejson, EntName}, _From, State) ->
  {Ejson, UpdatedState} = find_or_update(EntName, State),
  {reply, {ok, Ejson}, UpdatedState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({_Originator, {{stage,_}, _},  #stage_event{scope = Scope}}, State) ->
    EntName = deliv_scopes:'#get'(ent_name, Scope),
    notify_and_noreply(EntName, State);
handle_info({_Originator, change_updated,  DelivChange}, State) ->
    Scope = deliv_scopes:from_change(DelivChange),
    #proj_coordinates{ent_name = EntName} = deliv_scopes:scope_to_coordinates(Scope),
    notify_and_noreply(EntName, State);
handle_info({_Originator, change_created,  DelivChange}, State) ->
    Scope = deliv_scopes:from_change(DelivChange),
    #proj_coordinates{ent_name = EntName} = deliv_scopes:scope_to_coordinates(Scope),
    notify_and_noreply(EntName, State);
handle_info({_Originator, change_deleted, {{change_id, _ChangeId}, {deleted_by, Deleter}}}, State) ->
    {EntName} = deliv_user:scoping_names(Deleter),
    notify_and_noreply(EntName, State);
handle_info({_Originator, change_approved, DelivChange}, State) ->
    Scope = deliv_scopes:from_change(DelivChange),
    #proj_coordinates{ent_name = EntName} = deliv_scopes:scope_to_coordinates(Scope),
    notify_and_noreply(EntName, State);
handle_info({_Originator, change_superseded, DelivChange}, State) ->
    Scope = deliv_scopes:from_change(DelivChange),
    #proj_coordinates{ent_name = EntName} = deliv_scopes:scope_to_coordinates(Scope),
    notify_and_noreply(EntName, State);
handle_info({_Originator, change_delivered, DelivChange}, State) ->
    Scope = deliv_scopes:from_change(DelivChange),
    #proj_coordinates{ent_name = EntName} = deliv_scopes:scope_to_coordinates(Scope),
    notify_and_noreply(EntName, State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

notify_and_noreply(EntName, State) ->
    {ok, Ejson} = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),
    deliv_event:publish(?SUBSCRIBER_KEY(EntName), Ejson),
    {noreply, dict:store(EntName, Ejson, State)}.

find_or_update(EntName, State) ->
  case dict:find(EntName, State) of
    error ->
      {ok, Ejson} = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),
      {Ejson, dict:store(EntName, Ejson, State)};
    {ok, Ejson} ->
      {Ejson, State}
  end.
