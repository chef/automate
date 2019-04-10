%% @doc Listen for patchset events
-module(insights_patchset_event_listener).

-behaviour(insights_listener).

-include("insights.hrl").

%% insights_listener callbacks
-export([start_link/0,
         subscribe_to_events/0,
         handle_event/2]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    insights_listener:start_link(?MODULE).

%% Gen Server Callbacks
-spec subscribe_to_events() -> boolean().
subscribe_to_events() ->
    deliv_patchset:subscribe_patchset_events().

-spec handle_event(atom(), term()) -> #insights_event{} | {error, unhandled_event}.
handle_event(patchset_created, PatchsetEvent) ->
    {ok, {PatchsetToEjsonProplist}} = deliv_patchset:to_ejson(deliv_scopes:from_patchset(PatchsetEvent), PatchsetEvent),
    LocalToProplist = patchset_event_to_proplist(PatchsetEvent),
    PatchsetProplist = chef_utils:merge_proplists(PatchsetToEjsonProplist, LocalToProplist),
    insights_listener:new_event(patchset, created, {PatchsetProplist});
handle_event(_, _) -> {error, unhandled_event}.

patchset_event_to_proplist(PatchsetEvent) ->
    [
        {<<"patchset_id">>, deliv_patchset:getval(id, PatchsetEvent)},
        {<<"change_id">>, deliv_patchset:getval(change_id, PatchsetEvent)},
        {<<"sequence_number">>, deliv_patchset:getval(sequence_number, PatchsetEvent)},
        {<<"submitted_at">>, deliv_patchset:getval(submitted_at, PatchsetEvent)},
        {<<"sha">>, deliv_patchset:getval(sha, PatchsetEvent)},
        {<<"submitter_id">>, deliv_patchset:getval(submitter_id, PatchsetEvent)},
        {<<"verified_against_sha">>, deliv_patchset:getval(verified_against_sha, PatchsetEvent)},
        {<<"is_verified">>, deliv_patchset:getval(is_verified, PatchsetEvent)},
        {<<"status">>, deliv_patchset:getval(status, PatchsetEvent)}
    ].
