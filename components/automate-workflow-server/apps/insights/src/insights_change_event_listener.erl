%% @doc Listen for change events
-module(insights_change_event_listener).

-behaviour(insights_listener).

-include("insights.hrl").

%% insights_listener callbacks
-export([start_link/0,
         subscribe_to_events/0,
         handle_event/2]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    insights_listener:start_link(?MODULE).

-spec subscribe_to_events() -> boolean().
subscribe_to_events() ->
    deliv_change:subscribe_change_events().

-spec handle_event(atom(), term()) -> #insights_event{} | {error, unhandled_event}.
handle_event(change_updated, Change) ->
    insights_listener:new_event(change, updated, change_ejson(Change));
handle_event(change_created, Change) ->
    insights_listener:new_event(change, created, change_ejson(Change));
handle_event(change_approved, Change) ->
    insights_listener:new_event(change, approved, change_ejson(Change));
handle_event(change_delivered, Change) ->
    insights_listener:new_event(change, delivered, change_ejson(Change));
handle_event(change_superseded, Change) ->
    insights_listener:new_event(change, superseded, change_ejson(Change));
handle_event(change_deleted, {{change_id, ChangeId}, {deleted_by, Deleter}}) ->
    insights_listener:new_event(change, deleted, change_deleted_ejson(ChangeId, Deleter));
handle_event(_, _) -> {error, unhandled_event}.

change_deleted_ejson(ChangeId, Deleter) ->
    [EntName, OrgName, ProjName, PipeName] = deliv_change:scoping_names(ChangeId),
    {[
        {<<"change_id">>, ChangeId},
        {<<"deleted_by">>, deliv_user:getval(name, Deleter)},
        {<<"enterprise_name">>, EntName},
        {<<"organization_name">>, OrgName},
        {<<"pipeline_name">>, PipeName},
        {<<"project_name">>, ProjName}
    ]}.

change_ejson(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    [EntName, OrgName, ProjName, PipeName] = deliv_change:scoping_names(ChangeId),
    {[
     {<<"change_id">>,    ChangeId},
     {<<"topic">>,        deliv_change:getval(feature_branch, Change)},
     {<<"target">>,       deliv_change:getval(pipeline_name_at_creation, Change)},
     {<<"state">>,        deliv_change:getval(latest_patchset_status, Change)},
     {<<"enterprise_name">>, EntName},
     {<<"organization_name">>, OrgName},
     {<<"pipeline_name">>, PipeName},
     {<<"project_name">>, ProjName},
     {<<"submitted_at">>, format_timestamp(deliv_change:getval(submitted_at, Change))},
     {<<"submitted_by">>, deliv_change:getval(submitted_by, Change)},
     {<<"merge_sha">>,    deliv_change:getval(merge_sha, Change)},
     {<<"approved_by">>,  deliv_change:getval(approved_by, Change)},
     {<<"approved_at">>,  format_timestamp(deliv_change:getval(approved_at, Change))},
     {<<"title">>,        deliv_change:getval(title, Change)},
     {<<"description">>,  deliv_change:getval(description, Change)},
     {<<"delivered_by">>, deliv_change:getval(delivered_by, Change)},
     {<<"delivered_at">>, format_timestamp(deliv_change:getval(delivered_at, Change))},
     {<<"changeset_id">>, deliv_change:getval(changeset_id, Change)},
     {<<"latest_patchset">>, deliv_change:getval(latest_patchset, Change)},
     {<<"pipeline_id">>, deliv_change:getval(pipeline_id, Change)},
     {<<"superseding_change_id">>, deliv_change:getval(superseding_change_id, Change)}
    ]}.

format_timestamp(undefined) -> <<"">>;
format_timestamp(Timestamp) -> chef_utils:format_timestamp(Timestamp).
