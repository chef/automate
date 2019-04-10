-module (deliv_dependency_failures_impl).

%% Provides the business logic for dependency failures data

-include("deliv_types.hrl").

-export ([
          merge_overlapping_sets/2,
          are_dependencies_blocked/1,
          is_pipeline_blocked/2,
          get_all_blocked_pipeline_ids/1
         ]).

%% Blocked sets must be pairwise disjoint (no two sets have an intersection).
%% A new blocked set may have an intersection with one or more sets in the
%% set of blocked sets. By unioning intersecting sets we ensure pairwise
%% disjointness.
-spec merge_overlapping_sets(pipeline_statuses(), [pipeline_statuses()]) -> {pipeline_statuses(), [pipeline_statuses()]}.
merge_overlapping_sets(PipelineStatuses, BlockedSets) ->
    lists:foldl(
        fun (BlockedMap, {SetToUnion, NewBlockedSets}) ->
            case sets:is_disjoint(key_set(BlockedMap), key_set(SetToUnion)) of
                true -> {SetToUnion, [BlockedMap | NewBlockedSets]};
                false -> {maps:merge(BlockedMap, SetToUnion), NewBlockedSets}
            end
        end,
        {PipelineStatuses, []},
        BlockedSets
    ).

%% PipelineStatuses is a list of maps, the maps are called blocked sets,
%% whose keys are Pipeline IDs. Each Pipeline ID represents a blocked pipeline.
%%
%% This function returns all Pipeline IDs in a list given a list of blocked sets
%% by iterating over every blocked set (a map) and grabbing every Pipeline ID (the
%% keys of the maps).
%%
%% Blocked sets are disjoint so we are not worried about duplicate keys.
%% No promises on ordering of returned list.
-spec get_all_blocked_pipeline_ids([pipeline_statuses()]) -> [pipeline_id()].
get_all_blocked_pipeline_ids(PipelineStatuses) ->
    lists:foldl(
        fun(BlockedMap, PipelineIds) ->
            lists:append(maps:keys(BlockedMap), PipelineIds)
        end,
        [],
        PipelineStatuses
    ).

%% @doc Determine whether a pipeline-set is blocked.
%% @return true if any pipeline has status that is not 'passed', else false
-spec are_dependencies_blocked(pipeline_statuses()) -> boolean().
are_dependencies_blocked(DependencyFailures) ->
    lists:any(fun(Val) -> Val /= passed end, maps:values(DependencyFailures)).

-spec is_pipeline_blocked(pipeline_id(), [pipeline_statuses()]) -> boolean().
is_pipeline_blocked(PipelineId, BlockedMaps) ->
    lists:any(fun(Map) -> maps:is_key(PipelineId, Map) end, BlockedMaps).

%% @private

%% @doc Retrieve the keys of a map as a set
-spec key_set(#{T => any()}) -> sets:set(T).
key_set(Map) -> sets:from_list(maps:keys(Map)).
