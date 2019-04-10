-module(deliv_dependency_failures).

%% Maintains the state of what is safe to promote out of Union.

-behaviour(gen_server).

-include("deliv_types.hrl").
-include("deliv_events.hrl").
-include("deliv_coordinates.hrl").

%% Behaviour callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% Public API
-export([
    start_link/0,
    is_blocked/1,
    get_blocked_project_names/1
  ]).

-define(SERVER, ?MODULE).

%% Create a managed process in our supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Return whether the specified pipeline is in a blocked set
-spec is_blocked(non_neg_integer()) -> boolean().
is_blocked(PipelineId) ->
    gen_server:call(?SERVER, {is_blocked, PipelineId}).

-spec get_blocked_project_names(binary()) -> {ok, [binary()]} | {error, _}.
get_blocked_project_names(EntName) ->
    gen_server:call(?SERVER, {get_blocked_project_names, EntName}).

%% Initialize and return an empty state
-spec init([]) -> {ok, no_state}.
init([]) ->
    true = deliv_event:subscribe([union_finished]),
    {ok, no_state}.

%% Sync Calls
handle_call({is_blocked, PipelineId}, _From, no_state) ->
    [EntName, _, _] = deliv_pipeline:scoping_names(PipelineId),
    Result = with_enterprise(EntName, is_blocked, fun(BlockedMaps) ->
        deliv_dependency_failures_impl:is_pipeline_blocked(PipelineId, BlockedMaps)
    end),
    {reply, Result, no_state};
handle_call({get_blocked_project_names, EntName}, _From, no_state) ->
    Result = with_enterprise(EntName, get_all_blocked_pipeline_ids, fun(BlockedMaps) ->
        PipelineIds = deliv_dependency_failures_impl:get_all_blocked_pipeline_ids(BlockedMaps),
        {ok, get_project_names_from_pipeline_ids(PipelineIds)}
    end),
    {reply, Result, no_state};
handle_call(_Request, _From, no_state) ->
    {reply, ok, no_state}.

%% Async Calls
handle_cast(_Msg, no_state) ->
    {noreply, no_state}.

handle_info({_From, union_finished, NewSet}, no_state) ->
    [APipelineId | _ ] = maps:keys(NewSet),
    [EntName, _, _] = deliv_pipeline:scoping_names(APipelineId),
    {MergedSet, DisjointBlockedSets} = with_enterprise(EntName, union_finished, fun(BlockedMaps) ->
        deliv_dependency_failures_impl:merge_overlapping_sets(NewSet, BlockedMaps)
    end),

    IsSetBlocked = deliv_dependency_failures_impl:are_dependencies_blocked(MergedSet),
    publish_blocked_event_for_pipelines_in_set(MergedSet, IsSetBlocked),

    %% A set where all tests have passed should not be considered blocked.
    %% The only tests whose results have changed are in MergedStatuses, so that
    %% is the only set we need to check.
    UpdatedSets = case IsSetBlocked of
        true -> [MergedSet | DisjointBlockedSets];
        false -> DisjointBlockedSets
    end,
    log_blocked_sets(UpdatedSets),
    deliv_dependency_failures_db:insert(EntName, UpdatedSets),
    {noreply, no_state};
handle_info(_Info, no_state) ->
    {noreply, no_state}.

%% Terminate the gen_server
terminate(_Reason, no_state) ->
    ok.

%% Handle code changes
code_change(_OldVsn, no_state, _Extra) ->
    {ok, no_state}.

%% @private
with_enterprise(EntName, FunName, Fun) ->
    case deliv_dependency_failures_db:fetch(EntName) of
        {ok, BlockedPipelineIds} ->
            Fun(BlockedPipelineIds);
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, FunName, [EntName], Why),
            Error
    end.

publish_blocked_event_for_pipelines_in_set(Set, IsSetBlocked) ->
    lists:foreach(fun(Element) ->
        deliv_event:publish({pipeline_blocked, Element}, IsSetBlocked)
    end,
    maps:keys(Set)).

%% Helper function to log all the blocked sets for debugging purposes
log_blocked_sets(Sets) ->
    LogMessage = lists:foldl(
        fun(Set, Output) ->
            io_lib:format("~s  ~p\n", [Output, maps:keys(Set)])
        end,
        "Dependency Failures:\n",
        Sets
    ),
    chef_log:info(LogMessage).

-spec get_project_names_from_pipeline_ids([pipeline_id()]) -> [binary()].
get_project_names_from_pipeline_ids(PipelineIds) ->
    lists:foldl(
        fun(PipelineId, Acc) ->
            case get_project_name_from_pipeline_id(PipelineId) of
                {error, Why} ->
                    chef_log:warning("in module ~p failed to add project to blocked list due to ~p, ignoring it from the blocked list", [?MODULE, Why]),
                    Acc;
                ProjectName -> [ProjectName | Acc]
            end
        end,
        [],
        PipelineIds
    ).

-spec get_project_name_from_pipeline_id(pipeline_id()) -> binary() | {error, atom()}.
get_project_name_from_pipeline_id(PipelineId) ->
    ProjectIdOrError = fetch_value_or_fail(deliv_pipeline, project_id, PipelineId),
    fetch_value_or_fail(deliv_project, name, ProjectIdOrError).

% if we have already failed to find something, just return that error
-spec fetch_value_or_fail(atom(), atom(), any()) -> binary() | {error, atom()}.
fetch_value_or_fail(_Module, _Key, {error, Why})->
    {error, Why};
fetch_value_or_fail(Module, Key, Id)->
    case Module:fetch_by_id(Id) of
        {ok, Object} ->
            Module:getval(Key, Object);
        {error, not_found} ->
            {error, list_to_atom(atom_to_list(Module) ++ "not_found")}
    end.
