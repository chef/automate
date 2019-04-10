%% @doc This module provides json-format stage queue status information via the :ent/queue-status
%% endpoint. It is provided only for users authorized with the enterprise admin role,
%% and is intended to be used for debugging and tracing purporses.
%%
%% Returned data is in the form of a list of entries per queued item with various metadata, including
%% qeuee name, submitter, status, change id, enqueue time, and a summary of the phase runs thus far.
%%
%% NB: This data is subject to change, and customers or internal users should be aware of this
%% when decideding to collect this data using external tools.
%%
%% Also, note that the queue itself is not persistent across Delivery backend restarts.
-module(deliv_hand_queue_status).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         init/3,
         rest_init/2,
         to_json/2
        ]).

%% @doc authz privileges for enterprise admin role users only
-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

%% @doc convert the internal queue_status output into json
to_json(Req, #handler{ent_name = EntName} = State) ->
    StatusList = deliverance_stage_queue:queue_status(EntName),
    ProcessedStatusList = lists:map(fun([]) ->
                                        [];
                                       (Status) ->
                                        PhaseRunSummary = proplists:get_value(<<"phase_run_summary">>, Status),
                                        {ok, PhaseRunSummaryEjson} = deliv_phase_run_summary:to_ejson(PhaseRunSummary),
                                        {lists:keyreplace(<<"phase_run_summary">>, 1, Status, {<<"phase_run_summary">>, PhaseRunSummaryEjson})}
                                    end,
                                    StatusList),
    Json = {[{<<"queue_status">>, lists:flatten(ProcessedStatusList)}]},
    deliv_web_utils:content(Json, Req, State).
