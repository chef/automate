%% @doc Common code needed for generating JSON output for both the
%% deliv_hand_change and deliv_hand_changes Cowboy handlers
-module(deliv_pipeline_common).

-include("deliv_types.hrl").

-export([
         get_pipeline_as_ejson/4,
         get_pipelines_stats_as_ejson/1
        ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

get_pipeline_as_ejson(EntName, OrgName, ProjName, PipeName) ->
  case deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName) of
      {ok, Pipeline} ->
          pipeline_to_ejson(EntName, OrgName, ProjName, Pipeline);
      {error, _} = Error ->
          Error
  end.

%% @doc gets all the pipelines for an enterprise and returns them in json.
%% It currently mocks the data about pipeline state.
-spec get_pipelines_stats_as_ejson(binary()) -> {ok, json()} |
                                                {'error',atom() | tuple()}.
get_pipelines_stats_as_ejson(EntName) ->
    case deliv_enterprise:pipelines_with_stats(EntName) of
        {ok, Pipelines} ->
            pipelines_stats_to_ejson(Pipelines);
        {error, _} = Error ->
            Error
    end.

%% Ejson Processing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% There's a good bit of processing here! The main idea is to either
%% return {ok, ChangeEjson} or the first error that we encounter in
%% the course of building up that Ejson.
%%
%% For each change, we'll need to retrieve all patchsets for the
%% change. For each patchset, we'll need to retrieve all commit
%% messages from the commits on that patchset's branch, as well as the
%% cumulative diffstats for that patchset. Only if all of that works
%% do we want to return the Ejson. Furthermore, we don't want to
%% continue processing information once we've received an error.
%%
%% Function Patterns:
%%
%% handle_FOOs: handle results of retrieving data from another system (database or Git)
%% FOOs_to_ejson: convert valid retrieved FOOs into Ejson
%% FOOs_to_ejson_acc: accumulate valid Ejson conversions; short-circuit at first error
%% FOO_to_ejson: convert a single FOO to ejson
%% handle_FOO: called from FOO_to_ejson: determine ok / error status based on inputs
-spec pipelines_stats_to_ejson([[deliv_sqerl_record()]]) -> {ok, json()}.
pipelines_stats_to_ejson(Results) ->
    %% Results is db resultset
    pipelines_stats_to_ejson_acc(Results, []).

-spec pipelines_stats_to_ejson_acc([[deliv_sqerl_record()]], json()) -> {ok, json()}.
pipelines_stats_to_ejson_acc([], EjsonAcc) ->
    {ok, lists:reverse(EjsonAcc)};
pipelines_stats_to_ejson_acc([Result|Results], EjsonAcc) ->
    {ok, Ejson} = pipeline_stats_to_ejson(Result),
    pipelines_stats_to_ejson_acc(Results, [Ejson|EjsonAcc]).

%% @doc Builds an ejson representation of a pipeline including org name, proj name,
%% and pipeline name. The values for stats, deployed, and delivered are hard
%% coded for this first pass.
-spec pipeline_stats_to_ejson([deliv_sqerl_record()]) -> {ok, json()}.
pipeline_stats_to_ejson(Result) ->
    Id              = proplists:get_value(<<"id">>,                Result),
    Org             = proplists:get_value(<<"org_name">>,          Result),
    Project         = proplists:get_value(<<"proj_name">>,         Result),
    Name            = proplists:get_value(<<"name">>,              Result),
    BuildCount      = proplists:get_value(<<"build_count">>,       Result),
    BuildStatus     = proplists:get_value(<<"build_status">>,      Result),
    AcceptCount     = proplists:get_value(<<"acceptance_count">>,  Result),
    AcceptStatus    = proplists:get_value(<<"acceptance_status">>, Result),
    LastDeployed    = proplists:get_value(<<"last_deployed">>,     Result),
    LastDelivered   = proplists:get_value(<<"last_delivered">>,    Result),

    {ok, {[
           {<<"id">>,        Id},
           {<<"org">>,       Org},
           {<<"project">>,   Project},
           {<<"pipeline">>,  Name},
           {<<"stats">>,{[
                          {<<"build">>,{[
                                         {<<"changes">>, BuildCount},
                                         {<<"state">>, tx_stat(BuildStatus)}
                                       ]}},
                          {<<"acceptance">>,{[
                                              {<<"changes">>, AcceptCount},
                                              {<<"state">>, tx_stat(AcceptStatus)}
                                            ]}}
                        ]}},
            {<<"deployed">>, chef_utils:format_timestamp(LastDeployed)},
            {<<"delivered">>, chef_utils:format_timestamp(LastDelivered)}
          ]}}.

%% @doc Builds an ejson representation of a pipeline including org name, proj name,
%% and pipeline name. The values for stats, deployed, and delivered are hard
%% coded for this first pass.
-spec pipeline_to_ejson(binary(), binary(), binary(), d_pipeline()) -> {ok, json()}.
pipeline_to_ejson(EntName, OrgName, ProjName, Pipeline) ->
    PipeId      = deliv_pipeline:getval(id, Pipeline),
    PipeName    = deliv_pipeline:getval(name, Pipeline),

    %% TODO: is this exactly the info we want to return?
    SelfLink = deliv_web_utils:href(EntName, <<"/orgs/", OrgName/binary,
                                               "/projects/", ProjName/binary,
                                               "/pipelines/", PipeName/binary>>),
    UriEncodedPipeName = deliv_web_utils:encode_url(chef_utils:to_str(PipeName)),
    Links = [{<<"self">>, SelfLink},
             {<<"changes">>, deliv_web_utils:href(EntName, ["/orgs/", OrgName,
                                                            "/projects/", ProjName,
                                                            "/changes?pipeline=", UriEncodedPipeName,
                                                            "&limit=25"])}
            ],
    Hal = deliv_web_utils:make_hal(Links),

    {ok, {[
           {<<"id">>, PipeId},
           {<<"name">>, PipeName},
           {<<"_links">>, Hal}
         ]}}.

%% @doc Alas, some of our labeling is not super consistent. This helps
%% by translating the values currently stored as statuses in the
%% stage_runs database table into values our stats endpoint speaks.
%%
%% TODO: We really need to fix this properly with a database
%% migration.
tx_stat(<<"idle">>)       -> <<"running">>; %% idle means the stage has started, and we're waiting for workers
tx_stat(Status)           -> Status. %% running, failed, passed
