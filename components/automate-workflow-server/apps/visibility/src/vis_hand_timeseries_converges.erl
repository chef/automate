%% Cowboy handler for returning rolled up status counts for node converges.
-module(vis_hand_timeseries_converges).
-behaviour(deliv_rest).

-export([
         is_authorized/2,
         init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_accepted/2,
         from_json/2
        ]).

-include_lib("chef_common/include/chef_common.hrl").

is_authorized(Req, State) ->
    deliv_token:is_authorized(Req, State).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

from_json(Req, State) ->
    case vis_json:validate_and_return_json_or_400_with_proper_message(Req, State, filter_group_by_status_range_with_interval) of
        {halt, _Req1, _State1} = Halt404Message ->
            Halt404Message;
        {ReqJson, Req1, State1} ->
            Response = build_response(ReqJson),
            Req2 = cowboy_req:set_resp_body(Response, Req1),
            {true, Req2, State1}
    end.

%% private functions

-spec build_response(json()) -> binary().
build_response(ReqJson) ->
    Interval = ej:get({<<"range">>, <<"interval">>}, ReqJson),
    GroupBy = is_set(ej:get({<<"group_by">>}, ReqJson)),
    EsRequestJson = populate_es_request_from_vis_request(ReqJson, Interval, GroupBy),
    EsResponseJson = query_search_from_es(EsRequestJson),
    ResponseJson = transform_response(EsResponseJson, Interval, GroupBy),
    chef_json:encode(ResponseJson).

-spec populate_es_request_from_vis_request(json(), binary(), boolean()) -> json().
populate_es_request_from_vis_request(ReqJson, Interval, GroupBy) ->
    StartTime = ej:get({<<"range">>, <<"start_time">>}, ReqJson),
    EndTime = ej:get({<<"range">>, <<"end_time">>}, ReqJson),
    EsRequestJson1 = generate_es_query(StartTime, EndTime, Interval, GroupBy),
    vis_query_filter_builder:append_to_existing_bool_filters(ReqJson, EsRequestJson1).

-spec query_search_from_es(json()) -> json().
query_search_from_es(ESQueryJson) ->
    ESUrl = envy:get(visibility, elasticsearch_url, binary),
    ESRequestUrl = <<ESUrl/binary,
                     "/insights-*/converge/_search">>,
    {ok, _RespStatus, _RespHeaders, RespBody} = deliv_http:req(get, ESRequestUrl,
                                                               ESQueryJson),
    chef_json:decode(RespBody).

-spec transform_response(json(), binary(), boolean()) -> json().
transform_response(EsResponseJson, Interval, false) ->
    {Buckets, ResponseJson} = init_response_json(EsResponseJson, Interval),
    ej:set({<<"buckets">>}, ResponseJson,
           lists:foldl(fun(Bucket, Acc) -> Acc ++ [ej:get({<<"doc_count">>}, Bucket)] end,
                       [],
                       Buckets));
transform_response(EsResponseJson, Interval, true) ->
    {Buckets, ResponseJson1} = init_response_json(EsResponseJson, Interval),
    % TODO not very effective, iterates over the same Buckets list 3 times
    ResponseJson2 = set_status_bucket(ResponseJson1, Buckets, <<"success">>),
    ResponseJson3 = set_status_bucket(ResponseJson2, Buckets, <<"missing">>),
    set_status_bucket(ResponseJson3, Buckets, <<"failure">>).

-spec set_status_bucket(json(), [json()], binary()) -> json().
set_status_bucket(ResponseJson, Buckets, Status) ->
    ej:set_p(
      {<<"status_buckets">>, Status},
      ResponseJson,
      lists:foldl(fun(Bucket, Acc) -> Acc ++ [find_count_for_status(ej:get({<<"by_status">>, <<"buckets">>}, Bucket), Status)] end,
                  [],
                  Buckets)
     ).

-spec find_count_for_status([json()], binary()) -> integer().
find_count_for_status([], _) ->
    0;
find_count_for_status([ StatusBucket | StatusBuckets ], Status) ->
    case count_for_status(StatusBucket, Status) of
        false -> find_count_for_status(StatusBuckets, Status);
        Count -> Count
    end.

-spec count_for_status(json(), binary()) -> integer() | false.
count_for_status({[{<<"key">>, Status}, {<<"doc_count">>, Count}]}, Status) -> Count;
count_for_status(_, _) -> false.

% results sorted by key ascending (end_time)
%   https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-histogram-aggregation.html#_order
-spec generate_es_query(binary(), binary(), binary(), boolean()) -> json().
generate_es_query(StartTime, EndTime, Interval, true) ->
    Query = generate_es_query(StartTime, EndTime, Interval, false),
    ej:set(
        {"aggregations", "timeseries_converges", "aggs"},
        Query,
        {[{<<"by_status">>,
            {[{<<"terms">>,{[{<<"field">>,<<"status">>}]}}]}}]}
    );
generate_es_query(StartTime, EndTime, Interval, false) ->
    {[{<<"size">>,0},
      {<<"query">>,
       {[{<<"bool">>,
         {[{<<"must">>,
           [{[{<<"term">>,{[{<<"event_action">>,<<"finished">>}]}}]},
            {[{<<"range">>,
               {[{<<"end_time">>,
                  {[{<<"gte">>,StartTime},
                    {<<"lte">>,EndTime}]}}]}}]}]}]}}]}},
      {<<"aggregations">>,
       {[{<<"timeseries_converges">>,
          {[{<<"date_histogram">>,
             {[{<<"field">>,<<"end_time">>},
               {<<"interval">>,Interval},
               {<<"extended_bounds">>,
                {[{<<"min">>,StartTime},
                  {<<"max">>,EndTime}]}}]}}]}}]}}]}.

-spec init_response_json(json(), binary()) -> {[json()], json()}.
init_response_json(EsResponseJson, Interval) ->
    Buckets = ej:get({"aggregations", "timeseries_converges", "buckets"}, EsResponseJson),
    StartTime = ej:get({first, "key"}, Buckets) div 1000,
    NumBuckets = length(Buckets),
    ResponseJson = {[
        {<<"start_time">>, StartTime},
        {<<"interval">>, Interval},
        {<<"bucket_total">>, NumBuckets}
    ]},
    {Buckets, ResponseJson}.

-spec is_set(undefined | binary()) -> boolean().
is_set(undefined) -> false;
is_set(_) -> true.
