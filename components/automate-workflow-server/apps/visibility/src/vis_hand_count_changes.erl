%% Cowboy handler for returning cookbook counts.
-module(vis_hand_count_changes).
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
    case vis_json:validate_and_return_json_or_400_with_proper_message(Req, State, filter_range_without_interval) of
        {halt, _Req1, _State1} = Halt404Message ->
            Halt404Message;
        {ReqJson, Req1, State1} ->
            build_response(Req1, State1, ReqJson)
    end.

%% private functions

build_response(Req1, State, ReqJson) ->
    EsRequestJson = populate_es_request_from_vis_request(ReqJson),
    EsResponse = query_search_from_es(EsRequestJson),
    case EsResponse of
        {ok, Count} ->
            Response = chef_json:encode({[{<<"count">>, Count}]}),
            Req2 = cowboy_req:set_resp_body(Response, Req1),
            {true, Req2, State};
        {error, EsErrorMessage} ->
            deliv_web_utils:error_response(400, bad_request, EsErrorMessage, Req1, State)
    end.

-spec populate_es_request_from_vis_request(json()) -> json().
populate_es_request_from_vis_request(ReqJson) ->
    StartTime = ej:get({<<"range">>, <<"start_time">>}, ReqJson),
    EndTime = ej:get({<<"range">>, <<"end_time">>}, ReqJson),
    EsRequestJson = generate_es_query(StartTime, EndTime),
    vis_query_filter_builder:append_to_existing_bool_filters(ReqJson, EsRequestJson).

-spec generate_es_query(binary() | undefined, binary() | undefined) -> json().
generate_es_query(undefined, undefined) ->
    {[{<<"query">>,
       {[{<<"bool">>,
         {[
           {<<"must">>,
             [{[{<<"term">>,{[{<<"event_type">>,<<"change">>}]}}]},
              {[{<<"term">>,{[{<<"event_action">>,<<"delivered">>}]}}]}]}]}}]}}]};
generate_es_query(StartTime, EndTime) ->
    ej:set(
        {<<"query">>, <<"bool">>, <<"must">>, new},
        generate_es_query(undefined, undefined),
        {[
          {<<"range">>,
           {[
             {<<"@timestamp">>,
              {[
                {<<"gte">>, StartTime},
                {<<"lte">>, EndTime}
              ]}
             }
           ]}
          }
        ]}
    ).

-spec query_search_from_es(json()) -> {ok, integer()} | {error, binary()}.
query_search_from_es(ESQueryJson) ->
    ESUrl = envy:get(visibility, elasticsearch_url, binary),
    ESRequestUrl = <<ESUrl/binary,
                     "/insights-*/_count">>,
    {ok, RespStatus, _RespHeaders, RespBody} = deliv_http:req(get, ESRequestUrl,
                                                               ESQueryJson),
    case RespStatus of
        200 ->
            {ok, ej:get({<<"count">>}, chef_json:decode(RespBody))};
        _ ->
            {error, ej:get({<<"error">>, <<"root_cause">>, first, <<"reason">>}, chef_json:decode(RespBody))}
    end.
