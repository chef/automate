%% Cowboy handler for returning cookbook counts.
-module(vis_hand_count_cookbooks).
-behaviour(deliv_rest).

-export([
         allowed_methods/2,
         content_types_accepted/2,
         init/3,
         from_json/2,
         rest_init/2,
         is_authorized/2
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
            ESQueryJson = vis_query_filter_builder:append_to_existing_bool_filters(ReqJson,
                                                                                   add_range_to_query(ReqJson)),
            build_response(Req1, State1, query_count_from_es(ESQueryJson))
    end.

build_response(Req1, State, {ok, Count}) ->
    Response = chef_json:encode({[{<<"count">>, Count}]}),
    Req2 = cowboy_req:set_resp_body(Response, Req1),
    {true, Req2, State};
build_response(Req1, State, {error, EsErrorMessage}) ->
    deliv_web_utils:error_response(400, bad_request, EsErrorMessage, Req1, State).

-spec add_range_to_query(json()) -> json().
add_range_to_query(ReqJson) ->
    RangeJson = ej:get({<<"range">>}, ReqJson),
    case RangeJson of
        undefined ->
            init_es_query();
        Range ->
            init_es_query(Range)
    end.

-spec query_count_from_es(json()) -> {ok, integer()} | {error, binary()}.
query_count_from_es(ESQueryJson) ->
    ESUrl = envy:get(visibility, elasticsearch_url, binary),
    ESRequestUrl = <<ESUrl/binary, % /api/v0/visibility
                     "/insights-*/cookbook/_count">>,
    {ok, RespStatus, _RespHeaders, RespBody} = deliv_http:req(get, ESRequestUrl,
                                                               ESQueryJson),
    case RespStatus of
        200 ->
            {ok, ej:get({<<"count">>}, chef_json:decode(RespBody))};
        _ ->
            {error, ej:get({<<"error">>, <<"root_cause">>, first, <<"reason">>}, chef_json:decode(RespBody))}
    end.

-spec init_es_query() -> json().
init_es_query() ->
    {[
      {<<"query">>,
       {[
         {<<"bool">>,
          {[
            {<<"must">>, []}
           ]}
         }
        ]}
      }
     ]}.

-spec init_es_query(json()) -> json().
init_es_query(Range) ->
    ej:set(
        {<<"query">>, <<"bool">>, <<"must">>, new},
        init_es_query(),
        {[
          {<<"range">>,
           {[
             {<<"@timestamp">>,
              {[
                {<<"gte">>, ej:get({<<"start_time">>}, Range)},
                {<<"lte">>, ej:get({<<"end_time">>}, Range)}
              ]}
             }
           ]}
          }
        ]}
    ).
