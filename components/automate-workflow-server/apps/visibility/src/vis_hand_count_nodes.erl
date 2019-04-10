%% Cowboy handler for returning rolled up status counts for node converges.
-module(vis_hand_count_nodes).
-behaviour(deliv_rest).

-export([
         allowed_methods/2,
         content_types_accepted/2,
         init/3,
         from_json/2,
         rest_init/2,
         is_authorized/2,
         recursively_query_es/3
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
    case vis_json:validate_and_return_json_or_400_with_proper_message(Req, State, filter_group_by_status_range_without_interval) of
        {halt, _Req1, _State1} = Halt404Message ->
            Halt404Message;
        {ReqJson, Req1, State1} ->
            Response = build_response_based_on_group_by(ej:get({"group_by"}, ReqJson), ReqJson),
            Req2 = cowboy_req:set_resp_body(Response, Req1),
            {true, Req2, State1}
    end.

-spec build_response_based_on_group_by(binary() | undefined, json()) -> binary().
build_response_based_on_group_by(undefined, ReqJson) ->
    ESQueryJson = vis_query_filter_builder:append_to_existing_bool_filters(ReqJson,
                                                                           init_es_query()),
    chef_json:encode({[{<<"count">>, query_count_from_es(ESQueryJson)}]});
build_response_based_on_group_by(<<"status">>, ReqJson) ->
    recursively_query_es(ReqJson, [<<"success">>, <<"failure">>, <<"missing">>], {[]}).

-spec recursively_query_es(json(), [binary()], json()) -> binary().
recursively_query_es(_ReqJson, [], ResponseAcc) ->
    chef_json:encode(ResponseAcc);
recursively_query_es(ReqJson, StatusList, ResponseAcc) ->
    [Status|NewStatusList] = StatusList,
    InitESQuery = init_es_query(Status),
    ESQueryJson = vis_query_filter_builder:append_to_existing_bool_filters(ReqJson,
                                                                           InitESQuery),
    Count = query_count_from_es(ESQueryJson),
    NewResponseAcc = ej:set({Status}, ResponseAcc, Count),
    recursively_query_es(ReqJson, NewStatusList, NewResponseAcc).

-spec query_count_from_es(json()) -> integer().
query_count_from_es(ESQueryJson) ->
    ESUrl = envy:get(visibility, elasticsearch_url, binary),
    ESRequestUrl = <<ESUrl/binary,
                     "/node-state/_count">>,
    {ok, _RespStatus, _RespHeaders, RespBody} = deliv_http:req(post, ESRequestUrl,
                                                               ESQueryJson),
    ej:get({"count"}, chef_json:decode(RespBody)).

-spec init_es_query() -> json().
init_es_query() ->
    {[
      {<<"query">>,
       {[
         {<<"bool">>,
          {[
            {<<"filter">>,
             [
              {[
                {<<"term">>,
                 {[{<<"exists">>,<<"true">>}]}}
               ]}
             ]
            }
           ]}
         }
        ]}
      }
     ]}.


-spec init_es_query(binary()) -> json().
init_es_query(Status) ->
    ej:set(
        {<<"query">>, <<"bool">>, <<"filter">>, new},
        init_es_query(),
        {[{<<"term">>, {[{<<"status">>, Status}]}}]}
    ).
