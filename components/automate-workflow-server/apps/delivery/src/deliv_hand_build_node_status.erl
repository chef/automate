-module(deliv_hand_build_node_status).
-behaviour(deliv_rest).

-include("deliverance_types.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         init/3,
         rest_init/2,
         to_json/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

to_json(Req, State) ->
    {ok, SearchResult} = deliv_chef_api:search("node",
                                               "tags:delivery-build-node",
                                               <<"{\"name\":[\"name\"]}">>),
    BuildNodes = lists:map(fun(Row) ->
                                   ej:get({"name"}, ej:get({"data"}, Row))
                           end,
                           SearchResult#search.rows),
    DbResult = deliv_phase_run:list_running(),
    RunningNodes = lists:map(fun(Row) ->
                                     deliv_phase_run:getval(build_node, Row)
                             end,
                             DbResult),
    {NodeStatus, _} = lists:foldl(fun(Build, {Acc, Running}) ->
                                          Status = case lists:member(Build, Running) of
                                                       true -> <<"active">>;
                                                       false -> <<"idle">>
                                                   end,
                                          Json = {[
                                                   {<<"name">>, Build},
                                                   {<<"status">>, Status}
                                                  ]},
                                          {Acc ++ [Json], Running}
                                  end, {[], RunningNodes}, BuildNodes),
    deliv_web_utils:content(NodeStatus, Req, State).
