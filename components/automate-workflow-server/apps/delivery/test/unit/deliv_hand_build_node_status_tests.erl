-module(deliv_hand_build_node_status_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("deliv_types.hrl").
-include("../src/deliverance_types.hrl").

-compile([export_all]).

to_json_test_() ->
    hoax:fixture(?MODULE, "to_json_").

init_test() ->
    Request = req,
    State = bogus_state,
    Response = {upgrade, protocol, cowboy_rest, Request, State},
    ?assertEqual(Response,
                 deliv_hand_build_node_status:init(transport, Request, bogus_state)).

to_json_returns_build_nodes() ->
    Request = req,
    State = state,
    SearchResult = {ok,
                    #search{rows = [
                                    {[{<<"url">>,
                                       <<"https://chef.server/oranizations/cd/nodes/build_node_1">>},
                                      {<<"data">>,
                                       {[{<<"name">>, <<"build_node_1">>}]}}]},
                                    {[{<<"url">>,
                                       <<"https://chef.server/oranizations/cd/nodes/build_node_2">>},
                                      {<<"data">>,
                                       {[{<<"name">>, <<"build_node_2">>}]}}]},
                                    {[{<<"url">>,
                                       <<"https://chef.server/oranizations/cd/nodes/build_node_3">>},
                                      {<<"data">>,
                                       {[{<<"name">>, <<"build_node_3">>}]}}]},
                                    {[{<<"url">>,
                                       <<"https://chef.server/oranizations/cd/nodes/build_node_4">>},
                                      {<<"data">>,
                                       {[{<<"name">>, <<"build_node_4">>}]}}]}
                                   ]}},
    hoax:mock(deliv_chef_api,
              ?expect(search,
                      ?withArgs(["node",
                                 "tags:delivery-build-node",
                                 <<"{\"name\":[\"name\"]}">>]),
                      ?andReturn(SearchResult))),

    hoax:mock(deliv_phase_run,
              [?expect(list_running,
                       ?withArgs([]),
                       ?andReturn([build_node_1, build_node_3])),
               ?expect(getval,
                       ?withArgs([build_node, build_node_1]),
                       ?andReturn(<<"build_node_1">>)),
               ?expect(getval,
                       ?withArgs([build_node, build_node_3]),
                       ?andReturn(<<"build_node_3">>))]),

    JsonResult = [
                  {[{<<"name">>, <<"build_node_1">>}, {<<"status">>, <<"active">>}]},
                  {[{<<"name">>, <<"build_node_2">>}, {<<"status">>, <<"idle">>}]},
                  {[{<<"name">>, <<"build_node_3">>}, {<<"status">>, <<"active">>}]},
                  {[{<<"name">>, <<"build_node_4">>}, {<<"status">>, <<"idle">>}]}
                 ],
    Result = {ok, req2, State},
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([JsonResult, Request, State]),
                      ?andReturn(Result))),

    ?assertEqual(Result,
                 deliv_hand_build_node_status:to_json(Request, State)),
    ?verifyAll.

%% When determining build nodes for pre-dazzle installs, we'll most likely see
%% that phase runs are being executed on nodes that are NOT tagged with 'build-node'.
%% In this case, we should simply ignore these nodes.
to_json_ignores_extra_nodes_in_phase_runs() ->
    Request = req,
    State = state,
    SearchResult = {ok,
                    #search{rows = [
                                    {[{<<"url">>,
                                       <<"https://chef.server/oranizations/cd/nodes/build_node_1">>},
                                      {<<"data">>,
                                       {[{<<"name">>, <<"build_node_1">>}]}}]}
                                   ]}},
    hoax:mock(deliv_chef_api,
              ?expect(search,
                      ?withArgs(["node",
                                 "tags:delivery-build-node",
                                 <<"{\"name\":[\"name\"]}">>]),
                      ?andReturn(SearchResult))),

    hoax:mock(deliv_phase_run,
              [?expect(list_running,
                       ?withArgs([]),
                       ?andReturn([build_node_1, build_node_2, build_node_3])),
               ?expect(getval,
                       ?withArgs([build_node, build_node_1]),
                       ?andReturn(<<"build_node_1">>)),
               ?expect(getval,
                       ?withArgs([build_node, build_node_2]),
                       ?andReturn(<<"build_node_2">>)),
               ?expect(getval,
                       ?withArgs([build_node, build_node_3]),
                       ?andReturn(<<"build_node_3">>))]),

    JsonResult = [
                  {[{<<"name">>, <<"build_node_1">>}, {<<"status">>, <<"active">>}]}
                 ],
    Result = {ok, req2, State},
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([JsonResult, Request, State]),
                      ?andReturn(Result))),

    ?assertEqual(Result,
                 deliv_hand_build_node_status:to_json(Request, State)),
    ?verifyAll.

to_json_when_search_results_empty_returns_no_nodes() ->
    Request = req,
    State = state,
    SearchResult = {ok, #search{rows = []}},
    hoax:mock(deliv_chef_api,
              ?expect(search,
                      ?withArgs(["node",
                                 "tags:delivery-build-node",
                                 <<"{\"name\":[\"name\"]}">>]),
                      ?andReturn(SearchResult))),

    hoax:mock(deliv_phase_run,
              [?expect(list_running,
                       ?withArgs([]),
                       ?andReturn([build_node_1, build_node_2, build_node_3])),
               ?expect(getval,
                       ?withArgs([build_node, build_node_1]),
                       ?andReturn(<<"build_node_1">>)),
               ?expect(getval,
                       ?withArgs([build_node, build_node_2]),
                       ?andReturn(<<"build_node_2">>)),
               ?expect(getval,
                       ?withArgs([build_node, build_node_3]),
                       ?andReturn(<<"build_node_3">>))]),

    JsonResult = [],
    Result = {ok, req2, State},
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([JsonResult, Request, State]),
                      ?andReturn(Result))),

    ?assertEqual(Result,
                 deliv_hand_build_node_status:to_json(Request, State)),
    ?verifyAll.


to_json_when_no_running_jobs_returns_all_idle() ->
    Request = req,
    State = state,
    SearchResult = {ok,
                    #search{rows = [
                                    {[{<<"url">>,
                                       <<"https://chef.server/oranizations/cd/nodes/build_node_1">>},
                                      {<<"data">>,
                                       {[{<<"name">>, <<"build_node_1">>}]}}]},
                                    {[{<<"url">>,
                                       <<"https://chef.server/oranizations/cd/nodes/build_node_2">>},
                                      {<<"data">>,
                                       {[{<<"name">>, <<"build_node_2">>}]}}]}
                                   ]}},
    hoax:mock(deliv_chef_api,
              ?expect(search,
                      ?withArgs(["node",
                                 "tags:delivery-build-node",
                                 <<"{\"name\":[\"name\"]}">>]),
                      ?andReturn(SearchResult))),

    hoax:mock(deliv_phase_run,
              ?expect(list_running,
                      ?withArgs([]),
                      ?andReturn([]))),

    JsonResult = [
                  {[{<<"name">>, <<"build_node_1">>}, {<<"status">>, <<"idle">>}]},
                  {[{<<"name">>, <<"build_node_2">>}, {<<"status">>, <<"idle">>}]}
                 ],
    Result = {ok, req2, State},
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([JsonResult, Request, State]),
                      ?andReturn(Result))),

    ?assertEqual(Result,
                 deliv_hand_build_node_status:to_json(Request, State)),
    ?verifyAll.
