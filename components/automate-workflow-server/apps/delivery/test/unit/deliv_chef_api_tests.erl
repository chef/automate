%% @author Adam Jacob <adam@opscode.com>
%% Copyright 2013-2015 Chef Software, Inc. All Rights Reserved.
%%

-module(deliv_chef_api_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/deliverance_types.hrl").

-define(CHEF_CONFIG, stub_config).
-define(STATE, {state, ?CHEF_CONFIG}).

-define(PUSHY_JOB_ID, "97f0102c4e345db904b52bc2b48a75e4").

-define(PUSHY_JOB_CREATED, {
        ok,
        "201",
        [{"Server","nginx/1.2.3"},
         {"Date","Fri, 31 May 2013 20:04:05 GMT"},
         {"Content-Type","application/json"},
         {"Content-Length","95"},
         {"Connection","keep-alive"},
         {"Location",
          "https://10.0.2.15/organizations/cd/pushy"
          "/jobs/" ?PUSHY_JOB_ID
         }],
          <<"{\"uri\":\"https:\\/\\/10.0.2.15\\/organizations"
            "\\/cd\\/pushy\\/jobs\\/"
            ?PUSHY_JOB_ID
            "\"}">>}).

-define(PUSHY_JOB_NO_NODES, {ok,"400",
         [{"Server","nginx/1.2.3"},
          {"Date","Fri, 31 May 2013 20:03:48 GMT"},
          {"Content-Length","46"},
          {"Connection","keep-alive"}],
         <<"{\"error\":\"at least one node must be supplied\"}">>}).

-define(PUSHY_JOB_STATUS, {ok,"200",
    [{"Server","nginx/1.2.3"},
     {"Date","Fri, 31 May 2013 22:12:20 GMT"},
     {"Content-Type","application/json"},
     {"Content-Length","233"},
     {"Connection","keep-alive"}],
    <<"{\"nodes\":{\"unavailable\":[\"alice\"]},\"id\":\""
      ?PUSHY_JOB_ID
      "\",\"command\":\"chef-client\",\"status\":\"quorum_failed\","
      "\"run_timeout\":3600,"
      "\"created_at\":\"Fri, 31 May 2013 22:11:54 GMT\","
      "\"updated_at\":\"Fri, 31 May 2013 22:11:54 GMT\"}">>}).

-define(PUSHY_JOB_ERROR,
        {ok,"400",
         [{"Server","nginx/1.2.3"},
          {"Date","Fri, 31 May 2013 22:12:20 GMT"},
          {"Content-Type","application/json"},
          {"Content-Length","233"},
          {"Connection","keep-alive"}],
         <<"{\"nodes\":{\"unavailable\":[\"alice\"]},"
           "\"id\":\"" ?PUSHY_JOB_ID "\","
           "\"command\":\"chef-client\","
           "\"status\":\"quorum_failed\",\"run_timeout\":3600,"
           "\"created_at\":\"Fri, 31 May 2013 22:11:54 GMT\","
           "\"updated_at\":\"Fri, 31 May 2013 22:11:54 GMT\"}">>}).

push_job_start_test_() ->
    setup([{"POSTs well formed data to start push job, returns job ID",
       fun() ->
               Cmd = <<"chef-client">>,
               Nodes = [<<"alice">>],
               Quorum = 1,
               Return = ?PUSHY_JOB_CREATED,
               chef_req_meck_push_job_start(Cmd, Nodes, Quorum, Return),
               Got = deliv_chef_api:push_job_start(Cmd, Nodes, Quorum),
               ?assertEqual({ok, ?PUSHY_JOB_ID}, Got),
               ?assert(meck:validate(chef_req))
       end},
      {"handles no nodes error on job start attempt",
       fun() ->
               Cmd = <<"chef-client">>,
               Nodes = [],
               Quorum = 1,
               Return = ?PUSHY_JOB_NO_NODES,
               chef_req_meck_push_job_start(Cmd, Nodes, Quorum, Return),
               Got = deliv_chef_api:push_job_start(Cmd, Nodes, Quorum),
               ?assertEqual({error, ?PUSHY_JOB_NO_NODES}, Got),
               ?assert(meck:validate(chef_req))
       end}
     ]).

%% Mock chef_req:request via meck for a push job create
%% request. Include assertions on the format of the request body.
chef_req_meck_push_job_start(Cmd, Nodes, Quorum, Return) ->
    F = fun(post, "/pushy/jobs", RawPushReq, _) ->
                PushReq = chef_json:decode(RawPushReq),
                ?assertEqual(Cmd, ej:get([<<"command">>], PushReq)),
                ?assertEqual(Nodes, ej:get([<<"nodes">>], PushReq)),
                ?assertEqual(Quorum, ej:get([<<"quorum">>], PushReq)),
                ?assertEqual(4500, ej:get([<<"run_timeout">>], PushReq)),
                Return
        end,
    meck:expect(chef_req, request, F).

push_job_status_test_() ->
    setup([{"successful pushy job status call",
       fun() ->
               chef_req_meck_push_job_status(?PUSHY_JOB_ID, ?PUSHY_JOB_STATUS),
               Got = deliv_chef_api:push_job_status(?PUSHY_JOB_ID),
               PushTime = <<"Fri, 31 May 2013 22:11:54 GMT">>,
               Expect = {ok,
                         #push_job_status{
                            id = erlang:iolist_to_binary(?PUSHY_JOB_ID),
                            command = <<"chef-client">>,
                            status = <<"quorum_failed">>,
                            run_timeout = 3600,
                            created_at = PushTime,
                            updated_at = PushTime,
                            total_nodes = 1,
                            nodes = [{<<"unavailable">>, [<<"alice">>]}]
                           }
                        },
               ?assertEqual(Expect, Got),
               ?assert(meck:validate(chef_req))
       end},

      {"error pushy job status call",
       fun() ->
               chef_req_meck_push_job_status(?PUSHY_JOB_ID, ?PUSHY_JOB_ERROR),
               Got = deliv_chef_api:push_job_status(?PUSHY_JOB_ID),
               Expect = {error, ?PUSHY_JOB_ERROR},
               ?assertEqual(Expect, Got),
               ?assert(meck:validate(chef_req))
       end}
     ]).

%% Meck chef_req:request via meck for a push job status request.
chef_req_meck_push_job_status(JobId, Return) ->
    ExpectPath = "/pushy/jobs/" ++ JobId,
    meck:expect(chef_req, request,
                fun(get, ReqPath, _) ->
                        ?assertEqual(ExpectPath, ReqPath),
                        Return
                end),
    Return.

%%%% Search!
-define(SEARCH_OK,
        {ok, "200",
         [{"Server","nginx/1.2.3"},
          {"Date","Mon, 03 Jun 2013 18:23:55 GMT"},
          {"Content-Type","application/json"},
          {"Content-Length","117"},
          {"Connection","keep-alive"},
          {"X-Ops-API-Info","flavor=opc;version=11.0.0;oc_erchef=0.19.6"}],
         <<"{\"total\":1,\"start\":0,\"rows\":"
           "[{\"url\":\"https:\\/\\/10.0.2.15\\/organizations"
           "\\/cd\\/nodes\\/alice\",\"data\":{\"name\":\"alice\"}}]}">>}).

-define(SEARCH_MULTIPLE_OK,
        {ok, "200",
         [{"Server","nginx/1.2.3"},
          {"Date","Mon, 03 Jun 2013 18:23:55 GMT"},
          {"Content-Type","application/json"},
          {"Content-Length","117"},
          {"Connection","keep-alive"},
          {"X-Ops-API-Info","flavor=opc;version=11.0.0;oc_erchef=0.19.6"}],
         <<"{\"total\":1,\"start\":0,\"rows\":"
           "[{\"url\":\"https:\\/\\/10.0.2.15\\/organizations"
           "\\/cd\\/nodes\\/alice\",\"data\":{\"name\":\"alice\"}}]}">>}).

-define(SEARCH_404,
        {ok, "404",
         [{"Server","nginx/1.2.3"},
          {"Date","Mon, 03 Jun 2013 19:38:26 GMT"},
          {"Content-Type","application/json"},
          {"Content-Length","66"},
          {"Connection","keep-alive"},
          {"X-Ops-API-Info",
           "flavor=opc;version=11.0.0;oc_erchef=0.19.6"}],
         <<"{\"error\":[\"I don't know how to search for"
           "thought data objects.\"]}">>}).

search_test_() ->
    setup([{"successful search call",
       fun() ->
               chef_req_meck_search("node", "node:alice",
                                    {[{<<"name">>, [<<"name">>]}]},
                                    ?SEARCH_OK),
               Rows = [{[
                         {<<"url">>,
                          <<"https://10.0.2.15/organizations/cd/nodes/alice">>},
                         {<<"data">>, {[{<<"name">>,<<"alice">>}]}}
                        ]}],
               Result = deliv_chef_api:search("node",
                                              "node:alice",
                                              <<"{\"name\":[\"name\"]}">>),
               ?assertEqual({ok, #search{total=1, start=0, rows=Rows}}, Result)
       end},

      {"error search call: bogus index",
       fun() ->
               chef_req_meck_search("thought", "node:alice",
                                    {[{<<"name">>, [<<"name">>]}]},
                                    ?SEARCH_404),
               Result = deliv_chef_api:search("thought",
                                              "node:alice",
                                              <<"{\"name\":[\"name\"]}">>),
               ?assertEqual({error, ?SEARCH_404}, Result)
       end}
     ]).

chef_req_meck_search(Index, Query, Attrs, Result) ->
    EncQuery = deliv_web_utils:encode_url(Query),
    ExpectPath = "/search/" ++ Index ++ "?q=" ++ EncQuery,
    ExpectBody = chef_json:encode(Attrs),
    meck:expect(chef_req, request,
                fun(post, Path, Body, _) ->
                        ?assertEqual(ExpectPath, Path),
                        ?assertEqual(ExpectBody, Body),
                        Result
                end).

setup(Tests) ->
    {setup,
     fun() ->
         meck:new(chef_req),
         application:set_env(delivery, chef_req_config, ?CHEF_CONFIG),
         application:set_env(delivery, push_jobs_run_timeout, 4500)
     end,
     fun(_) ->
         ct_meck:unload(chef_req),
         application:unset_env(delivery, chef_req_config),
         application:unset_env(delivery, push_jobs_run_timeout)
     end,
     Tests}.
