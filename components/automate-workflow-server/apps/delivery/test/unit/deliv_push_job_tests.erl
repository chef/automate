%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Adam Jacob <adam@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
%%
-module(deliv_push_job_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../../src/deliverance_types.hrl").

-define(PUSH_JOB_STATE, deliv_push_job:example_state()).

-define(PUSH_JOB_STATUS_OK,
        #push_job_status{id= <<"97f0102c4e3438af386894684e0ea085">>,
                         command= <<"deliverance">>,
                         status= <<"complete">>,
                         run_timeout= 3600,
                         created_at= <<"Fri, 31 May 2013 22:11:54 GMT">>,
                         updated_at= <<"Fri, 31 May 2013 22:11:54 GMT">>,
                         total_nodes= 1,
                         nodes= [{<<"complete">>,
                                  [<<"alice">>]}]}).

do_start_test_() ->
    {foreach,
     fun() ->
             application:set_env(delivery, push_jobs_overall_timeout, 60 * 60 * 2),
             meck:new(deliv_chef_api),
             meck:new(deliv_phase),
             meck:expect(deliv_phase, update, 2, ok),
             meck:expect(deliv_phase, finished, 1, ok),
             meck:expect(deliv_chef_api,
                         push_job_start,
                         fun(<<"deliverance">>, [<<"alice">>], 1) ->
                                 {ok, "97f0102c4e3438af386894684e0ea085"}
                         end),
             meck:expect(deliv_chef_api,
                         push_job_status,
                         fun("97f0102c4e3438af386894684e0ea085") ->
                                 {ok, ?PUSH_JOB_STATUS_OK}
                         end),
             meck:expect(deliv_chef_api,
                         search,
                         fun("node", <<"alice">>,
                             <<"{\"name\":[\"name\"]}">>) ->
                                 {ok, #search{total = 1,
                                              start = 1,
                                              rows = [{[{<<"data">>,
                                                         {[{<<"name">>,
                                                           <<"alice">>}]}}]}]
                                             }}
                         end),
             meck:expect(deliv_chef_api,
                         push_node_states,
                         fun() ->
                                 {ok, [{[{<<"node_name">>, <<"alice">>},
                                         {<<"availability">>,
                                          <<"available">>}]}]}
                         end)
     end,
     fun(_) ->
             ok = ct_meck:unload([deliv_chef_api, deliv_phase]),
             application:unset_env(delivery, push_jobs_overall_timeout)
     end,
     [{"starts a deliverance push job",
       fun() ->
               deliv_push_job:do_start(?PUSH_JOB_STATE, <<"alice">>),
               ?assert(meck:validate(deliv_chef_api))
       end}
     ]}.

start_test_() ->
    {foreach,
     fun() ->
             application:set_env(delivery, push_jobs_overall_timeout, 60 * 60 * 2),
             meck:new(deliv_phase),
             meck:expect(deliv_phase, update, 2, ok),
             meck:expect(deliv_phase, finished, 1, ok),
             meck:new(deliv_chef_api),
             meck:expect(deliv_chef_api,
                         push_job_start,
                         fun(<<"deliverance">>, [<<"alice">>], 1) ->
                                 {ok, "97f0102c4e3438af386894684e0ea085"}
                         end),
             meck:expect(deliv_chef_api,
                         push_job_status,
                         fun("97f0102c4e3438af386894684e0ea085") ->
                                 {ok, ?PUSH_JOB_STATUS_OK}
                         end),
             meck:expect(deliv_chef_api,
                         search,
                         fun("node", <<"alice">>,
                             <<"{\"name\":[\"name\"]}">>) ->
                                 {ok, #search{total = 1,
                                              start = 1,
                                              rows = [{[{<<"data">>,
                                                         {[{<<"name">>,
                                                            <<"alice">>}]}}]}]
                                             }}
                         end),
             meck:expect(deliv_chef_api,
                         push_node_states,
                         fun() ->
                                 {ok, [{[{<<"node_name">>, <<"alice">>},
                                         {<<"availability">>,
                                          <<"available">>}]}]}
                         end)

     end,
     fun(_) ->
             ct_meck:unload(deliv_chef_api),
             ct_meck:unload(deliv_phase),
             application:unset_env(delivery, push_jobs_overall_timeout)
     end,
     [
      {"starts a deliverance push job through gen_server:call",
       {setup,
        fun() ->
                deliv_push_job:start_link(1, <<"alice">>, <<"deliverance">>)
        end,
        fun({ok, Pid}) ->
                deliv_push_job:stop(Pid);
           (_) ->
                ok
        end,
        fun({ok, Pid}) ->
                [
                 fun() ->
                         Result = deliv_push_job:start(Pid),
                         ?assertEqual(ok, Result)
                 end
                ]
        end}}
     ]}.

do_status_test_() ->
    {foreach,
     fun() ->
             meck:new(deliv_chef_api),
             meck:new(deliv_phase),
             meck:expect(deliv_phase, update, 2, ok),
             meck:expect(deliv_phase, finished, 1, ok),
             meck:expect(deliv_chef_api,
                         push_job_status,
                         fun("97f0102c4e3438af386894684e0ea085") ->
                                 {ok, ?PUSH_JOB_STATUS_OK}
                         end)
     end,
     fun(_) ->
             ct_meck:unload([deliv_phase, deliv_chef_api])
     end,
     [{"calls deliv_chef_api:push_job_status",
       fun() ->
             deliv_push_job:do_status(?PUSH_JOB_STATE,
                                      "97f0102c4e3438af386894684e0ea085"),
             ?assert(meck:validate(deliv_chef_api))
       end}
     ]}.

do_find_node_test_() ->
    {foreach,
     fun() ->
             meck:new(deliv_chef_api),
             meck:new(deliv_phase),
             meck:expect(deliv_phase, update, 2, ok),
             meck:expect(deliv_phase, finished, 1, ok),
             meck:expect(deliv_chef_api,
                         search,
                         fun("node", <<"builder">>,
                             <<"{\"name\":[\"name\"]}">>) ->
                                 {ok, #search{total = 3,
                                              start = 1,
                                              rows =  [{[{<<"data">>,
                                                         {[{<<"name">>,
                                                            <<"alice">>}]}}]},
                                                       {[{<<"data">>,
                                                         {[{<<"name">>,
                                                            <<"bob">>}]}}]},
                                                       {[{<<"data">>,
                                                         {[{<<"name">>,
                                                            <<"frank">>}]}}]},
                                                       {[{<<"data">>,
                                                         {[{<<"name">>,
                                                            <<"jon">>}]}}]}]
                                             }}
                         end),
             meck:expect(deliv_chef_api,
                         push_node_states,
                         fun() ->
                                 {ok, [{[{<<"node_name">>, <<"alice">>},
                                         {<<"availability">>,
                                          <<"available">>}]},
                                       {[{<<"node_name">>, <<"bob">>},
                                         {<<"availability">>,
                                          <<"available">>}]},
                                       {[{<<"node_name">>, <<"frank">>},
                                         {<<"availability">>,
                                          <<"available">>}]},
                                       {[{<<"node_name">>, <<"jon">>},
                                         {<<"availability">>,
                                          <<"available">>}]}
                                       ]}
                         end)
     end,
     fun(_) ->
             ct_meck:unload(deliv_phase),
             ct_meck:unload(deliv_chef_api)
     end,
     [{"calls deliv_chef_api:find_node",
       fun() ->
           RandNodes = [deliv_push_job:find_node(<<"builder">>) || _ <- lists:seq(1, 100)],
           [?assert(lists:member({ok, NodeName}, RandNodes))
            || NodeName <- [<<"alice">>, <<"bob">>, <<"frank">>, <<"jon">>]]
       end}
     ]}.
