-module(deliv_pipelines_common_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

get_pipelines_stats_as_ejson_no_pipelines_test_() ->
    EntName = <<"no_pipelines">>,
    Pipelines = [],
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_enterprise],
             meck:new(Mods),
             meck:expect(deliv_enterprise, pipelines_with_stats, [EntName], {ok, Pipelines}),
             Mods
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_pipeline_common:get_pipelines_stats_as_ejson(<<"no_pipelines">>),
              ?assertEqual({ok, []}, Ans)
      end
     ]}.

get_pipelines_stats_as_ejson_enterprise_not_found_test_() ->
    EntName = <<"not_found">>,
    Error = {error,{error,{error,error, <<"CD003">>,
                     <<"Enterprise not found">>,
                     [{hint, <<"Make sure the enterprise exists, and the name is spelled correctly">>},
                      {detail, <<"Enterprise \"not_found\" not found">>}]}}},
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_enterprise],
             meck:new(Mods),
             meck:expect(deliv_enterprise, pipelines_with_stats, [EntName], Error),
             Mods
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_pipeline_common:get_pipelines_stats_as_ejson(<<"not_found">>),
              ?assertEqual(Error, Ans)
      end
     ]}.

get_pipelines_stats_as_ejson_test_() ->
    EntName = <<"Chef">>,
    Pipelines = [[{<<"id">>,21},
                  {<<"org_name">>, <<"Adam Sandbox">>},
                  {<<"proj_name">>, <<"init-test">>},
                  {<<"name">>, <<"master">>},
                  {<<"build_count">>, 3},
                  {<<"build_status">>, <<"running">>},
                  {<<"acceptance_count">>, 3},
                  {<<"acceptance_status">>, <<"passed">>},
                  {<<"last_deployed">>, {{2015,2,12},{21,26,48.943438}}},
                  {<<"last_delivered">>, {{2015,2,12},{21,26,48.943438}}}],
                 [{<<"id">>,22},
                  {<<"org_name">>,<<"Chef_Partners">>},
                  {<<"proj_name">>,<<"azure-chef-extension">>},
                  {<<"name">>,<<"master">>},
                  {<<"build_count">>, 0},
                  {<<"build_status">>, <<"passed">>},
                  {<<"acceptance_count">>, 0},
                  {<<"acceptance_status">>, <<"passed">>},
                  {<<"last_deployed">>, {{2015,2,12},{21,26,48.943438}}},
                  {<<"last_delivered">>, {{2015,2,12},{21,26,48.943438}}}],
                 [{<<"id">>,23},
                  {<<"org_name">>,<<"adam_universe">>},
                  {<<"proj_name">>,<<"delivery-cli">>},
                  {<<"name">>,<<"master">>},
                  {<<"build_count">>, 2},
                  {<<"build_status">>, <<"failed">>},
                  {<<"acceptance_count">>, 1},
                  {<<"acceptance_status">>, <<"passed">>},
                  {<<"last_deployed">>, {{2015,2,12},{21,26,48.943438}}},
                  {<<"last_delivered">>, {{2015,2,12},{21,26,48.943438}}}]
                ],
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_enterprise],
             meck:new(Mods),
             meck:expect(deliv_enterprise, pipelines_with_stats, [EntName], {ok, Pipelines}),
             Mods
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_pipeline_common:get_pipelines_stats_as_ejson(<<"Chef">>),
              ?assertEqual(expected_pipelines_stats_ejson(), Ans)
      end
     ]}.

get_pipeline_as_ejson_enterprise_not_found_test_() ->
    Error = {error,{error,{error,error, <<"CD003">>,
                     <<"Enterprise not found">>,
                     [{hint, <<"Make sure the enterprise exists, and the name is spelled correctly">>},
                      {detail, <<"Enterprise \"not_found\" not found">>}]}}},
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_pipeline],
             meck:new(Mods),
             meck:expect(deliv_pipeline, fetch,
                         [<<"not_found">>, <<"Chef_Delivery">>,
                          <<"delivery">>, <<"master">>], Error),
             Mods
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_pipeline_common:get_pipeline_as_ejson(<<"not_found">>,
                                                                <<"Chef_Delivery">>,
                                                                <<"delivery">>,
                                                                <<"master">>),
              ?assertEqual(Error, Ans)
      end
     ]}.

get_pipeline_as_ejson_organization_not_found_test_() ->
    Error = {error,{error,{error,error, <<"CD005">>,
                     <<"Organization not found">>,
                     [{hint, <<"Make sure the organization exists, and the name is spelled correctly">>},
                      {detail, <<"Organization \"not_found\" not found">>}]}}},
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_pipeline],
             meck:new(Mods),
             meck:expect(deliv_pipeline, fetch,
                         [<<"Chef">>, <<"not_found">>,
                          <<"delivery">>, <<"master">>], Error),
             Mods
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_pipeline_common:get_pipeline_as_ejson(<<"Chef">>,
                                                                <<"not_found">>,
                                                                <<"delivery">>,
                                                                <<"master">>),
              ?assertEqual(Error, Ans)
      end
     ]}.

get_pipeline_as_ejson_project_not_found_test_() ->
    Error = {error,{error,{error,error, <<"CD006">>,
                     <<"Project not found">>,
                     [{hint, <<"Make sure the project exists, and the name is spelled correctly">>},
                      {detail, <<"Project \"not_found\" not found">>}]}}},
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_pipeline],
             meck:new(Mods),
             meck:expect(deliv_pipeline, fetch,
                         [<<"Chef">>, <<"Chef_Delivery">>,
                          <<"not_found">>, <<"master">>], Error),
             Mods
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_pipeline_common:get_pipeline_as_ejson(<<"Chef">>,
                                                                <<"Chef_Delivery">>,
                                                                <<"not_found">>,
                                                                <<"master">>),
              ?assertEqual(Error, Ans)
      end
     ]}.

get_pipeline_as_ejson_pipeline_not_found_test_() ->
    Error = {error,{error,{error,error, <<"CD006">>,
                     <<"Pipeline not found">>,
                     [{hint, <<"Make sure the pipeline exists, and the name is spelled correctly">>},
                      {detail, <<"Pipeline \"not_found\" not found">>}]}}},
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_pipeline],
             meck:new(Mods),
             meck:expect(deliv_pipeline, fetch,
                         [<<"Chef">>, <<"Chef_Delivery">>,
                          <<"delivery">>, <<"not_found">>], Error),
             Mods
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_pipeline_common:get_pipeline_as_ejson(<<"Chef">>,
                                                                <<"Chef_Delivery">>,
                                                                <<"delivery">>,
                                                                <<"not_found">>),
              ?assertEqual(Error, Ans)
      end
     ]}.

get_pipeline_as_ejson_test_() ->
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_pipeline],
             meck:new(Mods, [passthrough]),
             meck:expect(deliv_pipeline, fetch,
                         [<<"Chef">>, <<"Chef_Delivery">>,
                          <<"delivery">>, <<"master">>],
                         {ok,{deliv_pipeline,2,1,<<"master">>}}),
             Mods
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_pipeline_common:get_pipeline_as_ejson(<<"Chef">>,
                                                                <<"Chef_Delivery">>,
                                                                <<"delivery">>,
                                                                <<"master">>),
              ?assertEqual(expected_pipeline_ejson(), Ans)
      end
     ]}.


expected_pipelines_stats_ejson() ->
    {ok,
     [{[
        {<<"id">>,21},
        {<<"org">>,<<"Adam Sandbox">>},
        {<<"project">>,<<"init-test">>},
        {<<"pipeline">>,<<"master">>},
        {<<"stats">>,{[
                       {<<"build">>,{[
                                      {<<"changes">>,3},
                                      {<<"state">>,<<"running">>}
                                    ]}},
                       {<<"acceptance">>,{[
                                           {<<"changes">>,3},
                                           {<<"state">>,<<"passed">>}
                                         ]}}
                     ]}},
         {<<"deployed">>,<<"2015-02-12 21:26:48">>},
         {<<"delivered">>,<<"2015-02-12 21:26:48">>}
      ]},
      {[
         {<<"id">>,22},
         {<<"org">>,<<"Chef_Partners">>},
         {<<"project">>,<<"azure-chef-extension">>},
         {<<"pipeline">>,<<"master">>},
         {<<"stats">>,{[
                        {<<"build">>,{[
                                       {<<"changes">>,0},
                                       {<<"state">>,<<"passed">>}
                                     ]}},
                        {<<"acceptance">>,{[
                                            {<<"changes">>,0},
                                            {<<"state">>,<<"passed">>}
                                          ]}}
                      ]}},
          {<<"deployed">>,<<"2015-02-12 21:26:48">>},
          {<<"delivered">>,<<"2015-02-12 21:26:48">>}
       ]},
       {[
          {<<"id">>,23},
          {<<"org">>,<<"adam_universe">>},
          {<<"project">>,<<"delivery-cli">>},
          {<<"pipeline">>,<<"master">>},
          {<<"stats">>,{[
                         {<<"build">>,{[
                                        {<<"changes">>,2},
                                        {<<"state">>,<<"failed">>}
                                      ]}},
                         {<<"acceptance">>,{[
                                             {<<"changes">>,1},
                                             {<<"state">>,<<"passed">>}
                                           ]}}
                       ]}},
           {<<"deployed">>,<<"2015-02-12 21:26:48">>},
           {<<"delivered">>,<<"2015-02-12 21:26:48">>}
        ]}]}.

expected_pipeline_ejson() ->
    {ok,
       {[{<<"id">>, 2},
         {<<"name">>, <<"master">>},
         {<<"_links">>,
            {[{<<"self">>,
               {[{<<"href">>, <<"/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master">>}]}},
             {<<"changes">>,
               {[{<<"href">>, <<"/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/changes?pipeline=master&limit=25">>}]}}
            ]}}
       ]}}.

meck_app_config() ->
    meck:new(delivery_app),
    meck:expect(delivery_app, get_env,
                fun(hostname) ->
                        "127.0.0.1";
                   (api_proto) ->
                        "https"
                end),
    [].

meck_unload(OtherMods) ->
    ct_meck:unload([delivery_app | OtherMods]).
