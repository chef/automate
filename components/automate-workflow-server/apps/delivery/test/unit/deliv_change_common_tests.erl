-module(deliv_change_common_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

change_to_ejson_open_test_() ->
    ChangeId = <<"8725f96f-838c-4add-897e-06a48894382">>,
    RepoCoordinates = {<<"Chef">>, <<"Chef_Delivery">>, <<"delivery">>},
    Result = [{<<"id">>, ChangeId},
              {<<"status">>, <<"open">>},
              {<<"pipeline">>, <<"master">>},
              {<<"feature_branch">>, <<"sf/sunlamp">>},
              {<<"submitted_at">>, {{2014, 10, 28}, {20, 31, 42.4}}},
              {<<"submitted_by">>, <<"jra">>},
              {<<"merge_sha">>, <<"212dada6c9d2384fbdb0c9505457d4f2e5cb9512">>},
              {<<"patchsets">>,[{<<"sha">>, <<"607b39702ae1494b3f2c95d3e8804b861d30af18">>}]}
             ],
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_change, deliv_patchset, deliv_user, deliv_pipeline, deliv_project],
             meck:new(Mods),
             meck:expect(deliv_change, get_phase_run_summary,
                         fun(X) when X =:= ChangeId ->
                                 phase_run_summary_example()
                         end),
             meck:expect(deliv_change, get_stage_run_summary,
                         fun(X) when X =:= ChangeId ->
                                 deliv_stage_run_summary_tests:stage_run_summary_verify_successful()
                         end),
             meck:expect(deliv_change, fetch_by_id,
                         fun(X) when X =:= ChangeId ->
                                 change_example()
                         end),
             meck:expect(deliv_change, status,
                         fun(a_change) ->
                                 open
                         end),
             meck:expect(deliv_change, getval,
                         fun(title, a_change) -> <<"This is a title">>;
                            (description, a_change) -> <<"This is a description">>;
                            (id, a_change) -> ChangeId;
                            (latest_patchset_status, a_change) -> <<"open">>;
                            (pipeline_name_at_creation, a_change) -> <<"master">>;
                            (submitted_at , a_change) -> {{2014, 10, 28}, {20, 31, 42.4}};
                            (submitted_by, a_change) -> <<"jra">>;
                            (merge_sha, a_change) -> proplists:get_value(<<"merge_sha">>, Result);
                            (feature_branch, a_change) -> proplists:get_value(<<"feature_branch">>, Result);
                            (approved_by, a_change) -> undefined;
                            (delivered_by, a_change) -> undefined;
                            (delivered_at, a_change) -> undefined;
                            (pipeline_id, a_change) -> 10;
                            (superseding_change_id, a_change) -> undefined
                         end),
             meck:expect(deliv_patchset, patchsets_for_change,
                         fun(X) when X =:= ChangeId ->
                                 []
                         end),
             meck:expect(deliv_user, effective_roles,
                         fun(pipeline, [_, <<"admin">>, _, _, _]) ->
                                 {ok, [<<"admin">>]}
                         end),
             meck:expect(deliv_change, get_superseding_change,
                         fun(_) ->
                                 undefined
                         end),
             meck:expect(deliv_project, fetch,
                         fun(_, _, _) ->
                                 {ok, project}
                         end),
             meck:expect(deliv_project, getval,
                         fun(scm_module, _) ->
                                 <<"local">>
                         end),
             meck:expect(deliv_scopes, from_change_id,
                         fun(ChangeId2) ->
                                 deliv_scopes:'#new_common'([{ent_name, <<"Chef">>},
                                                             {org_name, <<"Chef_Delivery">>},
                                                             {proj_name, <<"delivery">>},
                                                             {pipe_name, <<"master">>},
                                                             {change_id, ChangeId2},
                                                             {scm_module, deliv_scm_local}
                                                            ])
                         end),
             Mods ++ [deliv_scopes]
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_change_common:change_to_ejson(RepoCoordinates,
                                                        <<"admin">>, a_change),
              ?assertEqual(expected_change_ejson(), Ans)
      end
     ]}.

change_to_ejson_merged_test_() ->
    ChangeId = <<"8725f96f-838c-4add-897e-06a48894382">>,
    RepoCoordinates = {<<"Chef">>, <<"Chef_Delivery">>, <<"delivery">>},
    Result = [{<<"id">>, ChangeId},
              {<<"status">>, <<"open">>},
              {<<"pipeline">>, <<"master">>},
              {<<"feature_branch">>, <<"sf/sunlamp">>},
              {<<"submitted_at">>, {{2014, 10, 28}, {20, 31, 42.4}}},
              {<<"submitted_by">>, <<"jra">>},
              {<<"merge_sha">>, <<"212dada6c9d2384fbdb0c9505457d4f2e5cb9512">>},
              {<<"patchsets">>,[{<<"sha">>, <<"607b39702ae1494b3f2c95d3e8804b861d30af18">>}]}
             ],
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_change, deliv_patchset, deliv_user, deliv_pipeline, deliv_project],
             meck:new(Mods),
             meck:expect(deliv_change, get_phase_run_summary,
                         fun(X) when X =:= ChangeId ->
                                 phase_run_summary_example()
                         end),
             meck:expect(deliv_change, get_stage_run_summary,
                         fun(X) when X =:= ChangeId ->
                                 deliv_stage_run_summary_tests:stage_run_summary_verify_successful()
                         end),
             meck:expect(deliv_change, fetch_by_id,
                         fun(X) when X =:= ChangeId ->
                                 change_example()
                         end),
             meck:expect(deliv_change, status,
                         fun(a_change) ->
                                 open
                         end),
             meck:expect(deliv_change, getval,
                         fun(title, a_change) -> <<"This is a title">>;
                            (description, a_change) -> <<"This is a description">>;
                            (id, a_change) -> ChangeId;
                            (latest_patchset_status, a_change) -> <<"merged">>;
                            (pipeline_name_at_creation, a_change) -> <<"master">>;
                            (submitted_at, a_change) -> {{2014, 10, 28}, {20, 31, 42.4}};
                            (submitted_by, a_change) -> <<"jra">>;
                            (merge_sha, a_change) -> proplists:get_value(<<"merge_sha">>, Result);
                            (feature_branch, a_change) -> proplists:get_value(<<"feature_branch">>, Result);
                            (approved_by, a_change) -> undefined;
                            (delivered_by, a_change) -> undefined;
                            (delivered_at, a_change) -> undefined;
                            (pipeline_id, a_change) -> 10;
                            (superseding_change_id, a_change) -> undefined
                         end),
             meck:expect(deliv_patchset, patchsets_for_change,
                         fun(X) when X =:= ChangeId ->
                                 []
                         end),
             meck:expect(deliv_change, get_superseding_change,
                         fun(_) ->
                                 undefined
                         end),
             meck:expect(deliv_project, fetch,
                         fun(_, _, _) ->
                                 {ok, project}
                         end),
             meck:expect(deliv_project, getval,
                         fun(scm_module, _) ->
                                 <<"local">>
                         end),
             meck:expect(deliv_user, effective_roles,
                         fun(pipeline, [_, <<"admin">>, _, _, _]) ->
                                 {ok, [<<"admin">>]}
                         end),
             meck:expect(deliv_scopes, from_change_id,
                         fun(ChangeId2) ->
                                 deliv_scopes:'#new_common'([{ent_name, <<"Chef">>},
                                                             {org_name, <<"Chef_Delivery">>},
                                                             {proj_name, <<"delivery">>},
                                                             {pipe_name, <<"master">>},
                                                             {change_id, ChangeId2},
                                                             {scm_module, deliv_scm_local}
                                                            ])
                         end),
             Mods ++ [deliv_scopes]
     end,
     fun meck_unload/1,
     [
      fun() ->
              {ok, Ans} = deliv_change_common:change_to_ejson(RepoCoordinates,
                                                              <<"admin">>,
                                                              a_change),
              {ok, OrigExpect} = expected_change_ejson(),
              Expected0 = ej:delete(["_links", "delete"], OrigExpect),
              Expected1 = ej:delete(["_links", "approve"], Expected0),
              Expected = ej:set(["state"], Expected1, <<"merged">>),
              {ExpectedList} = Expected,
              {AnsList} = Ans,
              [?assertEqual(Val1, Val2) || {Val1, Val2} <- lists:zip(ExpectedList, AnsList)],
              ?assertEqual(Expected, Ans)
      end
     ]}.

change_to_ejson_bitbucket_test_() ->
    ChangeId = <<"8725f96f-838c-4add-897e-06a48894382">>,
    RepoCoordinates = {<<"Chef">>, <<"Chef_Delivery">>, <<"delivery">>},
    Result = [{<<"id">>, ChangeId},
              {<<"status">>, <<"open">>},
              {<<"pipeline">>, <<"master">>},
              {<<"feature_branch">>, <<"sf/sunlamp">>},
              {<<"submitted_at">>, {{2014, 10, 28}, {20, 31, 42.4}}},
              {<<"submitted_by">>, <<"jra">>},
              {<<"merge_sha">>, <<"212dada6c9d2384fbdb0c9505457d4f2e5cb9512">>},
              {<<"patchsets">>,[{<<"sha">>, <<"607b39702ae1494b3f2c95d3e8804b861d30af18">>}]}
             ],
    PullRequestIdBin = <<"1">>,
    PullRequestTitle = <<"Bitbucket Pull Request (#", PullRequestIdBin/binary, ")">>,
    PullRequestUrl = <<"http://bitbucket-pr">>,
    HalMetaData = {[{<<"href">>, PullRequestUrl},
                    {<<"title">>, PullRequestTitle}]},

    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_change, deliv_patchset, deliv_user, deliv_pipeline, deliv_project, scm_change],
             meck:new(Mods),
             meck:expect(deliv_change, get_phase_run_summary,
                         fun(X) when X =:= ChangeId ->
                                 phase_run_summary_example()
                         end),
             meck:expect(deliv_change, get_stage_run_summary,
                         fun(X) when X =:= ChangeId ->
                                 deliv_stage_run_summary_tests:stage_run_summary_verify_successful()
                         end),
             meck:expect(deliv_change, fetch_by_id,
                         fun(X) when X =:= ChangeId ->
                                 change_example()
                         end),
             meck:expect(deliv_change, status,
                         fun(a_change) ->
                                 open
                         end),
             meck:expect(deliv_change, getval,
                         fun(title, a_change) -> <<"This is a title">>;
                            (description, a_change) -> <<"This is a description">>;
                            (id, a_change) -> ChangeId;
                            (latest_patchset_status, a_change) -> <<"open">>;
                            (pipeline_name_at_creation, a_change) -> <<"master">>;
                            (submitted_at , a_change) -> {{2014, 10, 28}, {20, 31, 42.4}};
                            (submitted_by, a_change) -> <<"jra">>;
                            (merge_sha, a_change) -> proplists:get_value(<<"merge_sha">>, Result);
                            (feature_branch, a_change) -> proplists:get_value(<<"feature_branch">>, Result);
                            (approved_by, a_change) -> undefined;
                            (delivered_by, a_change) -> undefined;
                            (delivered_at, a_change) -> undefined;
                            (pipeline_id, a_change) -> 10;
                            (superseding_change_id, a_change) -> undefined
                         end),
             meck:expect(deliv_patchset, patchsets_for_change,
                         fun(X) when X =:= ChangeId ->
                                 []
                         end),
             meck:expect(deliv_user, effective_roles,
                         fun(pipeline, [_, <<"admin">>, _, _, _]) ->
                                 {ok, [<<"admin">>]}
                         end),
             meck:expect(deliv_change, get_superseding_change,
                         fun(_) ->
                                 undefined
                         end),
             meck:expect(deliv_project, fetch,
                         fun(_, _, _) ->
                                 {ok, project}
                         end),
             meck:expect(deliv_project, getval,
                         fun(scm_module, _) ->
                                 <<"bitbucket_scm">>
                         end),
             meck:expect(scm_change, get_hal,
                         fun(_ChangeId, bitbucket_scm) ->
                            HalMetaData
                         end),
             meck:expect(deliv_scopes, from_change_id,
                         fun(ChangeId2) ->
                                 deliv_scopes:'#new_common'([{ent_name, <<"Chef">>},
                                                             {org_name, <<"Chef_Delivery">>},
                                                             {proj_name, <<"delivery">>},
                                                             {pipe_name, <<"master">>},
                                                             {change_id, ChangeId2},
                                                             {scm_module, deliv_scm_local}
                                                            ])
                         end),
             Mods ++ [deliv_scopes]
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_change_common:change_to_ejson(RepoCoordinates,
                                                        <<"admin">>, a_change),
              {ok, Ejson} = expected_change_ejson(),
              Expected =  ej:set({"_links", "external_pr"}, Ejson, {[
                  {<<"href">>, PullRequestUrl},
                  {<<"title">>, PullRequestTitle}
              ]}),
              ?assertEqual({ok, Expected}, Ans)
      end
     ]}.

change_to_ejson_github_test_() ->
    ChangeId = <<"8725f96f-838c-4add-897e-06a48894382">>,
    RepoCoordinates = {<<"Chef">>, <<"Chef_Delivery">>, <<"delivery">>},
    Result = [{<<"id">>, ChangeId},
              {<<"status">>, <<"open">>},
              {<<"pipeline">>, <<"master">>},
              {<<"feature_branch">>, <<"sf/sunlamp">>},
              {<<"submitted_at">>, {{2014, 10, 28}, {20, 31, 42.4}}},
              {<<"submitted_by">>, <<"jra">>},
              {<<"merge_sha">>, <<"212dada6c9d2384fbdb0c9505457d4f2e5cb9512">>},
              {<<"patchsets">>,[{<<"sha">>, <<"607b39702ae1494b3f2c95d3e8804b861d30af18">>}]}
             ],
    PullRequestIdBin = <<"1">>,
    PullRequestTitle = <<"GitHub Pull Request (#", PullRequestIdBin/binary, ")">>,
    PullRequestUrl = <<"http://github-pr">>,
    HalMetaData = {[{<<"href">>, PullRequestUrl},
                    {<<"title">>, PullRequestTitle}]},

    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_change, deliv_patchset, deliv_user, deliv_pipeline, deliv_project, scm_change],
             meck:new(Mods),
             meck:expect(deliv_change, get_phase_run_summary,
                         fun(X) when X =:= ChangeId ->
                                 phase_run_summary_example()
                         end),
             meck:expect(deliv_change, get_stage_run_summary,
                         fun(X) when X =:= ChangeId ->
                                 deliv_stage_run_summary_tests:stage_run_summary_verify_successful()
                         end),
             meck:expect(deliv_change, fetch_by_id,
                         fun(X) when X =:= ChangeId ->
                                 change_example()
                         end),
             meck:expect(deliv_change, status,
                         fun(a_change) ->
                                 open
                         end),
             meck:expect(deliv_change, getval,
                         fun(title, a_change) -> <<"This is a title">>;
                            (description, a_change) -> <<"This is a description">>;
                            (id, a_change) -> ChangeId;
                            (latest_patchset_status, a_change) -> <<"open">>;
                            (pipeline_name_at_creation, a_change) -> <<"master">>;
                            (submitted_at , a_change) -> {{2014, 10, 28}, {20, 31, 42.4}};
                            (submitted_by, a_change) -> <<"jra">>;
                            (merge_sha, a_change) -> proplists:get_value(<<"merge_sha">>, Result);
                            (feature_branch, a_change) -> proplists:get_value(<<"feature_branch">>, Result);
                            (approved_by, a_change) -> undefined;
                            (delivered_by, a_change) -> undefined;
                            (delivered_at, a_change) -> undefined;
                            (pipeline_id, a_change) -> 10;
                            (superseding_change_id, a_change) -> undefined
                         end),
             meck:expect(deliv_patchset, patchsets_for_change,
                         fun(X) when X =:= ChangeId ->
                                 []
                         end),
             meck:expect(deliv_user, effective_roles,
                         fun(pipeline, [_, <<"admin">>, _, _, _]) ->
                                 {ok, [<<"admin">>]}
                         end),
             meck:expect(deliv_change, get_superseding_change,
                         fun(_) ->
                                 undefined
                         end),
             meck:expect(deliv_project, fetch,
                         fun(_, _, _) ->
                                 {ok, project}
                         end),
             meck:expect(deliv_project, getval,
                         fun(scm_module, _) ->
                                 <<"github_scm">>
                         end),
             meck:expect(scm_change, get_hal,
                         fun(_ChangeId, github_scm) ->
                            HalMetaData
                         end),
             meck:expect(deliv_scopes, from_change_id,
                         fun(ChangeId2) ->
                                 deliv_scopes:'#new_common'([{ent_name, <<"Chef">>},
                                                             {org_name, <<"Chef_Delivery">>},
                                                             {proj_name, <<"delivery">>},
                                                             {pipe_name, <<"master">>},
                                                             {change_id, ChangeId2},
                                                             {scm_module, deliv_scm_local}
                                                            ])
                         end),
             Mods ++ [deliv_scopes]
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_change_common:change_to_ejson(RepoCoordinates,
                                                        <<"admin">>, a_change),
              {ok, Ejson} = expected_change_ejson(),
              Expected =  ej:set({"_links", "external_pr"}, Ejson, {[
                  {<<"href">>, PullRequestUrl},
                  {<<"title">>, PullRequestTitle}
              ]}),
              ?assertEqual({ok, Expected}, Ans)
      end
     ]}.

expected_change_ejson() ->
    {ok,
     {[{<<"id">>,<<"8725f96f-838c-4add-897e-06a48894382">>},
       {<<"topic">>,<<"sf/sunlamp">>},
       {<<"target">>,<<"master">>},
       {<<"state">>,<<"open">>},
       {<<"submit_at">>,<<"2014-10-28 20:31:42">>},
       {<<"submit_by">>,<<"jra">>},
       {<<"merge_sha">>, <<"212dada6c9d2384fbdb0c9505457d4f2e5cb9512">>},
       {<<"approved_by">>, undefined},
       {<<"title">>, <<"This is a title">>},
       {<<"description">>, <<"This is a description">>},
       {<<"delivered_by">>, undefined},
       {<<"delivered_at">>, <<"">>},
       {<<"promotion">>,
         {[
           {<<"status">>, <<"proceed">>}
         ]}
       },
       {<<"stages">>,
        [
          {[{<<"stage">>,<<"verify">>},
           {<<"status">>,<<"failed">>},
           {<<"phases">>,
            [
              {[{<<"name">>, <<"unit">>},
                 {<<"status">>, <<"passed">>},
                 {<<"run_details">>,
                    [
                      {[
                        {<<"status">>, <<"passed">>},
                        {<<"description">>, <<"Project A - (name:builder*.shd.chef.co AND platform_family:debian)">>},
                        {<<"href">>, <<"/api/v0/e/Chef/orgs/Chef_Delivery"
                         "/projects/delivery/pipelines/master/phase_runs/3">>},
                        {<<"job_href">>, <<"/api/v0/e/Chef/jobs/f72b54b968a37e15d5bcd600a34f80271c7d0043ebb91018eb1d0d94d0c628f1">>},
                        {<<"search_query">>, <<"(name:builder*.shd.chef.co AND platform_family:debian)">>},
                        {<<"search_description">>, undefined}
                      ]},
                      {[
                        {<<"status">>, <<"passed">>},
                        {<<"description">>, <<"Project A - (name:builder*.shd.chef.co AND platform_family:rhel)">>},
                        {<<"href">>, <<"/api/v0/e/Chef/orgs/Chef_Delivery"
                         "/projects/delivery/pipelines/master/phase_runs/4">>},
                        {<<"job_href">>, <<"/api/v0/e/Chef/jobs/78101db5e5dcbc32b8595dfbb61975a0957b7a5219d0526549bd9f8fd72a407f">>},
                        {<<"search_query">>, <<"(name:builder*.shd.chef.co AND platform_family:rhel)">>},
                        {<<"search_description">>, undefined}
                      ]}
                    ]
                 }
              ]},
              {[{<<"name">>, <<"lint">>},
                {<<"status">>, <<"failed">>},
                {<<"run_details">>,
                  [{[
                      {<<"status">>, <<"failed">>},
                      {<<"description">>, undefined},
                      {<<"href">>, <<"/api/v0/e/Chef/orgs/Chef_Delivery"
                        "/projects/delivery/pipelines/master/phase_runs/1">>},
                      {<<"job_href">>, <<"/api/v0/e/Chef/jobs/832f7c875aed1a93f65b52bb9ec55d4ee178410f0d294a075598a6bed1b96ac9">>},
                      {<<"search_query">>, undefined},
                      {<<"search_description">>, undefined}
                  ]}]
                }
              ]},
             {[
                {<<"name">>, <<"syntax">>},
                {<<"status">>, <<"passed">>},
                {<<"run_details">>,
                  [{[
                    {<<"status">>, <<"passed">>},
                    {<<"description">>, undefined},
                    {<<"href">>, <<"/api/v0/e/Chef/orgs/Chef_Delivery"
                    "/projects/delivery/pipelines/master/phase_runs/2">>},
                    {<<"job_href">>, <<"/api/v0/e/Chef/jobs/d13e999ce7fd6b508ce0bc74628049ec83a970988f6863cc035d8de2cb008192">>},
                    {<<"search_query">>, undefined},
                    {<<"search_description">>, undefined}
                  ]}]
                }
              ]}
            ]
          }]
       }]},
       {<<"patchsets">>,[]},
       {<<"superseding_change">>, undefined},
       {<<"_links">>,
        {[{<<"delete">>,
           {[{<<"href">>,
              <<"/api/v0/e/Chef/orgs"
                "/Chef_Delivery/projects/delivery"
                "/changes/8725f96f-838c-4add-897e-06a48894382">>}]}},
          {<<"approve">>,
           {[{<<"href">>,
              <<"/api/v0/e/Chef/orgs"
                "/Chef_Delivery/projects/delivery"
                "/changes/8725f96f-838c-4add-897e-06a48894382"
                "/merge">>}]}}]}}]}}.

phase_run_summary_example() ->
    {ok,
     [
      #phase_run_summary{stage_id = 1,stage = <<"verify">>,
                         stage_status = <<"failed">>,phase_id = 3,phase = <<"unit">>,
                         phase_status = <<"passed">>, description = <<"Project A - (name:builder*.shd.chef.co AND platform_family:debian)">>,
                         search_query = <<"(name:builder*.shd.chef.co AND platform_family:debian)">>},
      #phase_run_summary{stage_id = 1,stage = <<"verify">>,
                         stage_status = <<"failed">>,phase_id = 4,phase = <<"unit">>,
                         phase_status = <<"passed">>,
                         description = <<"Project A - (name:builder*.shd.chef.co AND platform_family:rhel)">>,
                         search_query = <<"(name:builder*.shd.chef.co AND platform_family:rhel)">>},
      #phase_run_summary{stage_id = 1,stage = <<"verify">>,
                         stage_status = <<"failed">>,phase_id = 1,phase = <<"lint">>,
                         phase_status = <<"failed">>},
      #phase_run_summary{stage_id = 1,stage = <<"verify">>,
                         stage_status = <<"failed">>,phase_id = 2,
                         phase = <<"syntax">>,phase_status = <<"passed">>}
    ]}.

change_example() ->
    {ok,
     {deliv_change,<<"8725f96f-838c-4add-897e-06a48894382">>,2,
      <<"sf/sunlamp">>,undefined,
      <<"This is a title">>,
      <<"This is a description">>}}.

patchset_sha_test_() ->
    ChangeId = <<"8725f96f-838c-4add-897e-06a48894382">>,
    Patchset = deliv_patchset:setvals([{id, 1},
                                       {change_id, ChangeId},
                                       {sequence_number, 1},
                                       {sha, <<"0f700ce50a95eceb80a30a402111317582603238">>},
                                       {submitter_id, <<"jra">>},
                                       {status, <<"open">>}],
                                      deliv_patchset:'#new'()),

    Exp =  [{deliv_patchset,1,<<"8725f96f-838c-4add-897e-06a48894382">>,1,undefined,
             <<"0f700ce50a95eceb80a30a402111317582603238">>,<<"jra">>,
             undefined,undefined,<<"open">>}],
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [sqerl_rec, deliv_db],
             meck:new(Mods, [passthrough]),
             meck:expect(sqerl_rec, qfetch,
                         [deliv_patchset, patchsets_for_change, [ChangeId]],
                         [Patchset]),
             Mods
     end,
     fun meck_unload/1,
     [
      fun() ->
              Ans = deliv_patchset:patchsets_for_change(ChangeId),
              ?assertEqual(Exp, Ans)
      end
     ]}.

meck_app_config() ->
    meck:new(delivery_app),
    meck:expect(delivery_app, get_env,
                fun(hostname) ->
                        "127.0.0.1";
                   (api_proto) ->
                        "http"
                end),
    [].

meck_unload(OtherMods) ->
    ct_meck:unload([delivery_app | OtherMods]).

patchsets_to_ejson_test_() ->
    hoax:fixture(?MODULE, "patchsets_to_ejson").

patchsets_to_ejson_returns_json_representation_of_patchset() ->
    EntName = <<"ent_name">>,
    OrgName = <<"org_name">>,
    ProjName = <<"proj_name">>,
    ChangeId = <<"change_id">>,
    PipeName = <<"pipe_name">>,
    FeatureBranch = <<"feature_branch">>,

    Scope = deliv_scopes:'#new_common'([{ent_name, EntName},
                                        {org_name, OrgName},
                                        {proj_name, ProjName},
                                        {pipe_name, PipeName},
                                        {change_id, ChangeId},
                                        {scm_module, deliv_scm_local}]),
    Patchset = deliv_patchset:setvals([{sequence_number, 1},
                                       {sha, <<"a_git_sha">>},
                                       {submitted_at, {{2015, 06, 10}, {11, 49, 25}}}],
                                      deliv_patchset:'#new'()),
    hoax:mock(deliv_scopes,
              ?expect(from_change_id,
                      ?withArgs([ChangeId]),
                      ?andReturn(Scope))),
    hoax:mock(deliv_patchset,
              ?expect(to_ejson,
                      ?withArgs([Scope, Patchset]),
                      ?andReturn({ok, {}}))),

    {ok, ActualEjson} = deliv_change_common:patchsets_to_ejson({EntName, OrgName, ProjName},
                                                               {ChangeId, PipeName, FeatureBranch},
                                                               [Patchset]),
    ?assertEqual([{}], ActualEjson),
    ?verifyAll.

change_to_ejson_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "change_to_ejson", setup, teardown).

setup() ->
    error_logger:tty(false),
    eu_database:setup(),
    setup_change_subscriptions(),
    eu_data:with_enterprise(<<"deliv_change_common_test_enterprise">>,
       eu_data:with_organization(<<"deliv_change_common_test_organization">>,
        eu_data:with_project(<<"deliv_change_common_test_project">>,
          eu_data:with_pipeline(<<"master">>, fun(Enterprise, Organization, Project, Pipeline) ->
            UserName = <<"deliv_change_common_test_user">>,
            User = eu_data:fetch_or_create_user(Enterprise, UserName),
            [Enterprise, User, Organization, Project, Pipeline]
          end)))).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

setup_change_subscriptions() ->
    application:set_env(delivery, stages_data, [
                                                {verify, do_not_care},
                                                {build, do_not_care},
                                                {acceptance, do_not_care},
                                                {union, do_not_care},
                                                {rehearsal, do_not_care},
                                                {delivered, do_not_care}
                                               ]),
    application:start(gproc).

change_to_ejson_promotion_status_is_caution_on_pipeline_union_failure([Enterprise,
        User, Organization, Project, Pipeline]) ->

    #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName}
        = eu_data:proj_coordinates(Enterprise, Organization, Project),
    UserName = deliv_user:getval(name, User),

    % Create a patchset and corresponding change.
    FeatureBranch = <<"deliv_change_common_test_feature_branch">>,
    Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project,
        Pipeline, FeatureBranch),
    Change = eu_data:change_from_patchset(Patchset),
    ChangeId = deliv_change:getval(id, Change),
    PipeId = deliv_change:getval(pipeline_id, Change),

    hoax:mock(deliv_dependency_failures,
              ?expect(is_blocked,
                      ?withArgs([PipeId]),
                      ?andReturn(true))),
    % Mock what we don't care to test here.
    hoax:mock(deliv_scm_local,
              ?expect(patchset_metadata_ejson,
                      ?withArgs([?any, ?any]),
                      ?andReturn({ok, {[], {[]}, []}}))),

    {ok, ChangeEjson} = deliv_change_common:get_change_as_ejson(EntName, OrgName,
        ProjName, UserName, ChangeId),
    ChangePromotion = ej:get({<<"promotion">>}, ChangeEjson),
    ExpectedChangePromotion = {[
        {<<"status">>, <<"caution">>},
        {<<"reason">>, <<"pipeline_union_failure">>}
    ]},
    ?assertEqual(ExpectedChangePromotion, ChangePromotion).

change_to_ejson_promotion_status_is_disabled_on_change_superseded([Enterprise,
        User, Organization, Project, Pipeline]) ->

    %% adding changes may trigger change_superseded events. since we're
    %% not testing for that here we stub to ignore.
    hoax:mock(deliv_event,
              ?expect(publish,
                      ?withArgs([?any, ?any]))),

    FeatureBranch = <<"deliv_changeset_eunit_feature">>,
    Change1Patchset = eu_data:create_patchset(Enterprise, User, Organization,
        Project, Pipeline, FeatureBranch),
    OldChange = eu_data:change_from_patchset(Change1Patchset),
    %% Fake an approve & merge
    OldChangeMerged = deliv_change:setvals([{merge_sha, <<"abc123">>}], OldChange),
    deliv_change:update(OldChangeMerged),
    deliv_changeset:add_to_changeset(OldChange, []),

    Change2Patchset = eu_data:create_patchset(Enterprise, User, Organization,
        Project, Pipeline, FeatureBranch),
    SupersedingChange = eu_data:change_from_patchset(Change2Patchset),
    deliv_changeset:add_to_changeset(SupersedingChange, []),

    #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName}
        = eu_data:proj_coordinates(Enterprise, Organization, Project),
    UserName = deliv_user:getval(name, User),

    % Mock what we don't care to test here.
    hoax:mock(deliv_scm_local,
              ?expect(patchset_metadata_ejson,
                      ?withArgs([?any, ?any]),
                      ?andReturn({ok, {[], {[]}, []}}))),

    OldChangeId = deliv_change:getval(id, OldChange),
    {ok, OldChangeEjson} = deliv_change_common:get_change_as_ejson(EntName, OrgName,
        ProjName, UserName, OldChangeId),
    ChangePromotion = ej:get({<<"promotion">>}, OldChangeEjson),
    ExpectedChangePromotion = {[
        {<<"status">>, <<"disabled">>},
        {<<"reason">>, <<"change_superseded">>}
    ]},
    ?assertEqual(ExpectedChangePromotion, ChangePromotion).

change_to_ejson_promotion_status_is_disabled_on_change_delivered([Enterprise,
        User, Organization, Project, Pipeline]) ->

    %% adding changes may trigger change_superseded events. since we're
    %% not testing for that here we stub to ignore.
    hoax:mock(deliv_event,
              ?expect(publish,
                      ?withArgs([?any, ?any]))),

    FeatureBranch = <<"deliv_changeset_eunit_feature">>,
    ChangePatchset = eu_data:create_patchset(Enterprise, User, Organization,
        Project, Pipeline, FeatureBranch),
    Change = eu_data:change_with_closed_changeset_from_patchset(ChangePatchset, User, []),

    #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName}
        = eu_data:proj_coordinates(Enterprise, Organization, Project),
    UserName = deliv_user:getval(name, User),

    % Mock what we don't care to test here.
    hoax:mock(deliv_scm_local,
              ?expect(patchset_metadata_ejson,
                      ?withArgs([?any, ?any]),
                      ?andReturn({ok, {[], {[]}, []}}))),

    ChangeId = deliv_change:getval(id, Change),
    {ok, ChangeEjson} = deliv_change_common:get_change_as_ejson(EntName, OrgName,
        ProjName, UserName, ChangeId),
    ChangePromotion = ej:get({<<"promotion">>}, ChangeEjson),
    ExpectedChangePromotion = {[
        {<<"status">>, <<"disabled">>},
        {<<"reason">>, <<"change_delivered">>}
    ]},
    ?assertEqual(ExpectedChangePromotion, ChangePromotion).

promotion_status_ejson_fixture_test_() ->
    hoax:fixture(?MODULE, "promotion_status_ejson").

promotion_status_ejson_returns_status_delivered_when_delivered() ->
    Expected = {[
        {<<"status">>, <<"disabled">>},
        {<<"reason">>, <<"change_delivered">>}
    ]},
    Result = deliv_change_common:promotion_status_ejson(delivered),
    ?assertEqual(Expected, Result).

promotion_status_ejson_returns_status_superseded_when_superseded() ->
    Expected = {[
        {<<"status">>, <<"disabled">>},
        {<<"reason">>, <<"change_superseded">>}
    ]},
    Result = deliv_change_common:promotion_status_ejson(superseded),
    ?assertEqual(Expected, Result).

promotion_status_ejson_returns_status_blocked_when_blocked() ->
    Expected = {[
        {<<"status">>, <<"caution">>},
        {<<"reason">>, <<"pipeline_union_failure">>}
    ]},
    Result = deliv_change_common:promotion_status_ejson(blocked),
    ?assertEqual(Expected, Result).

promotion_status_ejson_returns_status_proceed_when_open() ->
    Expected = {[
        {<<"status">>, <<"proceed">>}
    ]},
    Result = deliv_change_common:promotion_status_ejson(open),
    ?assertEqual(Expected, Result).

promotion_status_ejson_returns_status_proceed_when_approved() ->
    Expected = {[
        {<<"status">>, <<"proceed">>}
    ]},
    Result = deliv_change_common:promotion_status_ejson(approved),
    ?assertEqual(Expected, Result).
