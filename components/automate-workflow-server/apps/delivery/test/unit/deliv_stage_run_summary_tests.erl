-module(deliv_stage_run_summary_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

%% TODO: converting these tests from meck to hoax is left as an exercise
%% for the reader.

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

hal_from_stage_run_summary_test_() ->
    {setup,
     fun() ->
             meck_app_config(),
             Mods = [deliv_user],
             meck:new(Mods),
             meck:expect(deliv_user, effective_roles,
                         fun(pipeline, [_, <<"admin">>, _, _, _]) -> {ok, [<<"admin">>]};
                            (pipeline, [_, <<"shipper">>, _, _, _]) -> {ok, [<<"shipper">>]};
                            (pipeline, [_, <<"reviewer">>, _, _, _]) -> {ok, [<<"reviewer">>]};
                            (pipeline, [_, <<"committer">>, _, _, _]) -> {ok, [<<"committer">>]};
                            (pipeline, [_, <<"observer">>, _, _, _]) -> {ok, [<<"observer">>]};
                            (pipeline, [_, <<"none">>, _, _, _]) -> {ok, []};
                            (enterprise, [_, <<"admin">>]) -> {ok, [<<"admin">>]};
                            (enterprise, [_, <<"shipper">>]) -> {ok, [<<"shipper">>]};
                            (enterprise, [_, <<"reviewer">>]) -> {ok, [<<"reviewer">>]};
                            (enterprise, [_, <<"committer">>]) -> {ok, [<<"committer">>]};
                            (enterprise, [_, <<"observer">>]) -> {ok, [<<"observer">>]};
                            (enterprise, [_, <<"none">>]) -> {ok, []}
                         end),
             Mods
     end,
     fun meck_unload/1,
     fun(_) ->
             [
              empty_stage_run_hal_tests(),
              verify_stage_run_hal_tests(),
              build_stage_run_hal_tests(),
              acceptance_stage_run_hal_tests(),
              union_stage_run_hal_tests(),
              rehearsal_stage_run_hal_tests(),
              delivered_stage_run_hale_tests()
             ]
     end}.

empty_stage_run_hal_tests() ->
    RC = {<<"Chef">>, <<"Chef">>, <<"Test">>},
    CID = <<"8725f96f-838c-4add-897e-06a48894382">>,
    [{"Empty HAL when " ++ chef_utils:to_str(User) ++ " and no stages have run",
      fun() ->
              {ok, SRS} = stage_run_summary_empty(),
              ?assertEqual({[]},
                           deliv_stage_run_summary:to_hal(
                             SRS, <<"open">>, RC, <<"master">>, User, CID))
      end} || User <- all_users()].

verify_stage_run_hal_tests() ->
    RC = {<<"Chef">>, <<"Chef">>, <<"Test">>},
    CID = <<"8725f96f-838c-4add-897e-06a48894382">>,
    TRIGGER_VERIFY_DELETE_HAL =
        {[{<<"trigger-verify">>,
           {[{<<"href">>, <<"/api/v0/e/Chef/orgs/Chef/projects/Test",
                            "/changes/8725f96f-838c-4add-897e-06a48894382/trigger/",
                            "verify">>}]}},
          {<<"delete">>,
           {[{<<"href">>, <<"/api/v0/e/Chef/orgs/Chef/projects/Test",
                            "/changes/8725f96f-838c-4add-897e-06a48894382">>}]}}]},
    DELETE_APPROVE_HAL =
        {[{<<"delete">>,
           {[{<<"href">>, <<"/api/v0/e/Chef/orgs/Chef/projects/Test",
                            "/changes/8725f96f-838c-4add-897e-06a48894382">>}]}},
          {<<"approve">>,
           {[{<<"href">>, <<"/api/v0/e/Chef/orgs/Chef/projects/Test",
                            "/changes/8725f96f-838c-4add-897e-06a48894382",
                            "/merge">>}]}}]},
    DELETE_HAL =
        {[{<<"delete">>,
           {[{<<"href">>, <<"/api/v0/e/Chef/orgs/Chef/projects/Test",
                            "/changes/8725f96f-838c-4add-897e-06a48894382">>}]}}]},
    [{"Empty HAL when verify is incomplete",
      fun() ->
              {ok, SRS} = stage_run_summary_verify_incomplete(),
              ?assertEqual({[]},
                           deliv_stage_run_summary:to_hal(
                             SRS, <<"open">>, RC, <<"master">>, <<"admin">>, CID))
      end},
     [{"Empty HAL when verify is failed and user is "
       ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_verify_failed(),
               ?assertEqual({[]},
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"open">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"shipper">>, <<"observer">>]],
     [{"Empty HAL when verify is successful and user is "
       ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_verify_successful(),
               ?assertEqual({[]},
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"open">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"shipper">>, <<"observer">>, <<"none">>]],
     [{"Trigger verify and delete when verify is failed and user is "
       ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_verify_failed(),
               HSRS = deliv_stage_run_summary:to_hal(
                        SRS, <<"open">>, RC, <<"master">>, User, CID),
               ?assertEqual(TRIGGER_VERIFY_DELETE_HAL,
                            HSRS)
       end} || User <- [<<"admin">>, <<"reviewer">>, <<"committer">>]],
     [{"Delete and approve when verify is successful and user is "
       ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_verify_successful(),
               ?assertEqual(DELETE_APPROVE_HAL,
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"open">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"admin">>, <<"reviewer">>]],
     [{"Delete when verify is successful and user is "
       ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_verify_successful(),
               ?assertEqual(DELETE_HAL,
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"open">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"committer">>]]].

build_stage_run_hal_tests() ->
    RC = {<<"Chef">>, <<"Chef">>, <<"Test">>},
    CID = <<"8725f96f-838c-4add-897e-06a48894382">>,
    TRIGGER_BUILD_HAL = trigger_stage_hal(<<"build">>),
    [[{"Empty HAL when build stage run not latest for pipeline and user is "
       ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_build_not_latest(),
               ?assertEqual({[]},
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"shipper">>, <<"observer">>, <<"committer">>]],
     [{"Empty HAL when build stage run is latest for pipeline, failed, and user"
       ++ " is " ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_build_failed(),
               ?assertEqual({[]},
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"shipper">>, <<"observer">>, <<"committer">>]],
     [{"Empty HAL when build stage run is latest for pipeline, successful, and "
       ++ "user is " ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_build_failed(),
               ?assertEqual({[]},
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"shipper">>, <<"committer">>, <<"observer">>, <<"none">>]],
     [{"Trigger build HAL when build stage run is latest for pipeline, "
       ++ "failed, and user is " ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_build_failed(),
               ?assertEqual(TRIGGER_BUILD_HAL,
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"admin">>, <<"reviewer">>]]
    ].

acceptance_stage_run_hal_tests() ->
    RC = {<<"Chef">>, <<"Chef">>, <<"Test">>},
    CID = <<"8725f96f-838c-4add-897e-06a48894382">>,
    TRIGGER_ACCEPTANCE_HAL = trigger_stage_hal(<<"acceptance">>),
    DELIVER_HAL =
        {[{<<"deliver">>,
           {[{<<"href">>,
              <<"/api/v0/e/Chef/orgs/Chef/projects/Test/",
                "changes/8725f96f-838c-4add-897e-06a48894382/accept">>}]}}]},
    [
     [{"Empty HAL when acceptance stage run is not latest for pipeline and user "
       ++ "is " ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_acceptance_not_latest(),
               ?assertEqual({[]},
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- all_users()],
     [{"Empty HAL when acceptance stage run is latest for pipeline, failed, and "
       "user is " ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_acceptance_failed(),
               ?assertEqual({[]},
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"shipper">>, <<"committer">>, <<"observer">>, <<"none">>]],
     [{"Empty HAL when acceptance stage run is latest for pipeline, successful, "
       ++ "and user is " ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_acceptance_successful(),
               ?assertEqual({[]},
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"none">>, <<"reviewer">>, <<"committer">>, <<"observer">>]],
     [{"Trigger acceptance HAL when acceptance stage run is latest for pipeline, "
       ++ "failed, and user is " ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_acceptance_failed(),
               ?assertEqual(TRIGGER_ACCEPTANCE_HAL,
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"admin">>, <<"reviewer">>]],
     [{"Deliver HAL when acceptance stage run is latest for pipeline, successful,"
       ++ " and user is " ++ chef_utils:to_str(User),
       fun() ->
               {ok, SRS} = stage_run_summary_acceptance_successful(),
               ?assertEqual(DELIVER_HAL,
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"admin">>, <<"shipper">>]]].

union_stage_run_hal_tests() ->
    production_triple_tests(<<"union">>).

rehearsal_stage_run_hal_tests() ->
    production_triple_tests(<<"rehearsal">>).

delivered_stage_run_hale_tests() ->
    production_triple_tests(<<"delivered">>).

production_triple_tests(Stage) ->
    RC = {<<"Chef">>, <<"Chef">>, <<"Test">>},
    CID = <<"8725f96f-838c-4add-897e-06a48894382">>,
    TRIGGER_STAGE_HAL = trigger_stage_hal(Stage),
    [
     [{"Empty HAL when " ++ chef_utils:to_str(Stage) ++ " stage run is not "
       "latest for pipeline and user is " ++ chef_utils:to_str(User),
       fun() ->
               SRS_FUNC = <<"stage_run_summary_", Stage/binary, "_not_latest">>,
               {ok, SRS} = erlang:apply(?MODULE, erlang:binary_to_atom(SRS_FUNC, utf8), []),
               ?assertEqual({[]},
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- all_users()],
     [{"Empty HAL when " ++ chef_utils:to_str(Stage) ++ " stage run is latest "
       "for system, successful, and user is " ++ chef_utils:to_str(User),
       fun() ->
               SRS_FUNC = <<"stage_run_summary_", Stage/binary, "_successful">>,
               {ok, SRS} = erlang:apply(?MODULE, erlang:binary_to_atom(SRS_FUNC, utf8), []),
               ?assertEqual({[]},
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- all_users()],
     [{"Empty HAL when " ++ chef_utils:to_str(Stage) ++ " stage run is latest "
       "for system, failed, and user is " ++ chef_utils:to_str(User),
       fun() ->
               SRS_FUNC = <<"stage_run_summary_", Stage/binary, "_failed">>,
               {ok, SRS} = erlang:apply(?MODULE, erlang:binary_to_atom(SRS_FUNC, utf8), []),
               ?assertEqual({[]},
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"reviewer">>, <<"committer">>,
                        <<"observer">>, <<"none">>]],
     [{"Trigger " ++ chef_utils:to_str(Stage) ++ " HAL when "
       ++ chef_utils:to_str(Stage) ++ " stage run is latest for system, failed, "
       ++ "and user is " ++ chef_utils:to_str(User),
       fun() ->
               SRS_FUNC = <<"stage_run_summary_", Stage/binary, "_failed">>,
               {ok, SRS} = erlang:apply(?MODULE, erlang:binary_to_atom(SRS_FUNC, utf8), []),
               ?assertEqual(TRIGGER_STAGE_HAL,
                            deliv_stage_run_summary:to_hal(
                              SRS, <<"merged">>, RC, <<"master">>, User, CID))
       end} || User <- [<<"admin">>, <<"shipper">>]]].

-spec trigger_stage_hal(binary()) -> json().
trigger_stage_hal(Stage) ->
    {[{<<"trigger-", Stage/binary>>,
       {[{<<"href">>, <<"/api/v0/e/Chef/orgs/Chef/projects/Test",
                        "/changes/8725f96f-838c-4add-897e-06a48894382/trigger/",
                        Stage/binary>>}]}}]}.

all_users() ->
    [<<"admin">>, <<"shipper">>, <<"reviewer">>, <<"committer">>, <<"observer">>,
     <<"none">>].

stage_run_summary_empty() -> {ok, []}.

stage_run_summary_verify_incomplete() ->
    {ok,
     [#stage_run_summary{id = 1, stage = <<"verify">>, status = <<"running">>,
                         finished = false, pipeline_latest = true, system_latest = true}
     ]}.

stage_run_summary_verify_success() ->
    {ok,
     [#stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.

stage_run_summary_verify_failed() ->
    {ok,
     [#stage_run_summary{id = 1, stage = <<"verify">>, status = <<"failed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.

stage_run_summary_verify_successful() ->
    {ok,
     [#stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.

stage_run_summary_build_failed() ->
    {ok,
     [#stage_run_summary{id = 2, stage = <<"build">>, status = <<"failed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.

stage_run_summary_build_successful() ->
    {ok,
     [#stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.

stage_run_summary_build_not_latest() ->
    {ok,
     [#stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false}
     ]}.

stage_run_summary_acceptance_failed() ->
    {ok,
     [#stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"failed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.

stage_run_summary_acceptance_successful() ->
    {ok,
     [#stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.

stage_run_summary_acceptance_not_latest() ->
    {ok,
     [#stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false}
     ]}.
stage_run_summary_union_failed() ->
    {ok,
     [#stage_run_summary{id = 4, stage = <<"union">>, status = <<"failed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.
stage_run_summary_union_successful() ->
    {ok,
     [#stage_run_summary{id = 4, stage = <<"union">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.
stage_run_summary_union_not_latest() ->
    {ok,
     [#stage_run_summary{id = 4, stage = <<"union">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false}
     ]}.
stage_run_summary_rehearsal_failed() ->
    {ok,
     [#stage_run_summary{id = 5, stage = <<"rehearsal">>, status = <<"failed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 4, stage = <<"union">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.
stage_run_summary_rehearsal_successful() ->
    {ok,
     [#stage_run_summary{id = 5, stage = <<"rehearsal">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 4, stage = <<"union">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.
stage_run_summary_rehearsal_not_latest() ->
    {ok,
     [#stage_run_summary{id = 5, stage = <<"rehearsal">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 4, stage = <<"union">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false}
     ]}.
stage_run_summary_delivered_failed() ->
    {ok,
     [#stage_run_summary{id = 6, stage = <<"delivered">>, status = <<"failed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 5, stage = <<"rehearsal">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 4, stage = <<"union">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.
stage_run_summary_delivered_successful() ->
    {ok,
     [#stage_run_summary{id = 6, stage = <<"delivered">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 5, stage = <<"rehearsal">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 4, stage = <<"union">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = true, system_latest = true}
     ]}.
stage_run_summary_delivered_not_latest() ->
    {ok,
     [#stage_run_summary{id = 6, stage = <<"delivered">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 5, stage = <<"rehearsal">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 4, stage = <<"union">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 3, stage = <<"acceptance">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 2, stage = <<"build">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false},
      #stage_run_summary{id = 1, stage = <<"verify">>, status = <<"passed">>,
                         finished = true, pipeline_latest = false, system_latest = false}
     ]}.
