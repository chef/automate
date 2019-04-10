-module(deliv_job_command_tests).

-include_lib("hoax/include/hoax.hrl").

-include("../../src/deliv_phase.hrl").
-include("../../src/deliverance_types.hrl").


-compile([export_all]).
-define(FIPS_GIT_PORT, <<"11111">>).

deliv_job_command_test_() ->
    [
     hoax:parameterized_fixture(?MODULE, "command_when_a2mode_enabled", setup_with_a2mode, teardown),
     hoax:parameterized_fixture(?MODULE, "command_when_fips_enabled", setup_with_fips, teardown),
     hoax:parameterized_fixture(?MODULE, "command_when_fips_disabled", setup, teardown),
     hoax:parameterized_fixture(?MODULE, "command_when_change_is_not_merged", setup_with_no_merge, teardown),
     eunit_sugar:fixture(?MODULE, generate_id_)
    ].

setup() ->
    setup(<<"false">>).

setup(Fips) ->
    setup(Fips, <<"false">>, merge_sha, null).

setup(Fips, A2Mode) ->
    setup(Fips, A2Mode, merge_sha, null).

setup(Fips, MergeSha, PatchsetBranch) ->
    setup(Fips, <<"false">>, MergeSha, PatchsetBranch).

setup(Fips, A2Mode, MergeSha, PatchsetBranch) ->
    application:set_env(delivery, deliv_fips_stunnel_proxy_port, ?FIPS_GIT_PORT),
    StageRun = stage_run,
    PhaseRun = phase_run,
    Change = change,
    Patchset = patchset,

    ChangeId = change_id,
    EntName = ent_name,
    OrgName = org_name,
    ProjName = proj_name,
    PipeName = pipe_name,
    Scope = deliv_scopes:'#new_common'([]),

    EJson = {[{<<"enterprise">>, EntName},
              {<<"organization">>, OrgName},
              {<<"project">>, ProjName},
              {<<"pipeline">>, PipeName},
              {<<"change_id">>, ChangeId},
              {<<"patchset_number">>, sequence_number},
              {<<"stage">>, stage},
              {<<"stage_run_id">>, stage_run_id},
              {<<"phase">>, phase},
              {<<"phase_run_id">>, phase_run_id},
              {<<"git_url">>, git_url},
              {<<"sha">>, MergeSha},
              {<<"fips">>, Fips},
              {<<"fips_git_port">>, ?FIPS_GIT_PORT},
              {<<"a2_mode">>, A2Mode},
              {<<"patchset_branch">>, PatchsetBranch},
              {<<"delivery_api_url">>, api_url},
              {<<"delivery_data_url">>, api_url},
              {<<"delivery_change_url">>, change_url},
              {<<"log_level">>, <<"info">>},
              {<<"token">>, builder_token},
              {<<"dispatch_version">>, dispatch_version},
              {<<"timeout">>, timeout}]},

    [EJson, StageRun, PhaseRun, Change, Patchset,
     ChangeId, EntName, OrgName, ProjName, PipeName, Scope].

setup_with_a2mode() ->
    application:set_env(delivery, a2_mode, true),
    setup(<<"false">>, <<"true">>).

setup_with_fips() ->
    application:set_env(delivery, deliv_fips_mode, true),
    setup(<<"true">>).

setup_with_no_merge() ->
    setup(<<"false">>, <<"false">>, non_merge_sha, patchset_branch).

teardown([_, _, _, _, _, _, _, _, _, _, _]) ->
    application:unset_env(delivery, a2_mode),
    application:unset_env(delivery, deliv_fips_mode),
    application:unset_env(delivery, deliv_fips_stunnel_proxy_port).

command_when_a2mode_disabled_and_change_is_merged_collects_all_the_things_and_base64_encodes_it_to_json([EJson, StageRun, PhaseRun, Change, Patchset,
                                                                                                         ChangeId, EntName, OrgName, ProjName, PipeName, Scope]) ->
    hoax:expect(receive
                    deliv_scopes:from_change(Change) -> Scope;
                    deliv_scopes:'#get'(scm_module, Scope) -> deliv_scm_module;
                    deliv_scm_module:clone_url(<<"builder">>, Patchset, Scope, false) -> git_url;
                    deliv_change:getval(id, Change) -> change_id;
                    deliv_change:scoping_names(change_id) -> [EntName, OrgName, ProjName, PipeName];
                    deliv_web_utils:make_web_url_for_change(EntName, OrgName, ProjName, ChangeId) -> change_url;
                    deliv_token:assign_token(EntName, ?BUILDER_NAME) -> {ok, builder_token};
                    deliv_web_utils:make_api_url_prefix() -> api_url;
                    deliv_change:is_merged(Change) -> true;
                    deliv_change:getval(merge_sha, Change) -> merge_sha;
                    deliv_patchset:getval(sequence_number, Patchset) -> sequence_number;
                    deliv_stage_run:getval(stage, StageRun) -> stage;
                    deliv_stage_run:getval(id, StageRun) -> stage_run_id;
                    deliv_phase_run:getval(phase, PhaseRun) -> phase;
                    deliv_phase_run:getval(id, PhaseRun) -> phase_run_id;
                    chef_json:encode(EJson) -> json_binary;
                    base64:encode(json_binary) -> <<"json-base64">>
                end),

    Actual = deliv_job_command:command(StageRun, PhaseRun, Change, Patchset, dispatch_version, timeout),
    ?assertEqual(<<"delivery-cmd json-base64">>, Actual),
    ?verifyAll.

command_when_fips_disabled_and_change_is_merged_collects_all_the_things_and_base64_encodes_it_to_json([EJson, StageRun, PhaseRun, Change, Patchset,
                                                                                                       ChangeId, EntName, OrgName, ProjName, PipeName, Scope]) ->
    hoax:expect(receive
                    deliv_scopes:from_change(Change) -> Scope;
                    deliv_scopes:'#get'(scm_module, Scope) -> deliv_scm_module;
                    deliv_scm_module:clone_url(<<"builder">>, Patchset, Scope, false) -> git_url;
                    deliv_change:getval(id, Change) -> change_id;
                    deliv_change:scoping_names(change_id) -> [EntName, OrgName, ProjName, PipeName];
                    deliv_web_utils:make_web_url_for_change(EntName, OrgName, ProjName, ChangeId) -> change_url;
                    deliv_token:assign_token(EntName, ?BUILDER_NAME) -> {ok, builder_token};
                    deliv_web_utils:make_api_url_prefix() -> api_url;
                    deliv_change:is_merged(Change) -> true;
                    deliv_change:getval(merge_sha, Change) -> merge_sha;
                    deliv_patchset:getval(sequence_number, Patchset) -> sequence_number;
                    deliv_stage_run:getval(stage, StageRun) -> stage;
                    deliv_stage_run:getval(id, StageRun) -> stage_run_id;
                    deliv_phase_run:getval(phase, PhaseRun) -> phase;
                    deliv_phase_run:getval(id, PhaseRun) -> phase_run_id;
                    chef_json:encode(EJson) -> json_binary;
                    base64:encode(json_binary) -> <<"json-base64">>
                end),

    Actual = deliv_job_command:command(StageRun, PhaseRun, Change, Patchset, dispatch_version, timeout),
    ?assertEqual(<<"delivery-cmd json-base64">>, Actual),
    ?verifyAll.

command_when_fips_enabled_and_change_is_merged_collects_all_the_things_and_base64_encodes_it_to_json([EJson, StageRun, PhaseRun, Change, Patchset,
                                                                                                       ChangeId, EntName, OrgName, ProjName, PipeName, Scope]) ->
    hoax:expect(receive
                    deliv_scopes:from_change(Change) -> Scope;
                    deliv_scopes:'#get'(scm_module, Scope) -> deliv_scm_module;
                    deliv_scm_module:clone_url(<<"builder">>, Patchset, Scope, true) -> git_url;
                    deliv_change:getval(id, Change) -> change_id;
                    deliv_change:scoping_names(change_id) -> [EntName, OrgName, ProjName, PipeName];
                    deliv_web_utils:make_web_url_for_change(EntName, OrgName, ProjName, ChangeId) -> change_url;
                    deliv_token:assign_token(EntName, ?BUILDER_NAME) -> {ok, builder_token};
                    deliv_web_utils:make_api_url_prefix() -> api_url;
                    deliv_change:is_merged(Change) -> true;
                    deliv_change:getval(merge_sha, Change) -> merge_sha;
                    deliv_patchset:getval(sequence_number, Patchset) -> sequence_number;
                    deliv_stage_run:getval(stage, StageRun) -> stage;
                    deliv_stage_run:getval(id, StageRun) -> stage_run_id;
                    deliv_phase_run:getval(phase, PhaseRun) -> phase;
                    deliv_phase_run:getval(id, PhaseRun) -> phase_run_id;
                    chef_json:encode(EJson) -> json_binary;
                    base64:encode(json_binary) -> <<"json-base64">>
                end),

    Actual = deliv_job_command:command(StageRun,
                                       PhaseRun,
                                       Change,
                                       Patchset,
                                       dispatch_version,
                                       timeout),
    ?assertEqual(<<"delivery-cmd json-base64">>, Actual),
    ?verifyAll.

command_when_change_is_not_merged_collects_all_the_things_and_base64_encodes_it_to_json([EJson, StageRun, PhaseRun, Change, Patchset,
                                                                                         ChangeId, EntName, OrgName, ProjName, PipeName, Scope]) ->
    StageRun = stage_run,
    PhaseRun = phase_run,
    Change = change,
    Patchset = patchset,

    ChangeId = change_id,
    EntName = ent_name,
    OrgName = org_name,
    ProjName = proj_name,
    PipeName = pipe_name,

    hoax:expect(receive
                    deliv_scopes:from_change(Change) -> Scope;
                    deliv_scopes:'#get'(scm_module, Scope) -> deliv_scm_module;
                    deliv_scm_module:clone_url(<<"builder">>, Patchset, Scope, false) -> git_url;
                    deliv_change:getval(id, Change) -> change_id;
                    deliv_change:scoping_names(change_id) -> [EntName, OrgName, ProjName, PipeName];
                    deliv_web_utils:make_web_url_for_change(EntName, OrgName, ProjName, ChangeId) -> change_url;
                    deliv_token:assign_token(EntName, ?BUILDER_NAME) -> {ok, builder_token};
                    deliv_web_utils:make_api_url_prefix() -> api_url;
                    deliv_change:is_merged(Change) -> false;
                    deliv_scm_module:patchset_branch(Change, Patchset, Scope) -> patchset_branch;
                    deliv_patchset:getval(sha, Patchset) -> non_merge_sha;
                    deliv_patchset:getval(sequence_number, Patchset) -> sequence_number;
                    deliv_stage_run:getval(stage, StageRun) -> stage;
                    deliv_stage_run:getval(id, StageRun) -> stage_run_id;
                    deliv_phase_run:getval(phase, PhaseRun) -> phase;
                    deliv_phase_run:getval(id, PhaseRun) -> phase_run_id;
                    chef_json:encode(EJson) -> json_binary;
                    base64:encode(json_binary) -> <<"json-base64">>
                end),

    Actual = deliv_job_command:command(StageRun,
                                       PhaseRun,
                                       Change,
                                       Patchset,
                                       dispatch_version,
                                       timeout),
    ?assertEqual(<<"delivery-cmd json-base64">>, Actual),
    ?verifyAll.

generate_id_returns_correct_binary_hash() ->
    StageId = 12,
    PhaseId = 13,
    ChangeId = <<"changeid">>,
    Expected = <<"e1b3ca31d11eedb82e189dc0b3dc30341d54e339499cffaf21814204b0d93b65">>,

    Actual = deliv_job_command:generate_id(StageId, PhaseId, ChangeId),
    ?assertEqual(Expected, Actual).
