-module(deliv_hand_post_receive_hook_tests).

-include_lib("hoax/include/hoax.hrl").
-include("deliv_types.hrl").
-include("deliv_coordinates.hrl").

-compile([export_all]).

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, handle_parse_).

content_types_accepted_accepts_json_test() ->
    ExpectedContentType = {[{{<<"application">>,<<"json">>,'*'}, handle}], request, state},
    ?assertEqual(ExpectedContentType, deliv_hand_post_receive_hook:content_types_accepted(request, state)).

allowed_methods_returns_get_test() ->
    ?assertEqual({[<<"POST">>], request, state}, deliv_hand_post_receive_hook:allowed_methods(request, state)).

handle_parse_returns_403_when_commit_sha_is_the_same() ->
    Sha = <<"02821b73355bc4b5de92a5c8153b2c45f18eb623">>,
    ReferenceName = <<"refs/heads/_for/master/my_new_feature">>,
    EntName = <<"Treebeard">>,
    OrgName = <<"Fangorn">>,
    ProjName = <<"Mirkwood">>,
    ReqEjson =  {[
              {<<"enterprise">>, EntName},
              {<<"organization">>, OrgName},
              {<<"project">>, ProjName},
              {<<"user">>, <<"pivotal">>},
              {<<"reference_name">>, ReferenceName },
              {<<"to_commit">>, Sha}
            ]},
    Msg = <<"Could not create new patchset - empty change">>,
    Jesse = chef_json:simple_string_dict_spec([<<"enterprise">>,
                                               <<"user">>,
                                               <<"organization">>,
                                               <<"project">>,
                                               <<"reference_name">>,
                                               <<"to_commit">>]),
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, Jesse) -> {ReqEjson, req};
                    deliv_web_utils:error_response(403, empty_change, Msg, req, state) -> winning;
                    deliv_pipeline:fetch(EntName, OrgName, ProjName, <<"master">>) -> {ok, pipeline};
                    deliv_project:fetch(EntName, OrgName, ProjName) -> {ok, project};
                    deliv_project:repo_path(project) -> <<"repoPath">>;
                    deliv_git:rev_parse(<<"master">>, "repoPath") -> Sha
                end),

    Response = deliv_hand_post_receive_hook:handle(req, state),
    ?assertEqual(winning, Response),
    ?verifyAll.

handle_parse_returns_403_when_commit_sha_is_git_failure_sha() ->
    Sha = <<"0000000000000000000000000000000000000000">>,
    ReferenceName = <<"refs/heads/_for/master/my_new_feature">>,
    EntName = <<"Treebeard">>,
    OrgName = <<"Fangorn">>,
    ProjName = <<"Mirkwood">>,
    ReqEjson =  {[
              {<<"enterprise">>, EntName},
              {<<"organization">>, OrgName},
              {<<"project">>, ProjName},
              {<<"user">>, <<"pivotal">>},
              {<<"reference_name">>, ReferenceName },
              {<<"to_commit">>, Sha}
            ]},
    Msg = <<"Could not create new patchset - invalid commit sha">>,
    Jesse = chef_json:simple_string_dict_spec([<<"enterprise">>,
                                               <<"user">>,
                                               <<"organization">>,
                                               <<"project">>,
                                               <<"reference_name">>,
                                               <<"to_commit">>]),
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, Jesse) -> {ReqEjson, req};
                    deliv_pipeline:fetch(EntName, OrgName, ProjName, <<"master">>) -> {ok, pipeline};
                    deliv_project:fetch(EntName, OrgName, ProjName) -> {ok, project};
                    deliv_project:repo_path(project) -> <<"repoPath">>;
                    deliv_git:rev_parse(<<"master">>, "repoPath") -> whateversha;
                    deliv_web_utils:error_response(403, invalid_sha, Msg, req, state) -> winning
                end),

    Response = deliv_hand_post_receive_hook:handle(req, state),
    ?assertEqual(winning, Response),
    ?verifyAll.

handle_parse_when_project_config_validation_fails_returns_400_invalid_config_and_removes_target_branch() ->
    Sha = <<"02821b73355bc4b5de92a5c8153b2c45f18eb623">>,
    ReferenceName = <<"refs/heads/_for/master/my_new_feature">>,
    EntName = <<"Treebeard">>,
    OrgName = <<"Fangorn">>,
    ProjName = <<"Mirkwood">>,
    ReqEjson =  {[
              {<<"enterprise">>, EntName},
              {<<"organization">>, OrgName},
              {<<"project">>, ProjName},
              {<<"user">>, <<"pivotal">>},
              {<<"reference_name">>, ReferenceName },
              {<<"to_commit">>, Sha}
            ]},
    Msg = <<"Could not create new patchset - invalid project config">>,
    Jesse = chef_json:simple_string_dict_spec([<<"enterprise">>,
                                               <<"user">>,
                                               <<"organization">>,
                                               <<"project">>,
                                               <<"reference_name">>,
                                               <<"to_commit">>]),
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, Jesse) -> {ReqEjson, req};
                    deliv_pipeline:fetch(EntName, OrgName, ProjName, <<"master">>) -> {ok, pipeline};
                    deliv_project:fetch(EntName, OrgName, ProjName) -> {ok, project};
                    deliv_project:repo_path(project) -> <<"repoPath">>;
                    deliv_git:rev_parse(<<"master">>, "repoPath") -> whateversha;
                    deliv_git:file_at_sha("repoPath", Sha, <<".delivery/config.json">>) -> {ok, config_json};
                    deliv_proj_config:validate(config_json) -> {error, invalid_config};
                    deliv_git:run_git(process_with_pipeline, "repoPath", ["branch", "-D", "_for/master/my_new_feature"]) -> {ok, ignored_output};
                    deliv_web_utils:error_response(400, invalid_config, Msg, req, state) -> winning
                end),

    Response = deliv_hand_post_receive_hook:handle(req, state),
    ?assertEqual(winning, Response),
    ?verifyAll.

handle_parse_when_patchset_creation_fails_because_something_was_not_found_returns_404_with_message() ->
    Sha = <<"02821b73355bc4b5de92a5c8153b2c45f18eb623">>,
    FeatBranchName = <<"my_new_feature">>,
    ReferenceName = <<"refs/heads/_for/master/my_new_feature">>,
    UserName = <<"treefrog">>,
    EntName = <<"Treebeard">>,
    OrgName = <<"Fangorn">>,
    ProjName = <<"Mirkwood">>,
    PipeName = <<"master">>,
    ReqEjson =  {[
              {<<"enterprise">>, EntName},
              {<<"organization">>, OrgName},
              {<<"project">>, ProjName},
              {<<"user">>, UserName},
              {<<"reference_name">>, ReferenceName},
              {<<"to_commit">>, Sha}
            ]},
    Jesse = chef_json:simple_string_dict_spec([<<"enterprise">>,
                                               <<"user">>,
                                               <<"organization">>,
                                               <<"project">>,
                                               <<"reference_name">>,
                                               <<"to_commit">>]),
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, Jesse) -> {ReqEjson, req};
                    deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName) -> {ok, pipeline};
                    deliv_project:fetch(EntName, OrgName, ProjName) -> {ok, project};
                    deliv_project:repo_path(project) -> <<"repoPath">>;
                    deliv_git:rev_parse(PipeName, "repoPath") -> whateversha;
                    deliv_git:file_at_sha("repoPath", Sha, <<".delivery/config.json">>) -> {ok, config_json};
                    deliv_proj_config:validate(config_json) -> {ok, valid_config};
                    deliv_patchset:new(EntName, UserName, OrgName, ProjName, PipeName, FeatBranchName, Sha) -> {error, something};
                    deliv_pg:translate_error(something) -> {not_found, what};
                    deliv_web_utils:error_response(404, <<"what not found">>, req, state) -> winning
                end),

    Response = deliv_hand_post_receive_hook:handle(req, state),
    ?assertEqual(winning, Response),
    ?verifyAll.

handle_parse_when_patchset_creation_fails_for_a_not_anticipated_reason_returns_500() ->
    Sha = <<"02821b73355bc4b5de92a5c8153b2c45f18eb623">>,
    FeatBranchName = <<"my_new_feature">>,
    ReferenceName = <<"refs/heads/_for/master/my_new_feature">>,
    UserName = <<"treefrog">>,
    EntName = <<"Treebeard">>,
    OrgName = <<"Fangorn">>,
    ProjName = <<"Mirkwood">>,
    PipeName = <<"master">>,
    ReqEjson =  {[
              {<<"enterprise">>, EntName},
              {<<"organization">>, OrgName},
              {<<"project">>, ProjName},
              {<<"user">>, UserName},
              {<<"reference_name">>, ReferenceName},
              {<<"to_commit">>, Sha}
            ]},
    Jesse = chef_json:simple_string_dict_spec([<<"enterprise">>,
                                               <<"user">>,
                                               <<"organization">>,
                                               <<"project">>,
                                               <<"reference_name">>,
                                               <<"to_commit">>]),
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, Jesse) -> {ReqEjson, req};
                    deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName) -> {ok, pipeline};
                    deliv_project:fetch(EntName, OrgName, ProjName) -> {ok, project};
                    deliv_project:repo_path(project) -> <<"repoPath">>;
                    deliv_git:rev_parse(PipeName, "repoPath") -> whateversha;
                    deliv_git:file_at_sha("repoPath", Sha, <<".delivery/config.json">>) -> {ok, config_json};
                    deliv_proj_config:validate(config_json) -> {ok, valid_config};
                    deliv_patchset:new(EntName, UserName, OrgName, ProjName, PipeName, FeatBranchName, Sha) -> {error, something};
                    deliv_pg:translate_error(something) -> reason;
                    deliv_web_utils:error_response(500, internal_error, req, state) -> winning
                end),

    Response = deliv_hand_post_receive_hook:handle(req, state),
    ?assertEqual(winning, Response),
    ?verifyAll.

handle_parse_when_patchset_creation_fails_because_patchset_is_not_unique_returns_message_pointing_at_known_patchset() ->
    Sha = <<"02821b73355bc4b5de92a5c8153b2c45f18eb623">>,
    FeatBranchName = <<"my_new_feature">>,
    RawBranchName = "_for/master/my_new_feature",
    ReferenceName = <<"refs/heads/_for/master/my_new_feature">>,
    UserName = <<"treefrog">>,
    EntName = <<"Treebeard">>,
    OrgName = <<"Fangorn">>,
    ProjName = <<"Mirkwood">>,
    PipeName = <<"master">>,
    ReqEjson =  {[
              {<<"enterprise">>, EntName},
              {<<"organization">>, OrgName},
              {<<"project">>, ProjName},
              {<<"user">>, UserName},
              {<<"reference_name">>, ReferenceName},
              {<<"to_commit">>, Sha}
            ]},
    Jesse = chef_json:simple_string_dict_spec([<<"enterprise">>,
                                               <<"user">>,
                                               <<"organization">>,
                                               <<"project">>,
                                               <<"reference_name">>,
                                               <<"to_commit">>]),
    Msg = <<"Patchset already up to date, nothing to do\nhttps://delivery/url/of/change">>,
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, Jesse) -> {ReqEjson, req};
                    deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName) -> {ok, pipeline};
                    deliv_project:fetch(EntName, OrgName, ProjName) -> {ok, project};
                    deliv_project:repo_path(project) -> <<"repoPath">>;
                    deliv_git:rev_parse(PipeName, "repoPath") -> whateversha;
                    deliv_git:file_at_sha("repoPath", Sha, <<".delivery/config.json">>) -> {ok, config_json};
                    deliv_proj_config:validate(config_json) -> {ok, valid_config};
                    deliv_patchset:new(EntName, UserName, OrgName, ProjName, PipeName, FeatBranchName, Sha) -> {error, something};
                    deliv_pg:translate_error(something) -> new_patchset_identical_to_old_one;
                    deliv_patchset:fetch(EntName, OrgName, ProjName, PipeName, FeatBranchName, 1) -> {ok, patchset};
                    deliv_patchset:getval(change_id, patchset) -> change_id;
                    deliv_git:run_git(handle_new_patchset, "repoPath", ["branch", "-D", RawBranchName]) -> {ok, output};
                    deliv_web_utils:make_web_url_for_change(EntName, OrgName, ProjName, change_id) -> <<"https://delivery/url/of/change">>;
                    cowboy_req:set_resp_body(Msg, req) -> req2
                end),

    Response = deliv_hand_post_receive_hook:handle(req, state),
    ?assertEqual({true, req2, state}, Response),
    ?verifyAll.

handle_parse_when_everything_is_fine_creates_a_patchset_and_hands_over_to_scm_module() ->
    Sha = <<"02821b73355bc4b5de92a5c8153b2c45f18eb623">>,
    FeatBranchName = <<"my_new_feature">>,
    RawBranchName = "_for/master/my_new_feature",
    ReferenceName = <<"refs/heads/_for/master/my_new_feature">>,
    UserName = <<"treefrog">>,
    EntName = <<"Treebeard">>,
    OrgName = <<"Fangorn">>,
    ProjName = <<"Mirkwood">>,
    PipeName = <<"master">>,
    ReqEjson =  {[
              {<<"enterprise">>, EntName},
              {<<"organization">>, OrgName},
              {<<"project">>, ProjName},
              {<<"user">>, UserName},
              {<<"reference_name">>, ReferenceName},
              {<<"to_commit">>, Sha}
            ]},
    Coords = #proj_coordinates{
                ent_name = EntName,
                org_name = OrgName,
                proj_name = ProjName},
    Jesse = chef_json:simple_string_dict_spec([<<"enterprise">>,
                                               <<"user">>,
                                               <<"organization">>,
                                               <<"project">>,
                                               <<"reference_name">>,
                                               <<"to_commit">>]),
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, Jesse) -> {ReqEjson, req};
                    deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName) -> {ok, pipeline};
                    deliv_project:fetch(EntName, OrgName, ProjName) -> {ok, project};
                    deliv_project:repo_path(project) -> <<"repoPath">>;
                    deliv_git:rev_parse(PipeName, "repoPath") -> whateversha;
                    deliv_git:file_at_sha("repoPath", Sha, <<".delivery/config.json">>) -> {ok, config_json};
                    deliv_proj_config:validate(config_json) -> {ok, valid_config};
                    deliv_patchset:new(EntName, UserName, OrgName, ProjName, PipeName, FeatBranchName, Sha) -> [patchset];
                    deliv_project:getval(scm_module, project) -> <<"appropriate_scm_module">>;
                    appropriate_scm_module:process_new_patchset(patchset, "repoPath", PipeName, FeatBranchName, RawBranchName, Coords) -> {ok, msg};
                    cowboy_req:set_resp_body(msg, req) -> req2;
                    deliv_event:publish(patchset_created, patchset) -> {ok, event}
                end),

    Response = deliv_hand_post_receive_hook:handle(req, state),
    ?assertEqual({true, req2, state}, Response),
    ?verifyAll.
