-module(scm_hand_projects_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

init_returns_correct_tuple_test() ->
    Actual = scm_hand_projects:init(ignored, req, #handler{}),

    Expected = {upgrade, protocol, cowboy_rest, req, #handler{}},

    ?assertEqual(Expected, Actual).

rest_init_returns_correct_tuple_test() ->
    Actual = scm_hand_projects:rest_init(req, #handler{}),

    Expected = {ok, req, #handler{}},

    ?assertEqual(Expected, Actual).

allowed_methods_allows_only_POST_test() ->
    ?assertEqual({[<<"POST">>], req, #handler{}},
                 scm_hand_projects:allowed_methods(req, #handler{})).

content_types_accepted_accepts_json_test() ->
    hoax:test(fun() ->
                      hoax:mock(deliv_web_utils,
                                ?expect(content_type_json_map,
                                        ?withArgs([from_json]),
                                        ?andReturn(expected_map))),

                      Actual = scm_hand_projects:content_types_accepted(req, #handler{}),

                      ?assertEqual({expected_map, req, #handler{}}, Actual),

                      ?verifyAll
              end).

from_json_fixture_test_() ->
    hoax:fixture(?MODULE, "from_json_").

from_json_returns_error_with_bad_request() ->
    hoax:mock(deliv_web_utils, [
                                ?expect(parse_json_req,
                                        ?withArgs([req, bitbucket_project]),
                                        ?andReturn({{error, reason}, req})),
                                ?expect(error_response,
                                        ?withArgs([400, bad_request, req, #handler{}]),
                                        ?andReturn(error))
                               ]),

    Actual = scm_hand_projects:from_json(req, #handler{}),

    ?assertEqual(error, Actual),
    ?verifyAll.

from_json_handles_conflict_on_new_project() ->
    Ent = <<"enterprise">>,
    Org = <<"organization">>,
    Proj = <<"proj">>,
    Pipe = <<"pipe">>,
    BBProj = <<"bb_proj">>,
    RepoName = <<"repo_name">>,
    SCMType = <<"bitbucket">>,
    Ejson = {[{<<"name">>, Proj},
              {<<"scm">>,
                [{<<"project_key">>, BBProj},
                 {<<"repo_name">>, RepoName},
                 {<<"pipeline_branch">>, Pipe},
                 {<<"type">>, SCMType}]}
            ]},
    State = #handler{ent_name = Ent},

    hoax:mock(deliv_web_utils, [
                                ?expect(parse_json_req,
                                        ?withArgs([req, bitbucket_project]),
                                        ?andReturn({Ejson, req1})),
                                ?expect(error_response,
                                        ?withArgs([409, conflict, "The repository name you entered has already been linked to another project in Chef Automate.", req2, State]),
                                        ?andReturn(error))
                               ]),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([org_name, req1]),
                      ?andReturn({Org, req2}))),
    hoax:mock(deliv_project,
              ?expect(new,
                      ?withArgs([Ent, Org, Proj, Pipe, bitbucket_scm, BBProj, RepoName]),
                      ?andReturn({error, {conflict, "Duplicate"}}))),
    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([Ent, SCMType]),
                      ?andReturn({ok, bitbucket_auth_creds}))),
    hoax:mock(deliv_basic_auth_application,
              ?expect(getval,
                      ?withArgs([name, bitbucket_auth_creds]),
                      ?andReturn(<<"bitbucket">>))),


    Actual = scm_hand_projects:from_json(req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

from_json_handles_other_error_on_new_project() ->
    Ent = <<"enterprise">>,
    Org = <<"organization">>,
    Proj = <<"proj">>,
    Pipe = <<"pipe">>,
    BBProj = <<"bb_proj">>,
    RepoName = <<"repo_name">>,
    SCMType = <<"bitbucket">>,
    Ejson = {[{<<"name">>, Proj},
              {<<"scm">>,
                [{<<"project_key">>, BBProj},
                 {<<"repo_name">>, RepoName},
                 {<<"pipeline_branch">>, Pipe},
                 {<<"type">>, SCMType}]}
            ]},
    State = #handler{ent_name = Ent},

    hoax:mock(deliv_web_utils, [
                                ?expect(parse_json_req,
                                        ?withArgs([req, bitbucket_project]),
                                        ?andReturn({Ejson, req1})),
                                ?expect(error_response,
                                        ?withArgs([500, internal_error, req2, State]),
                                        ?andReturn(error))
                               ]),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([org_name, req1]),
                      ?andReturn({Org, req2}))),
    hoax:mock(deliv_project,
              ?expect(new,
                      ?withArgs([Ent, Org, Proj, Pipe, bitbucket_scm, BBProj, RepoName]),
                      ?andReturn({error, reason}))),
    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([Ent, SCMType]),
                      ?andReturn({ok, bitbucket_auth_creds}))),
    hoax:mock(deliv_basic_auth_application,
              ?expect(getval,
                      ?withArgs([name, bitbucket_auth_creds]),
                      ?andReturn(<<"bitbucket">>))),

    Actual = scm_hand_projects:from_json(req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

from_json_responds_with_412_precondition_failed_when_missing_credentials() ->
    Ent = <<"enterprise">>,
    Proj = <<"proj">>,
    Pipe = <<"pipe">>,
    BBProj = <<"bb_proj">>,
    RepoName = <<"repo_name">>,
    SCMType = <<"bitbucket">>,
    Ejson = {[{<<"name">>, Proj},
              {<<"scm">>,
                [{<<"project_key">>, BBProj},
                 {<<"repo_name">>, RepoName},
                 {<<"pipeline_branch">>, Pipe},
                  {<<"type">>, SCMType}]}
            ]},
    State = #handler{ent_name = Ent},

    hoax:mock(deliv_web_utils, [
                                ?expect(parse_json_req,
                                        ?withArgs([req, bitbucket_project]),
                                        ?andReturn({Ejson, req1})),
                                ?expect(error_response,
                                        ?withArgs([412, missing_bitbucket_configuration, req1, State]),
                                        ?andReturn(error))
                               ]),
    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([Ent, SCMType]),
                      ?andReturn({error, some_error}))),

    Actual = scm_hand_projects:from_json(req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

from_json_creates_new_bb_project_when_bb_type() ->
    Ent = <<"enterprise">>,
    Org = <<"organization">>,
    Proj = <<"proj">>,
    Pipe = <<"pipe">>,
    BBProj = <<"bb_proj">>,
    RepoName = <<"repo_name">>,
    SCMType = <<"bitbucket">>,
    Ejson = {[{<<"name">>, Proj},
              {<<"scm">>,
                [{<<"project_key">>, BBProj},
                 {<<"repo_name">>, RepoName},
                 {<<"pipeline_branch">>, Pipe},
                 {<<"type">>, SCMType}]}
            ]},
    State = #handler{ent_name = Ent},

    hoax:mock(deliv_web_utils, ?expect(parse_json_req,
                                        ?withArgs([req, bitbucket_project]),
                                        ?andReturn({Ejson, req1}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([org_name, req1]),
                      ?andReturn({Org, req2}))),
    hoax:mock(deliv_project,
              ?expect(new,
                      ?withArgs([Ent, Org, Proj, Pipe, bitbucket_scm, BBProj, RepoName]),
                      ?andReturn({ok, Proj}))),
    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([Ent, SCMType]),
                      ?andReturn({ok, bitbucket_auth_creds}))),
    hoax:mock(deliv_basic_auth_application,
              ?expect(getval,
                      ?withArgs([name, bitbucket_auth_creds]),
                      ?andReturn(<<"bitbucket">>))),

    Actual = scm_hand_projects:from_json(req, State),

    ?assertEqual({true, req2, State}, Actual),
    ?verifyAll.

from_json_creates_new_gh_project_when_gh_type() ->
    Ent = <<"enterprise">>,
    Org = <<"organization">>,
    Proj = <<"proj">>,
    Pipe = <<"pipe">>,
    GHOrg = <<"gh_proj">>,
    GHProj = <<"repo_name">>,
    SCMType = <<"github">>,
    Ejson = {[{<<"name">>, Proj},
              {<<"scm">>,
                [{<<"organization">>, GHOrg},
                 {<<"project">>, GHProj},
                 {<<"branch">>, Pipe},
                 {<<"type">>, SCMType}]}
            ]},
    State = #handler{ent_name = Ent},

    hoax:mock(deliv_web_utils, ?expect(parse_json_req,
                                        ?withArgs([req, bitbucket_project]),
                                        ?andReturn({Ejson, req1}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([org_name, req1]),
                      ?andReturn({Org, req2}))),
    hoax:mock(deliv_project,
              ?expect(new,
                      ?withArgs([Ent, Org, Proj, Pipe, github_scm, GHOrg, GHProj]),
                      ?andReturn({ok, Proj}))),
    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([Ent, SCMType]),
                      ?andReturn({ok, github_auth_creds}))),
    hoax:mock(deliv_basic_auth_application,
              ?expect(getval,
                      ?withArgs([name, github_auth_creds]),
                      ?andReturn(<<"github">>))),

    Actual = scm_hand_projects:from_json(req, State),

    ?assertEqual({true, req2, State}, Actual),
    ?verifyAll.
