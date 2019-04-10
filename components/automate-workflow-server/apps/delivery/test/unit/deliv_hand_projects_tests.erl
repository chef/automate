-module(deliv_hand_projects_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

content_types_provided_accepts_json_test() ->
    ExpectedContentType = {[{{<<"application">>,<<"json">>,'*'},to_json}, {<<"*/*">>, to_json}], request, state},
    ?assertEqual(ExpectedContentType, deliv_hand_projects:content_types_provided(request, state)).

allowed_methods_returns_get_test() ->
    ?assertEqual({[<<"GET">>, <<"POST">>], request, state}, deliv_hand_projects:allowed_methods(request, state)).

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, to_json).

fetch_scm_url_fixture_test_() ->
    hoax:fixture(?MODULE, fetch_scm_url).

to_json_returns_list_of_single_project_when_fetched_project_is_local() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    OrgId = 1,
    ProjName = <<"projname">>,
    ScmModule = <<"deliv_scm_local">>,
    ScmType = <<"local">>,
    UserName = <<"username">>,
    PipeName = <<"pipelinename">>,
    State = #handler{ent_name = EntName, user_name = UserName},
    Project = deliv_project:fromlist([{name, ProjName}, {scm_module, ScmModule}]),

    ProjectJson = {[
        {<<"name">>, ProjName},
        {<<"ent_name">>, EntName},
        {<<"org_name">>, OrgName},
        {<<"scm">>, {[
            {<<"type">>, ScmType}
        ]}},
        {<<"git_url">>, <<"url">>},
        {<<"_links">>, {[
            {<<"full">>, {[
                {<<"href">>, <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary>>}
            ]}},
            {<<"list_pipelines">>, {[
                {<<"href">>, <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary, "/pipelines">>}
            ]}},
            {pipelines, [{[
                {href, <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary, "/pipelines/", PipeName/binary>>},
                {name, PipeName}
            ]}]}
        ]}}
    ]},

    hoax:mock(cowboy_req, [
        ?expect(binding,
            ?withArgs([org_name, request]),
            ?andReturn({OrgName, request2})),
        ?expect(set_resp_header,
            ?withArgs([<<"content-type">>,<<"application/json">>,request2]),
            ?andReturn({OrgName, request2}))
    ]),
    hoax:mock(deliv_organization, [
        ?expect(fetch,
            ?withArgs([EntName, OrgName]),
            ?andReturn({ok, org})),
        ?expect(getval,
            ?withArgs([id, org]),
            ?andReturn(OrgId))
    ]),
    hoax:mock(deliv_db, [
        ?expect(qfetch,
            ?withArgs([deliv_project, projects, [OrgId]]),
            ?andReturn([Project])),
        ?expect(fetch_names,
            ?withArgs([deliv_pipeline, [EntName, OrgName, ProjName]]),
            ?andReturn([PipeName]))
    ]),
    hoax:mock(deliv_project_json,
        ?expect(create_body,
            ?withArgs([EntName, OrgName, ProjName, ScmType, UserName, [PipeName]]),
            ?andReturn(ProjectJson))
    ),

    ExpectedJson = [ProjectJson],
    ExpectedResponse = deliv_web_utils:content(ExpectedJson, request2, State),

    ?assertEqual(ExpectedResponse, deliv_hand_projects:to_json(request, State)),
    ?verifyAll.

to_json_returns_list_of_single_project_when_fetched_project_is_bitbucket() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    OrgId = 1,
    ProjName = <<"projname">>,
    ProjId = 1,
    ScmModule = <<"bitbucket_scm">>,
    ScmType = <<"bitbucket">>,
    UserName = <<"username">>,
    PipeName = <<"pipelinename">>,
    BBProj = <<"bb_proj">>,
    State = #handler{ent_name = EntName, user_name = UserName},
    Project = deliv_project:fromlist([{id, ProjId}, {name, ProjName}, {scm_module, ScmModule}]),
    ScmUrl = <<"https://bitbucket.com">>,


    BitbucketMetadata = scm_bitbucket_project_metadata:fromlist([{<<"bitbucket_project">>, BBProj},
                                                                 {<<"repo_name">>, ProjName}]),
    ProjectJson = {[
        {<<"name">>, ProjName},
        {<<"ent_name">>, EntName},
        {<<"org_name">>, OrgName},
        {<<"scm">>, {[
            {<<"type">>, ScmType},
            {<<"project_key">>, BBProj},
            {<<"repo_name">>, ProjName}
        ]}},
        {<<"git_url">>, <<"url">>},
        {<<"_links">>, {[
            {<<"full">>, {[
                {<<"href">>, <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary>>}
            ]}},
            {<<"list_pipelines">>, {[
                {<<"href">>, <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary, "/pipelines">>}
            ]}},
            {pipelines, [{[
                {href, <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary, "/pipelines/", PipeName/binary>>},
                {name, PipeName}
            ]}]}
        ]}}
    ]},

    hoax:mock(cowboy_req, [
        ?expect(binding,
            ?withArgs([org_name, request]),
            ?andReturn({OrgName, request2})),
        ?expect(set_resp_header,
            ?withArgs([<<"content-type">>,<<"application/json">>,request2]),
            ?andReturn({OrgName, request2}))
    ]),
    hoax:mock(deliv_organization, [
        ?expect(fetch,
            ?withArgs([EntName, OrgName]),
            ?andReturn({ok, org})),
        ?expect(getval,
            ?withArgs([id, org]),
            ?andReturn(OrgId))
    ]),
    hoax:mock(deliv_db, [
        ?expect(qfetch,
            ?withArgs([deliv_project, projects, [OrgId]]),
            ?andReturn([Project])),
        ?expect(fetch_names,
            ?withArgs([deliv_pipeline, [EntName, OrgName, ProjName]]),
            ?andReturn([PipeName])),
        ?expect(fetch_by_id,
            ?withArgs([scm_bitbucket_project_metadata, ProjId]),
            ?andReturn({ok, BitbucketMetadata}))
    ]),
    hoax:mock(deliv_project_json,
        ?expect(create_body,
            ?withArgs([EntName, OrgName, ProjName, ScmType, BitbucketMetadata, UserName, [PipeName], ScmUrl]),
            ?andReturn(ProjectJson))
    ),
    hoax:mock(scm_basic_auth,
        ?expect(load_basic_auth_credentials,
            ?withArgs([EntName, ScmType]),
            ?andReturn({ok, scm_config}))
    ),
    hoax:mock(deliv_basic_auth_application,
        ?expect(getval,
            ?withArgs([root_api_url, scm_config]),
            ?andReturn(ScmUrl))
    ),


    ExpectedJson = [ProjectJson],
    ExpectedResponse = deliv_web_utils:content(ExpectedJson, request2, State),

    ?assertEqual(ExpectedResponse, deliv_hand_projects:to_json(request, State)),
    ?verifyAll.

to_json_returns_list_of_single_project_when_fetched_project_is_github() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    OrgId = 1,
    ProjName = <<"projname">>,
    ProjId = 1,
    ScmModule = <<"deliv_scm_github">>,
    ScmType = <<"github">>,
    ScmUrl = <<"https://github.com">>,
    UserName = <<"username">>,
    PipeName = <<"pipelinename">>,
    GHOwner = <<"bb_proj">>,
    State = #handler{ent_name = EntName, user_name = UserName},
    Project = deliv_project:fromlist([{id, ProjId}, {name, ProjName}, {scm_module, ScmModule}]),

    GithubMetatdata = deliv_project_github_metadata:fromlist([{<<"repo_owner">>, GHOwner},
                                                              {<<"repo_name">>, ProjName}]),
    ProjectJson = {[
        {<<"name">>, ProjName},
        {<<"ent_name">>, EntName},
        {<<"org_name">>, OrgName},
        {<<"scm">>, {[
            {<<"type">>, ScmType},
            {<<"repo_owner">>, GHOwner},
            {<<"repo_name">>, ProjName}
        ]}},
        {<<"_links">>, {[
            {<<"full">>, {[
                {<<"href">>, <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary>>}
            ]}},
            {<<"list_pipelines">>, {[
                {<<"href">>, <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary, "/pipelines">>}
            ]}},
            {pipelines, [{[
                {href, <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary, "/pipelines/", PipeName/binary>>},
                {name, PipeName}
            ]}]}
        ]}}
    ]},

    hoax:mock(cowboy_req, [
        ?expect(binding,
            ?withArgs([org_name, request]),
            ?andReturn({OrgName, request2})),
        ?expect(set_resp_header,
            ?withArgs([<<"content-type">>,<<"application/json">>,request2]),
            ?andReturn({OrgName, request2}))
    ]),
    hoax:mock(deliv_organization, [
        ?expect(fetch,
            ?withArgs([EntName, OrgName]),
            ?andReturn({ok, org})),
        ?expect(getval,
            ?withArgs([id, org]),
            ?andReturn(OrgId))
    ]),
    hoax:mock(deliv_db, [
        ?expect(qfetch,
            ?withArgs([deliv_project, projects, [OrgId]]),
            ?andReturn([Project])),
        ?expect(fetch_names,
            ?withArgs([deliv_pipeline, [EntName, OrgName, ProjName]]),
            ?andReturn([PipeName])),
        ?expect(fetch_by_id,
            ?withArgs([deliv_project_github_metadata, ProjId]),
            ?andReturn({ok, GithubMetatdata}))
    ]),
    hoax:mock(deliv_project_json,
        ?expect(create_body,
            ?withArgs([EntName, OrgName, ProjName, ScmType, GithubMetatdata, UserName, [PipeName], ScmUrl]),
            ?andReturn(ProjectJson))
    ),
    hoax:expect(receive
                    scm_basic_auth:load_basic_auth_credentials(EntName, ScmType) -> {ok, scm_config};
                    deliv_basic_auth_application:getval(root_api_url, scm_config) -> ScmUrl
                end),

    ExpectedJson = [ProjectJson],
    ExpectedResponse = deliv_web_utils:content(ExpectedJson, request2, State),

    ?assertEqual(ExpectedResponse, deliv_hand_projects:to_json(request, State)),
    ?verifyAll.

to_json_returns_halt_when_projects_fetch_fails() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    OrgId = 1,
    UserName = <<"username">>,
    State = #handler{ent_name = EntName, user_name = UserName},

    hoax:mock(cowboy_req, [
        ?expect(binding,
            ?withArgs([org_name, request]),
            ?andReturn({OrgName, request2})),
        ?expect(set_resp_header,
            ?withArgs([<<"content-type">>,<<"application/json">>,request2]),
            ?andReturn({OrgName, request3})),
        ?expect(reply,
            ?withArgs([500, [], ?any, ?any]),
            ?andReturn({ok, request4}))
    ]),
    hoax:mock(deliv_organization, [
        ?expect(fetch,
            ?withArgs([EntName, OrgName]),
            ?andReturn({ok, org})),
        ?expect(getval,
            ?withArgs([id, org]),
            ?andReturn(OrgId))
    ]),
    hoax:mock(deliv_db,
        ?expect(qfetch,
            ?withArgs([deliv_project, projects, [OrgId]]),
            ?andReturn({error, reason}))
    ),

    ExpectedResponse = {halt, request4, State},

    ?assertEqual(ExpectedResponse, deliv_hand_projects:to_json(request, State)),
    ?verifyAll.

fetch_scm_url_returns_a_tuple_of_undefined_and_the_scm_urls_if_local() ->
    Result = deliv_hand_projects:fetch_scm_url(ent_name, <<"local">>, [scm_urls]),
    ?assertEqual({undefined, [scm_urls]}, Result).

fetch_scm_url_returns_a_tuple_of_an_scm_url_and_the_scm_urls_if_githubv2_and_a_githubv2_url() ->
    ScmUrl = <<"https://github.com">>,
    ScmType = <<"githubV2">>,
    ScmUrls = [{ScmType, ScmUrl}],
    Result = deliv_hand_projects:fetch_scm_url(ent_name, ScmType, ScmUrls),
    ?assertEqual({ScmUrl, ScmUrls}, Result).

fetch_scm_url_returns_a_tuple_of_an_scm_url_and_the_scm_urls_if_bitbucket_and_a_bitbucket_url() ->
    ScmUrl = <<"https://github.com">>,
    ScmType = <<"bitbucket">>,
    ScmUrls = [{ScmType, ScmUrl}],
    Result = deliv_hand_projects:fetch_scm_url(ent_name, ScmType, ScmUrls),
    ?assertEqual({ScmUrl, ScmUrls}, Result).

fetch_scm_url_fetches_creds_and_returns_a_tuple_of_an_scm_url_and_the_scm_urls_if_githubv2_and_no_existing_scm_urls() ->
    ScmUrl = <<"https://github.com">>,
    ScmUrls = {<<"bitbucket">>, <<"https://bitbucket.com">>},
    ScmType = <<"githubV2">>,
    NewScmUrls = [{ScmType, ScmUrl}, ScmUrls],
    EntName = <<"ent">>,
    AuthType = <<"github">>,
    hoax:expect(receive
                    scm_basic_auth:load_basic_auth_credentials(EntName, AuthType) -> {ok, scm_config};
                    deliv_basic_auth_application:getval(root_api_url, scm_config) -> ScmUrl
                end),

    Result = deliv_hand_projects:fetch_scm_url(EntName, ScmType, [ScmUrls]),
    ?assertEqual({ScmUrl, NewScmUrls}, Result).

fetch_scm_url_fetches_creds_and_returns_a_tuple_of_undefined_and_the_scm_urls_if_githubv2_and_no_creds() ->
    ScmUrls = [],
    ScmType = <<"githubV2">>,
    EntName = <<"ent">>,
    AuthType = <<"github">>,
    hoax:expect(receive
                    scm_basic_auth:load_basic_auth_credentials(EntName, AuthType) -> {error, not_found}
                end),

    Result = deliv_hand_projects:fetch_scm_url(EntName, ScmType, ScmUrls),
    ?assertEqual({undefined, []}, Result).

fetch_scm_url_fetches_creds_and_returns_a_tuple_of_an_scm_url_and_the_scm_urls_if_bitbucket_and_no_existing_scm_urls() ->
    ScmUrl = <<"https://bitbucket.com">>,
    ScmUrls = {<<"githubV2">>, <<"https://github.com">>},
    ScmType = <<"bitbucket">>,
    NewScmUrls = [{ScmType, ScmUrl}, ScmUrls],
    EntName = <<"ent">>,
    AuthType = <<"bitbucket">>,
    hoax:expect(receive
                    scm_basic_auth:load_basic_auth_credentials(EntName, AuthType) -> {ok, scm_config};
                    deliv_basic_auth_application:getval(root_api_url, scm_config) -> ScmUrl
                end),

    Result = deliv_hand_projects:fetch_scm_url(EntName, ScmType, [ScmUrls]),
    ?assertEqual({ScmUrl, NewScmUrls}, Result).
