-module(deliv_hand_scm_providers_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

content_types_provided_accepts_json_test() ->
    ExpectedContentType = {[{{<<"application">>,<<"json">>,'*'},to_json}], request, state},
    ?assertEqual(ExpectedContentType, deliv_hand_scm_providers:content_types_provided(request, state)).

allowed_methods_returns_get_test() ->
    ?assertEqual({[<<"GET">>], request, state}, deliv_hand_scm_providers:allowed_methods(request, state)).

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, to_json).

to_json_returns_list_of_scm_providers() ->
    EntName = <<"TestEnt">>,
    UserName = <<"TestUser">>,
    State = #handler{ent_name = EntName, user_name = UserName},

    hoax:mock(cowboy_req,
        ?expect(set_resp_header,
            ?withArgs([<<"content-type">>,<<"application/json">>,request2]),
            ?andReturn({EntName, request2}))
    ),
    hoax:mock(scm_basic_auth, [
        ?expect(load_basic_auth_credentials,
            ?withArgs([EntName, <<"bitbucket">>]),
            ?andReturn({error, not_found})),
        ?expect(load_basic_auth_credentials,
            ?withArgs([EntName, <<"github">>]),
            ?andReturn({error, not_found})
       )]
    ),

    ExpectedJson = [
        {[
            {<<"type">>, <<"local">>},
            {<<"name">>, <<"Chef Automate">>},
            {<<"projectCreateUri">>, <<"/projects">>},
            {<<"scmSetupConfigs">>, [true]}
        ]},
        {[
            {<<"type">>, <<"github">>},
            {<<"name">>, <<"GitHub">>},
            {<<"verify_ssl">>, true},
            {<<"projectCreateUri">>, <<"/github-projects">>},
            {<<"scmSetupConfigs">>, []}
        ]},
        {[
            {<<"type">>, <<"bitbucket">>},
            {<<"name">>, <<"Bitbucket">>},
            {<<"projectCreateUri">>, <<"/bitbucket-projects">>},
            {<<"scmSetupConfigs">>, []}
        ]}
    ],
    ExpectedResponse = deliv_web_utils:content(ExpectedJson, request2, State),

    ?assertEqual(ExpectedResponse, deliv_hand_scm_providers:to_json(request2, State)),
    ?verifyAll.

to_json_returns_list_of_scm_providers_including_setup_config_for_bitbucket() ->
    EntName = <<"TestEnt">>,
    UserName = <<"TestUser">>,
    State = #handler{ent_name = EntName, user_name = UserName},
    BBAuth = deliv_basic_auth_application:fromlist([
        {name, <<"bitbucket">>},
        {root_api_url, <<"http://bitbucket.url">>},
        {user_id, UserName},
        {password, <<"TestPass">>},
        {ent_id, 1}
    ]),
    BBAuthJson = chef_json:encode(chef_json:record_to_json(BBAuth)),

    hoax:mock(cowboy_req,
        ?expect(set_resp_header,
            ?withArgs([<<"content-type">>,<<"application/json">>,request2]),
            ?andReturn({EntName, request2}))
    ),
    hoax:mock(scm_basic_auth, [
        ?expect(load_basic_auth_credentials,
            ?withArgs([EntName, <<"bitbucket">>]),
            ?andReturn({ok, BBAuth})),
        ?expect(to_ejson_with_self_hal,
            ?withArgs([EntName, BBAuth]),
            ?andReturn(BBAuthJson)),
        ?expect(load_basic_auth_credentials,
            ?withArgs([EntName, <<"github">>]),
            ?andReturn({error, not_found}))
    ]),

    ExpectedJson = [
        {[
            {<<"type">>, <<"local">>},
            {<<"name">>, <<"Chef Automate">>},
            {<<"projectCreateUri">>, <<"/projects">>},
            {<<"scmSetupConfigs">>, [true]}
        ]},
        {[
            {<<"type">>, <<"github">>},
            {<<"name">>, <<"GitHub">>},
            {<<"verify_ssl">>, true},
            {<<"projectCreateUri">>, <<"/github-projects">>},
            {<<"scmSetupConfigs">>, []}
        ]},
        {[
            {<<"type">>, <<"bitbucket">>},
            {<<"name">>, <<"Bitbucket">>},
            {<<"projectCreateUri">>, <<"/bitbucket-projects">>},
            {<<"scmSetupConfigs">>, [BBAuthJson]}
        ]}
    ],
    ExpectedResponse = deliv_web_utils:content(ExpectedJson, request2, State),

    ?assertEqual(ExpectedResponse, deliv_hand_scm_providers:to_json(request2, State)),
    ?verifyAll.

to_json_returns_list_of_scm_providers_including_setup_config_for_bitbucket_and_github() ->
    EntName = <<"TestEnt">>,
    UserName = <<"TestUser">>,
    State = #handler{ent_name = EntName, user_name = UserName},
    BBAuth = deliv_basic_auth_application:fromlist([
        {name, <<"bitbucket">>},
        {root_api_url, <<"http://bitbucket.url">>},
        {user_id, UserName},
        {password, <<"TestPass">>},
        {ent_id, 1}
    ]),
    BBAuthJson = chef_json:encode(chef_json:record_to_json(BBAuth)),
    GHAuth = deliv_basic_auth_application:fromlist([
        {name, <<"github">>},
        {root_api_url, <<"http://github.url">>},
        {user_id, UserName},
        {password, <<"TestPass">>},
        {ent_id, 1}
    ]),
    GHAuthJson = chef_json:encode(chef_json:record_to_json(GHAuth)),

    hoax:mock(cowboy_req,
        ?expect(set_resp_header,
            ?withArgs([<<"content-type">>,<<"application/json">>,request2]),
            ?andReturn({EntName, request2}))
    ),
    hoax:mock(scm_basic_auth, [
        ?expect(load_basic_auth_credentials,
            ?withArgs([EntName, <<"bitbucket">>]),
            ?andReturn({ok, BBAuth})),
        ?expect(to_ejson_with_self_hal,
            ?withArgs([EntName, BBAuth]),
            ?andReturn(BBAuthJson)),
        ?expect(load_basic_auth_credentials,
            ?withArgs([EntName, <<"github">>]),
            ?andReturn({ok, GHAuth})),
        ?expect(to_ejson_with_self_hal,
            ?withArgs([EntName, GHAuth]),
            ?andReturn(GHAuthJson))
    ]),

    ExpectedJson = [
        {[
            {<<"type">>, <<"local">>},
            {<<"name">>, <<"Chef Automate">>},
            {<<"projectCreateUri">>, <<"/projects">>},
            {<<"scmSetupConfigs">>, [true]}
        ]},
        {[
            {<<"type">>, <<"github">>},
            {<<"name">>, <<"GitHub">>},
            {<<"verify_ssl">>, true},
            {<<"projectCreateUri">>, <<"/github-projects">>},
            {<<"scmSetupConfigs">>, [GHAuthJson]}
        ]},
        {[
            {<<"type">>, <<"bitbucket">>},
            {<<"name">>, <<"Bitbucket">>},
            {<<"projectCreateUri">>, <<"/bitbucket-projects">>},
            {<<"scmSetupConfigs">>, [BBAuthJson]}
        ]}
    ],
    ExpectedResponse = deliv_web_utils:content(ExpectedJson, request2, State),

    ?assertEqual(ExpectedResponse, deliv_hand_scm_providers:to_json(request2, State)),
    ?verifyAll.
