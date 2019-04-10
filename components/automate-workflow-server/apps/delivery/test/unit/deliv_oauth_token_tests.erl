-module(deliv_oauth_token_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

initialize_inserts_to_db_and_returns_record() ->
    OauthAppId = 1,
    AppName = <<"github">>,
    Scope = project,
    ScopeId = 10,
    State = chef_utils:random_string(128),
    OauthRecord = deliv_oauth_token:fromlist([{oauth_app_id, OauthAppId},
                                              {scope, Scope},
                                              {scope_id, ScopeId},
                                              {state, State}]),
    Application = deliv_oauth_application:'#new'(),

    hoax:mock(chef_utils,
              ?expect(random_string,
                      ?withArgs([128]),
                      ?andReturn(State))),
    hoax:mock(deliv_oauth_application, [
                                        ?expect(fetch,
                                                ?withArgs([AppName]),
                                                ?andReturn({ok, Application})),
                                        ?expect(getval,
                                                ?withArgs([id, Application]),
                                                ?andReturn(OauthAppId))
                                       ]),
    hoax:mock(deliv_db,
              ?expect(insert,
                      ?withArgs([OauthRecord]),
                      ?andReturn([OauthRecord]))),
    ?assertEqual({ok, OauthRecord}, deliv_oauth_token:initialize(AppName, Scope, ScopeId)),
    ?verifyAll.

authorize_url_returns_token_request_url() ->
    TokenUrl = "http://example.com/oauth/authorize",
    AppId = 1,
    AppModule = deliv_github_oauth,
    State = chef_utils:random_string(128),
    OauthRecord = deliv_oauth_token:fromlist([{oauth_app_id, AppId}, {state, State}]),
    Application = deliv_oauth_application:'#new'(),

    hoax:mock(deliv_oauth_application, [
                                        ?expect(fetch_by_id,
                                                ?withArgs([AppId]),
                                                ?andReturn({ok, Application})),
                                        ?expect(get_module,
                                                ?withArgs([Application]),
                                                ?andReturn(AppModule))
                                       ]),
    hoax:mock(AppModule,
              ?expect(authorize_url,
                      ?withArgs([State, Application]),
                      ?andReturn(TokenUrl))),

    ?assertEqual(TokenUrl, deliv_oauth_token:authorize_url(OauthRecord)),
    ?verifyAll.

fetch_token_returns_token() ->
    Code = <<"code">>,
    AppId = 1,
    AppModule = deliv_github_oauth,
    OauthRecord = deliv_oauth_token:fromlist([{oauth_app_id, AppId}]),
    Application = deliv_oauth_application:'#new'(),
    Token = chef_utils:random_string(128),

    hoax:mock(deliv_oauth_application, [
                                        ?expect(fetch_by_id,
                                                ?withArgs([AppId]),
                                                ?andReturn({ok, Application})),
                                        ?expect(get_module,
                                                ?withArgs([Application]),
                                                ?andReturn(AppModule))
                                       ]),
    hoax:mock(AppModule,
              ?expect(fetch_token,
                      ?withArgs([Code, Application]),
                      ?andReturn({ok, Token}))),

    ?assertEqual({ok, Token}, deliv_oauth_token:fetch_token(Code, OauthRecord)),
    ?verifyAll.

save_token_updates_token_in_database() ->
    Token = chef_utils:random_string(128),
    OauthAppId = 1,
    OauthRecord = deliv_oauth_token:fromlist([{oauth_app_id, OauthAppId}]),
    OauthRecordWithToken = deliv_oauth_token:setvals([{token, Token}], OauthRecord),
    AppModule = deliv_github_oauth,

    hoax:mock(deliv_oauth_application,
              ?expect(get_module_by_id,
                      ?withArgs([OauthAppId]),
                      ?andReturn(AppModule))),
    hoax:mock(AppModule,
              ?expect(save_token,
                      ?withArgs([Token, OauthRecord]),
                      ?andReturn({ok, OauthRecordWithToken}))),

    ?assertEqual({ok, OauthRecordWithToken},
                 deliv_oauth_token:save_token(Token, OauthRecord)),
    ?verifyAll.

fetch_by_enterprise_test_() ->
    hoax:parameterized_fixture(?MODULE, "fetch_by_enterprise", setup, teardown).

setup() ->
    {<<"EntName">>, deliv_oauth_module}.

teardown(_) ->
    ok.

fetch_by_enterprise_returns_tuple_if_record_exists({EntName, Module}) ->
    Record = deliv_oauth_token:'#new'(),
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_oauth_token, fetch_by_enterprise, [EntName, Module]]),
                      ?andReturn([Record]))),

    ?assertEqual({ok, Record}, deliv_oauth_token:fetch_by_enterprise(EntName, Module)),
    ?verifyAll.

fetch_by_enterprise_returns_error_if_no_record_exists({EntName, Module}) ->
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_oauth_token, fetch_by_enterprise, [EntName, Module]]),
                      ?andReturn([]))),

    ?assertEqual({error, not_found}, deliv_oauth_token:fetch_by_enterprise(EntName, Module)),
    ?verifyAll.

fetch_by_enterprise_passes_back_error({EntName, Module}) ->
    Error = {error, sql_error},
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_oauth_token, fetch_by_enterprise, [EntName, Module]]),
                      ?andReturn(Error))),

    ?assertEqual(Error, deliv_oauth_token:fetch_by_enterprise(EntName, Module)),
    ?verifyAll.
