-module(scm_cred_validation_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

validate_auth_creds_fixture_test_() ->
    hoax:fixture(?MODULE, "validate_auth_creds_").

validate_auth_creds_returns_400_bad_request_when_url_is_invalid() ->
    Url = <<"foobear">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    State = #handler{},

    hoax:expect(receive
         deliv_web_utils:validate_url_is_well_formed(Url) -> error;
         deliv_web_utils:error_response(400, bad_request, "Invalid URL", req, State) -> error
     end),

    Actual = scm_cred_validation:validate_auth_creds(func, Url, UserId, Password, <<"bitbucket">>, req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

validate_auth_creds_returns_400_with_helpful_error_when_url_is_unreachable() ->
    Url = <<"http://example.com">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    State = #handler{},
    Error = {error, {conn_failed, {error, nxdomain}}},

    hoax:mock(deliv_web_utils, [
              ?expect(validate_url_is_well_formed,
                      ?withArgs([Url]),
                      ?andReturn(ok)),
              ?expect(error_response,
                      ?withArgs([400, bad_request, "Could not connect to Bitbucket Instance URL.", req, State]),
                      ?andReturn(error))]),
    hoax:mock(scm_bitbucket_rest_api,
              ?expect(check_reachability,
                      ?withArgs([Url, UserId, Password]),
                      ?andReturn(Error))),

    Actual = scm_cred_validation:validate_auth_creds(func, Url, UserId, Password, <<"bitbucket">>, req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

validate_auth_creds_when_github_returns_400_with_helpful_error_when_url_is_unreachable() ->
    Url = <<"http://example.com">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    State = #handler{},
    Error = {error, {conn_failed, {error, nxdomain}}},

    hoax:mock(deliv_web_utils, [
              ?expect(validate_url_is_well_formed,
                      ?withArgs([Url]),
                      ?andReturn(ok)),
              ?expect(error_response,
                      ?withArgs([400, bad_request, "Could not connect to GitHub URL.", req, State]),
                      ?andReturn(error))]),
    hoax:mock(scm_github_rest_api,
              ?expect(check_reachability,
                      ?withArgs([Url, Password]),
                      ?andReturn(Error))),

    Actual = scm_cred_validation:validate_auth_creds(func, Url, UserId, Password, <<"github">>, req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

validate_auth_creds_returns_400_with_redirect_message_on_redirect() ->
    Url = <<"http://bitbucket.com">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    State = #handler{},
    Location = "https://bitbucket.com",
    Message = "Bitbucket Instance URL results in redirect. Please use 'https://bitbucket.com'.",

    hoax:mock(deliv_web_utils, [
              ?expect(validate_url_is_well_formed,
                      ?withArgs([Url]),
                      ?andReturn(ok)),
              ?expect(error_response,
                      ?withArgs([400, bad_request, Message, req, State]),
                      ?andReturn(error))]),
    hoax:mock(scm_bitbucket_rest_api,
              ?expect(check_reachability,
                      ?withArgs([Url, UserId, Password]),
                      ?andReturn({error, {redirect, Location}}))),

    Actual = scm_cred_validation:validate_auth_creds(func, Url, UserId, Password, <<"bitbucket">>, req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

validate_auth_creds_returns_400_with_error_message_on_404() ->
    Url = <<"http://bitbucket.com">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    State = #handler{},
    Message = "Bitbucket Instance URL: 404 Not Found.",

    hoax:mock(deliv_web_utils, [
              ?expect(validate_url_is_well_formed,
                      ?withArgs([Url]),
                      ?andReturn(ok)),
              ?expect(error_response,
                      ?withArgs([400, bad_request, Message, req, State]),
                      ?andReturn(error))]),
    hoax:mock(scm_bitbucket_rest_api,
              ?expect(check_reachability,
                      ?withArgs([Url, UserId, Password]),
                      ?andReturn({error, not_found}))),

    Actual = scm_cred_validation:validate_auth_creds(func, Url, UserId, Password, <<"bitbucket">>, req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

validate_auth_creds_returns_400_bad_request_when_credentials_are_bad() ->
    Url = <<"http://example.com">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    State = #handler{},

    hoax:mock(deliv_web_utils, [
              ?expect(validate_url_is_well_formed,
                      ?withArgs([Url]),
                      ?andReturn(ok)),
              ?expect(error_response,
                      ?withArgs([400, bad_request, "Invalid credentials", req, State]),
                      ?andReturn(error))]),
    hoax:mock(scm_bitbucket_rest_api,
              ?expect(check_reachability,
                      ?withArgs([Url, UserId, Password]),
                      ?andReturn({error, unauthorized}))),

    Actual = scm_cred_validation:validate_auth_creds(func, Url, UserId, Password, <<"bitbucket">>, req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

validate_auth_creds_returns_whatever_the_passed_in_function_returns_if_everything_goes_well() ->
    Url = <<"http://example.com">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    State = #handler{},
    Fun = fun(_, _, _, _, _) -> success end,

    hoax:mock(deliv_web_utils,
              ?expect(validate_url_is_well_formed,
                      ?withArgs([Url]),
                      ?andReturn(ok))),
    hoax:mock(scm_bitbucket_rest_api,
              ?expect(check_reachability,
                      ?withArgs([Url, UserId, Password]),
                      ?andReturn({ok, authenticated}))),

    Actual = scm_cred_validation:validate_auth_creds(Fun, Url, UserId, Password, <<"bitbucket">>, req, State),

    ?assertEqual(success, Actual),
    ?verifyAll.

%% github tests

validate_auth_creds_returns_whatever_the_passed_in_function_returns_if_everything_goes_well_github() ->
    Url = <<"http://example.com">>,
    UserId = <<"user">>,
    Token = <<"Password">>,
    State = #handler{},
    Fun = fun(_, _, _, _, _) -> success end,

    hoax:expect(receive
                deliv_web_utils:validate_url_is_well_formed(Url) -> ok;
                scm_github_rest_api:check_reachability(Url, Token) -> {ok, authenticated}
            end),

    Actual = scm_cred_validation:validate_auth_creds(Fun, Url, UserId, Token, <<"github">>, req, State),

    ?assertEqual(success, Actual),
    ?verifyAll.

validate_auth_creds_returns_400_bad_request_when_github_credentials_are_bad() ->
    Url = <<"http://example.com">>,
    UserId = <<"user">>,
    Token = <<"Token">>,
    State = #handler{},
    Fun = fun(_, _, _, _, _) -> success end,

    hoax:expect(receive
                    deliv_web_utils:validate_url_is_well_formed(Url) -> ok;
                    deliv_web_utils:error_response(400, bad_request, "Invalid credentials", req, State) -> error;
                    scm_github_rest_api:check_reachability(Url, Token) -> {error, unauthorized}
                end),

    Actual = scm_cred_validation:validate_auth_creds(Fun, Url, UserId, Token, <<"github">>, req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

validate_auth_creds_when_github_returns_400_with_redirect_message_on_redirect() ->
    Url = <<"http://github.com">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    State = #handler{},
    Location = "https://github.com",
    Message = "GitHub URL results in redirect. Please use 'https://github.com'.",

    hoax:mock(deliv_web_utils, [
              ?expect(validate_url_is_well_formed,
                      ?withArgs([Url]),
                      ?andReturn(ok)),
              ?expect(error_response,
                      ?withArgs([400, bad_request, Message, req, State]),
                      ?andReturn(error))]),
    hoax:mock(scm_github_rest_api,
              ?expect(check_reachability,
                      ?withArgs([Url, Password]),
                      ?andReturn({error, {redirect, Location}}))),

    Actual = scm_cred_validation:validate_auth_creds(func, Url, UserId, Password, <<"github">>, req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

validate_auth_creds_returns_400_bad_request_when_password_is_not_passed_in_and_doesnt_exist() ->
    Url = <<"foobear">>,
    UserId = <<"user">>,
    EntName = <<"enterprise">>,
    ScmType = <<"bitbucket">>,
    Password = undefined,
    State = #handler{ent_name = EntName},
    BasicAuth = deliv_basic_auth_application:fromlist([{name, <<"bitbucket">>}]),

    hoax:expect(receive
         scm_basic_auth:load_basic_auth_credentials(EntName, ScmType) -> {ok, BasicAuth};
         deliv_web_utils:error_response(400, bad_request, "Invalid Password", req, State) -> error
     end),

    Actual = scm_cred_validation:validate_auth_creds(func, Url, UserId, Password, ScmType, req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

validate_auth_creds_finds_and_uses_existing_password_when_password_is_not_passed_in_via_json() ->
    Url = <<"foobear">>,
    UserId = <<"user">>,
    EntName = <<"enterprise">>,
    ScmType = <<"bitbucket">>,
    IncomingPassword = undefined,
    ExistingPassword = <<"password">>,
    State = #handler{ent_name = EntName},
    BasicAuth = deliv_basic_auth_application:fromlist([{name, <<"bitbucket">>}, {password, ExistingPassword}]),
    Fun = fun(_, _, _, _, _) -> success end,

    hoax:expect(receive
         scm_basic_auth:load_basic_auth_credentials(EntName, ScmType) -> {ok, BasicAuth};
         deliv_web_utils:validate_url_is_well_formed(Url) -> ok;
         scm_bitbucket_rest_api:check_reachability(Url, UserId, ExistingPassword) -> {ok, authenticated}
     end),

    Actual = scm_cred_validation:validate_auth_creds(Fun, Url, UserId, IncomingPassword, ScmType, req, State),

    ?assertEqual(success, Actual),
    ?verifyAll.
