-module(scm_basic_auth_integration_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_ () ->
    hoax:parameterized_fixture(?MODULE, setup, teardown).

setup() ->
    error_logger:tty(false),
    eu_database:setup(),
    eu_data:with_enterprise(<<"mockent">>, fun(Enterprise) ->
                                                    {deliv_enterprise:getval(id,Enterprise),
                                                    deliv_enterprise:getval(name,Enterprise)} end).

teardown(_) ->
    eu_database:teardown(),
    error_logger:tty(true).

%% Bitbucket scm provider tests

save_basic_auth_credentials({EntId, _}) ->
    {ok, BasicAuth} = scm_basic_auth:save_basic_auth_credentials(<<"http://mock.url">>,
                                                                       <<"mockuserid">>,
                                                                       <<"mockpw">>,
                                                                       EntId,
                                                                       <<"bitbucket">>),

    ?assertEqual(<<"bitbucket">>, deliv_basic_auth_application:getval(name, BasicAuth)),
    ?assertEqual(<<"http://mock.url">>, deliv_basic_auth_application:getval(root_api_url, BasicAuth)),
    ?assertEqual(<<"mockuserid">>, deliv_basic_auth_application:getval(user_id, BasicAuth)),
    ?assertEqual(<<"mockpw">>, deliv_basic_auth_application:getval(password, BasicAuth)),
    ?assertEqual(EntId, deliv_basic_auth_application:getval(ent_id, BasicAuth)).

save_basic_auth_credentials_prevent_duplicate({EntId, _}) ->
    ?assertMatch({ok, _},
                 scm_basic_auth:save_basic_auth_credentials(<<"http://mock.url">>,
                                                                  <<"mockuserid">>,
                                                                  <<"mockpw">>,
                                                                  EntId,
                                                                  <<"bitbucket">>)),
    ?assertEqual({error, conflict},
                 scm_basic_auth:save_basic_auth_credentials(<<"http://mock.url">>,
                                                                  <<"mockuserid">>,
                                                                  <<"mockpw">>,
                                                                  EntId,
                                                                  <<"bitbucket">>)).

load_basic_auth_credentials({EntId, EntName}) ->
    scm_basic_auth:save_basic_auth_credentials(<<"http://mock.url">>,
                                                     <<"mockuserid">>,
                                                     <<"mockpw">>,
                                                     EntId,
                                                     <<"bitbucket">>),

    {ok, BasicAuth} = scm_basic_auth:load_basic_auth_credentials(EntName, <<"bitbucket">>),


    ?assertEqual(<<"bitbucket">>, deliv_basic_auth_application:getval(name, BasicAuth)),
    ?assertEqual(<<"http://mock.url">>, deliv_basic_auth_application:getval(root_api_url, BasicAuth)),
    ?assertEqual(<<"mockuserid">>, deliv_basic_auth_application:getval(user_id, BasicAuth)),
    ?assertEqual(<<"mockpw">>, deliv_basic_auth_application:getval(password, BasicAuth)),
    ?assertEqual(EntId, deliv_basic_auth_application:getval(ent_id, BasicAuth)).

delete_basic_auth_credentials({EntId, EntName}) ->
    Url = <<"http://mock.url">>,
    scm_basic_auth:save_basic_auth_credentials(Url,
                                                     <<"mockuserid">>,
                                                     <<"mockpw">>,
                                                     EntId,
                                                     <<"bitbucket">>),

    ?assertEqual(ok, scm_basic_auth:delete_basic_auth_credentials(EntName, Url, <<"bitbucket">>)).

%% Github scm provider tests

save_basic_auth_credentials_github({EntId, _}) ->
    {ok, BasicAuth} = scm_basic_auth:save_basic_auth_credentials(<<"http://mock.url">>,
                                                                       <<"mockuserid">>,
                                                                       <<"mockpw">>,
                                                                       EntId,
                                                                       <<"github">>),

    ?assertEqual(<<"github">>, deliv_basic_auth_application:getval(name, BasicAuth)),
    ?assertEqual(<<"http://mock.url">>, deliv_basic_auth_application:getval(root_api_url, BasicAuth)),
    ?assertEqual(<<"mockuserid">>, deliv_basic_auth_application:getval(user_id, BasicAuth)),
    ?assertEqual(<<"mockpw">>, deliv_basic_auth_application:getval(password, BasicAuth)),
    ?assertEqual(EntId, deliv_basic_auth_application:getval(ent_id, BasicAuth)).

save_basic_auth_credentials_prevent_duplicate_github({EntId, _}) ->
    ?assertMatch({ok, _},
                 scm_basic_auth:save_basic_auth_credentials(<<"http://mock.url">>,
                                                                  <<"mockuserid">>,
                                                                  <<"mockpw">>,
                                                                  EntId,
                                                                  <<"github">>)),
    ?assertEqual({error, conflict},
                 scm_basic_auth:save_basic_auth_credentials(<<"http://mock.url">>,
                                                                  <<"mockuserid">>,
                                                                  <<"mockpw">>,
                                                                  EntId,
                                                                  <<"github">>)).

load_basic_auth_credentials_github({EntId, EntName}) ->
    scm_basic_auth:save_basic_auth_credentials(<<"http://mock.url">>,
                                                     <<"mockuserid">>,
                                                     <<"mockpw">>,
                                                     EntId,
                                                     <<"github">>),

    {ok, BasicAuth} = scm_basic_auth:load_basic_auth_credentials(EntName, <<"github">>),


    ?assertEqual(<<"github">>, deliv_basic_auth_application:getval(name, BasicAuth)),
    ?assertEqual(<<"http://mock.url">>, deliv_basic_auth_application:getval(root_api_url, BasicAuth)),
    ?assertEqual(<<"mockuserid">>, deliv_basic_auth_application:getval(user_id, BasicAuth)),
    ?assertEqual(<<"mockpw">>, deliv_basic_auth_application:getval(password, BasicAuth)),
    ?assertEqual(EntId, deliv_basic_auth_application:getval(ent_id, BasicAuth)).
