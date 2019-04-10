-module(deliv_basic_auth_application_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

encrypt_and_insert_test_() ->
    hoax:fixture(?MODULE, "encrypt_and_insert").

encrypt_and_update_test_() ->
    hoax:fixture(?MODULE, "encrypt_and_update").

fetch_by_name_and_decrypt_test_() ->
    hoax:fixture(?MODULE, "fetch_by_name_and_decrypt").

fetch_by_name_url_and_decrypt_test_() ->
    hoax:fixture(?MODULE, "fetch_by_name_url_and_decrypt").

encryption_round_trip_test_() ->
    hoax:fixture(?MODULE, "encryption_round_trip").

delete_by_enterprise_name_url_app_name_test_() ->
    hoax:fixture(?MODULE, "delete_by_enterprise_name_url_app_name").

encrypt_and_insert() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([{name, <<"mockname">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, <<"mockpw">>},
                                                       {ent_id, <<"mockentid">>}], BasicAuth),
    EncryptedAuth2 = deliv_basic_auth_application:setvals(
                       [{password, base64:encode(<<"mockpw">>)}],
                       BasicAuth2),
    hoax:mock(deliv_db,
              ?expect(insert,
                      ?withArgs([EncryptedAuth2]),
                      ?andReturn([EncryptedAuth2]))),

    {ok, PersistedVal} = deliv_basic_auth_application:encrypt_and_insert(<<"mockname">>,
                                                                         <<"http://mock.url">>,
                                                                         <<"mockuserid">>,
                                                                         <<"mockpw">>,
                                                                         <<"mockentid">>),
    ?assertEqual(<<"mockpw">>, deliv_basic_auth_application:getval(password,
                                                                   PersistedVal)),
    ?verifyAll.

encrypt_and_insert_returns_error_conflict_when_conflict() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([{name, <<"mockname">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, <<"mockpw">>},
                                                       {ent_id, <<"mockentid">>}], BasicAuth),
    EncryptedAuth2 = deliv_basic_auth_application:setvals(
                       [{password, base64:encode(<<"mockpw">>)}],
                       BasicAuth2),
    hoax:mock(deliv_db,
              ?expect(insert,
                      ?withArgs([EncryptedAuth2]),
                      ?andReturn({error, {conflict, db_conflict}}))),

    Result = deliv_basic_auth_application:encrypt_and_insert(<<"mockname">>,
                                                             <<"http://mock.url">>,
                                                             <<"mockuserid">>,
                                                             <<"mockpw">>,
                                                             <<"mockentid">>),
    ?assertEqual({error, conflict}, Result),
    ?verifyAll.

encrypt_and_insert_forwards_db_errors() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([{name, <<"mockname">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, <<"mockpw">>},
                                                       {ent_id, <<"mockentid">>}], BasicAuth),
    EncryptedAuth2 = deliv_basic_auth_application:setvals(
                         [{password, base64:encode(<<"mockpw">>)}],
                         BasicAuth2),
    hoax:mock(deliv_db,
              ?expect(insert,
                      ?withArgs([EncryptedAuth2]),
                      ?andReturn({error, db_error}))),

    Result = deliv_basic_auth_application:encrypt_and_insert(<<"mockname">>,
                                                             <<"http://mock.url">>,
                                                             <<"mockuserid">>,
                                                             <<"mockpw">>,
                                                             <<"mockentid">>),
    ?assertEqual({error, db_error}, Result),
    ?verifyAll.

encrypt_and_update() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([{name, <<"mockname">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, <<"mockpw">>},
                                                       {ent_id, <<"mockentid">>}], BasicAuth),
    EncryptedAuth2 = deliv_basic_auth_application:setvals(
                       [{password, base64:encode(<<"mockpw">>)}],
                       BasicAuth2),
    hoax:mock(sqerl_rec,
              ?expect(qfetch,
                      ?withArgs([deliv_basic_auth_application, update_by_ent_name_url,
                                 [<<"mockname">>, <<"http://mock.url">>, <<"mockuserid">>,
                                 base64:encode(<<"mockpw">>), <<"mockentid">>]]),
                      ?andReturn([EncryptedAuth2]))),

    {ok, PersistedVal} = deliv_basic_auth_application:encrypt_and_update(<<"mockname">>,
                                                                         <<"http://mock.url">>,
                                                                         <<"mockuserid">>,
                                                                         <<"mockpw">>,
                                                                         <<"mockentid">>),
    ?assertEqual(<<"mockpw">>, deliv_basic_auth_application:getval(password,
                                                                   PersistedVal)),
    ?verifyAll.

encrypt_and_update_returns_error_not_found_when_no_rows_updated() ->
    hoax:mock(sqerl_rec,
              ?expect(qfetch,
                      ?withArgs([deliv_basic_auth_application, update_by_ent_name_url,
                                 [<<"mockname">>, <<"http://mock.url">>, <<"mockuserid">>,
                                  base64:encode(<<"mockpw">>), <<"mockentid">>]]),
                      ?andReturn({error, {{ok, 0}, db_error}}))),

    Result = deliv_basic_auth_application:encrypt_and_update(<<"mockname">>,
                                                             <<"http://mock.url">>,
                                                             <<"mockuserid">>,
                                                             <<"mockpw">>,
                                                             <<"mockentid">>),
    ?assertEqual({error, not_found}, Result),
    ?verifyAll.

encrypt_and_update_forwards_db_errors() ->
    hoax:mock(sqerl_rec,
              ?expect(qfetch,
                      ?withArgs([deliv_basic_auth_application, update_by_ent_name_url,
                                 [<<"mockname">>, <<"http://mock.url">>, <<"mockuserid">>,
                                  base64:encode(<<"mockpw">>), <<"mockentid">>]]),
                      ?andReturn({error, db_error}))),

    Result = deliv_basic_auth_application:encrypt_and_update(<<"mockname">>,
                                                             <<"http://mock.url">>,
                                                             <<"mockuserid">>,
                                                             <<"mockpw">>,
                                                             <<"mockentid">>),
    ?assertEqual({error, db_error}, Result),
    ?verifyAll.

fetch_by_name_and_decrypt() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"mockname">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password,base64:encode(<<"mockpw">>)},
                                                       {ent_id, <<"mockentid">>}], BasicAuth),
    hoax:mock(sqerl_rec,
              ?expect(qfetch,
                      ?withArgs([deliv_basic_auth_application, fetch_by_enterprise_name_app_name, [<<"mockent">>, <<"mockname">>]]),
                      ?andReturn([BasicAuth2]))),

    {ok, Result} = deliv_basic_auth_application:fetch_by_enterprise_name_app_name_and_decrypt(<<"mockent">>, <<"mockname">>),
    ?assertEqual(<<"mockpw">>, deliv_basic_auth_application:getval(password,
                                                                   Result)),
    ?verifyAll.

fetch_by_name_and_decrypt_returns_error_not_found_when_not_found() ->
    hoax:mock(sqerl_rec,
              ?expect(qfetch,
                      ?withArgs([deliv_basic_auth_application, fetch_by_enterprise_name_app_name, [<<"mockent">>, <<"mockname">>]]),
                      ?andReturn([]))),

    Result = deliv_basic_auth_application:fetch_by_enterprise_name_app_name_and_decrypt(<<"mockent">>, <<"mockname">>),
    ?assertEqual({error, not_found}, Result),
    ?verifyAll.

fetch_by_name_and_decrypt_forwards_db_errors() ->
    hoax:mock(sqerl_rec,
              ?expect(qfetch,
                      ?withArgs([deliv_basic_auth_application, fetch_by_enterprise_name_app_name, [<<"mockent">>, <<"mockname">>]]),
                      ?andReturn({error, db_error}))),

    Result = deliv_basic_auth_application:fetch_by_enterprise_name_app_name_and_decrypt(<<"mockent">>, <<"mockname">>),
    ?assertEqual({error, db_error}, Result),
    ?verifyAll.

fetch_by_name_url_and_decrypt() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"mockname">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password,base64:encode(<<"mockpw">>)},
                                                       {ent_id, <<"mockentid">>}], BasicAuth),
    hoax:mock(sqerl_rec,
              ?expect(qfetch,
                      ?withArgs([deliv_basic_auth_application, fetch_by_enterprise_name_url_app_name, [<<"mockent">>, <<"mockname">>, <<"http://mock.url">>]]),
                      ?andReturn([BasicAuth2]))),

    {ok, Result} = deliv_basic_auth_application:fetch_by_enterprise_name_url_app_name_and_decrypt(<<"mockent">>, <<"mockname">>, <<"http://mock.url">>),
    ?assertEqual(<<"mockpw">>, deliv_basic_auth_application:getval(password,
                                                                   Result)),
    ?verifyAll.

fetch_by_name_url_and_decrypt_returns_error_not_found_when_not_found() ->
    hoax:mock(sqerl_rec,
              ?expect(qfetch,
                      ?withArgs([deliv_basic_auth_application, fetch_by_enterprise_name_url_app_name, [<<"mockent">>, <<"mockname">>, <<"http://mock.url">>]]),
                      ?andReturn([]))),

    Result = deliv_basic_auth_application:fetch_by_enterprise_name_url_app_name_and_decrypt(<<"mockent">>, <<"mockname">>, <<"http://mock.url">>),
    ?assertEqual({error, not_found}, Result),
    ?verifyAll.

fetch_by_name_url_and_decrypt_forwards_db_errors() ->
    hoax:mock(sqerl_rec,
              ?expect(qfetch,
                      ?withArgs([deliv_basic_auth_application, fetch_by_enterprise_name_url_app_name, [<<"mockent">>, <<"mockname">>, <<"http://mock.url">>]]),
                      ?andReturn({error, db_error}))),

    Result = deliv_basic_auth_application:fetch_by_enterprise_name_url_app_name_and_decrypt(<<"mockent">>, <<"mockname">>, <<"http://mock.url">>),
    ?assertEqual({error, db_error}, Result),
    ?verifyAll.

encryption_round_trip() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"mockname">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, base64:encode(<<"mockpw">>)},
                                                       {ent_id, <<"mockentid">>}], BasicAuth),
    DecryptedRec = deliv_basic_auth_application:decrypt_password(BasicAuth2),

    ?assertEqual(<<"mockpw">>, deliv_basic_auth_application:getval(password, DecryptedRec)).

delete_by_enterprise_name_url_app_name_returns_ok_when_delete_is_successful() ->
    EntName = <<"mockent">>,
    AppName = <<"mockapp">>,
    RootApiUrl = <<"http://mock.url">>,

    hoax:mock(sqerl_rec,
              ?expect(cquery,
                      ?withArgs([deliv_basic_auth_application,
                                 delete_by_enterprise_name_url_app_name,
                                 [EntName, AppName, RootApiUrl]]),
                      ?andReturn({ok, 1}))),

    Actual = deliv_basic_auth_application:delete_by_enterprise_name_url_app_name(EntName, AppName, RootApiUrl),
    ?assertEqual(ok, Actual),
    ?verifyAll.

delete_by_enterprise_name_url_app_name_returns_error_not_found_when_application_is_not_found() ->
    EntName = <<"mockent">>,
    AppName = <<"mockapp">>,
    RootApiUrl = <<"http://mock.url">>,

    hoax:mock(sqerl_rec,
              ?expect(cquery,
                      ?withArgs([deliv_basic_auth_application,
                                 delete_by_enterprise_name_url_app_name,
                                 [EntName, AppName, RootApiUrl]]),
                      ?andReturn({ok, 0}))),

    Actual = deliv_basic_auth_application:delete_by_enterprise_name_url_app_name(EntName, AppName, RootApiUrl),
    ?assertEqual({error, not_found}, Actual),
    ?verifyAll.

delete_by_enterprise_name_url_app_name_forwards_other_errors() ->
    EntName = <<"mockent">>,
    AppName = <<"mockapp">>,
    RootApiUrl = <<"http://mock.url">>,

    hoax:mock(sqerl_rec,
              ?expect(cquery,
                      ?withArgs([deliv_basic_auth_application,
                                 delete_by_enterprise_name_url_app_name,
                                 [EntName, AppName, RootApiUrl]]),
                      ?andReturn({error, some_error}))),

    Actual = deliv_basic_auth_application:delete_by_enterprise_name_url_app_name(EntName, AppName, RootApiUrl),
    ?assertEqual({error, some_error}, Actual),
    ?verifyAll.
