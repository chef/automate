-module(scm_basic_auth_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_ () ->
    hoax:fixture(?MODULE).

%% save_basic_auth_credentials/3
save_basic_auth_credentials() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"bitbucket">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, <<"mockpw">>}], BasicAuth),
    hoax:expect(receive
        deliv_basic_auth_application:encrypt_and_insert(<<"bitbucket">>,
                                                        <<"http://mock.url">>,
                                                        <<"mockuserid">>,
                                                        <<"mockpw">>,
                                                        <<"mockentid">>) -> {ok, BasicAuth2}
        end),

    ?assertEqual({ok, BasicAuth2},
                 scm_basic_auth:save_basic_auth_credentials(<<"http://mock.url">>,
                                                                  <<"mockuserid">>,
                                                                  <<"mockpw">>,
                                                                  <<"mockentid">>,
                                                                  <<"bitbucket">>)),
    ?verifyAll.

%% update_basic_auth_credentials/3
update_basic_auth_credentials() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"bitbucket">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, <<"mockpw">>}], BasicAuth),
    hoax:expect(receive
        deliv_basic_auth_application:encrypt_and_update(<<"bitbucket">>,
                                                        <<"http://mock.url">>,
                                                        <<"mockuserid">>,
                                                        <<"mockpw">>,
                                                        <<"mockentid">>) -> {ok, BasicAuth2}
        end),

    ?assertEqual({ok, BasicAuth2},
                 scm_basic_auth:update_basic_auth_credentials(<<"http://mock.url">>,
                                                                    <<"mockuserid">>,
                                                                    <<"mockpw">>,
                                                                    <<"mockentid">>,
                                                                    <<"bitbucket">>)),
    ?verifyAll.

%% load_basic_auth_credentials/2
load_basic_auth_credentials() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"bitbucket">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, <<"mockpw">>}], BasicAuth),
    hoax:expect(receive
        deliv_basic_auth_application:fetch_by_enterprise_name_app_name_and_decrypt(
                                      <<"mockname">>,
                                      <<"bitbucket">>) -> {ok, BasicAuth2}
    end),

    ?assertEqual({ok, BasicAuth2},
                 scm_basic_auth:load_basic_auth_credentials(<<"mockname">>, <<"bitbucket">>)),
    ?verifyAll.

%% load_basic_auth_credentials/3
load_basic_auth_credentials_arity_3() ->
    EntName = <<"mockentname">>,
    Url = <<"http://mock.url">>,
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"bitbucket">>},
                                                       {root_api_url, Url},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, <<"mockpw">>}], BasicAuth),
    hoax:expect(receive
          deliv_basic_auth_application:fetch_by_enterprise_name_url_app_name_and_decrypt(
                      EntName, <<"bitbucket">>, Url) -> {ok, BasicAuth2}
    end),

    ?assertEqual({ok, BasicAuth2},
                 scm_basic_auth:load_basic_auth_credentials(EntName, <<"bitbucket">>, Url)),
    ?verifyAll.

%% delete_basic_auth_credentials/3
delete_basic_auth_credentials_deletes_bitbucket_link_when_no_projects_exist() ->
    EntName = <<"mockentname">>,
    Url = <<"http://mock.url">>,
    ScmType = <<"bitbucket">>,

    hoax:expect(receive
         scm_bitbucket_project_metadata:fetch_all_for_ent_name_url(EntName,
                                                               Url) -> {error, not_found};
         deliv_basic_auth_application:delete_by_enterprise_name_url_app_name(EntName,
                                                                             <<"bitbucket">>,
                                                                             Url) -> ok
      end),

    ?assertEqual(ok, scm_basic_auth:delete_basic_auth_credentials(EntName, Url, ScmType)),
    ?verifyAll.

delete_basic_auth_credentials_returns_error_projects_exist_when_there_are_existing_projects() ->
    EntName = <<"mockentname">>,
    Url = <<"http://mock.url">>,
    ScmType = <<"bitbucket">>,

    hoax:expect(receive
         scm_bitbucket_project_metadata:fetch_all_for_ent_name_url(EntName,
                                                               Url) -> {ok, [metadata]}
      end),

    ?assertEqual({error, projects_exist}, scm_basic_auth:delete_basic_auth_credentials(EntName, Url, ScmType)),
    ?verifyAll.

delete_basic_auth_credentials_forwards_db_error_when_checking_for_projects() ->
    EntName = <<"mockentname">>,
    Url = <<"http://mock.url">>,
    ScmType = <<"bitbucket">>,

    hoax:expect(receive
         scm_bitbucket_project_metadata:fetch_all_for_ent_name_url(EntName,
                                                               Url) -> {error, an_error}
      end),

    ?assertEqual({error, an_error}, scm_basic_auth:delete_basic_auth_credentials(EntName, Url, ScmType)),
    ?verifyAll.

delete_basic_auth_credentials_forwards_db_error_when_deleting_metadata() ->
    EntName = <<"mockentname">>,
    Url = <<"http://mock.url">>,
    ScmType = <<"bitbucket">>,

    hoax:expect(receive
         scm_bitbucket_project_metadata:fetch_all_for_ent_name_url(EntName,
                                                               Url) -> {error, not_found};
         deliv_basic_auth_application:delete_by_enterprise_name_url_app_name(EntName,
                                                                             <<"bitbucket">>,
                                                                             Url) -> {error, an_error}
      end),

    ?assertEqual({error, an_error}, scm_basic_auth:delete_basic_auth_credentials(EntName, Url, ScmType)),
    ?verifyAll.


to_ejson_with_self_hal() ->
    EntName = <<"ent_name">>,
    Url = <<"http://mock.url">>,
    UserId = <<"mockuserid">>,
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"bitbucket">>},
                                                       {root_api_url, Url},
                                                       {user_id,  UserId},
                                                       {password, <<"mockpw">>}], BasicAuth),
    EncUrl = deliv_web_utils:encode_url(Url),

    hoax:expect(receive
        deliv_web_utils:make_api_url_prefix(EntName) -> <<"http://http://192.168.33.66/api/v0/e/",
                                                          EntName/binary, "/">>;
        deliv_web_utils:encode_url(Url) -> EncUrl
      end),

    Expected = {[{<<"root_api_url">>, Url},
                 {<<"user_id">>, UserId},
                 {<<"_links">>, {[
                     {<<"self">>, {[
                         {<<"href">>, <<"http://http://192.168.33.66/api/v0/e/",
                                        EntName/binary, "/scm/bitbucket/servers/",
                                        EncUrl/binary>>}
                     ]}}
                 ]}}
                ]},
    ?assertEqual(Expected, scm_basic_auth:to_ejson_with_self_hal(EntName, BasicAuth2)).

%% github tests

%% save_basic_auth_credentials/3
save_basic_auth_credentials_github() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"github">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, <<"mockpw">>}], BasicAuth),
    hoax:expect(receive
        deliv_basic_auth_application:encrypt_and_insert(<<"github">>,
                                                        <<"http://mock.url">>,
                                                        <<"mockuserid">>,
                                                        <<"mockpw">>,
                                                        <<"mockentid">>) -> {ok, BasicAuth2}
        end),

    ?assertEqual({ok, BasicAuth2},
                 scm_basic_auth:save_basic_auth_credentials(<<"http://mock.url">>,
                                                                  <<"mockuserid">>,
                                                                  <<"mockpw">>,
                                                                  <<"mockentid">>,
                                                                  <<"github">>)),
    ?verifyAll.

%% load_basic_auth_credentials/2
load_basic_auth_credentials_github() ->
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"github">>},
                                                       {root_api_url, <<"http://mock.url">>},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, <<"mockpw">>}], BasicAuth),
    hoax:expect(receive
        deliv_basic_auth_application:fetch_by_enterprise_name_app_name_and_decrypt(
                                      <<"mockname">>,
                                      <<"github">>) -> {ok, BasicAuth2}
    end),

    ?assertEqual({ok, BasicAuth2},
                 scm_basic_auth:load_basic_auth_credentials(<<"mockname">>, <<"github">>)),
    ?verifyAll.

%% load_basic_auth_credentials/3
load_basic_auth_credentials_arity_3_github() ->
    EntName = <<"mockentname">>,
    Url = <<"http://mock.url">>,
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"github">>},
                                                       {root_api_url, Url},
                                                       {user_id,  <<"mockuserid">>},
                                                       {password, <<"mockpw">>}], BasicAuth),
    hoax:expect(receive
          deliv_basic_auth_application:fetch_by_enterprise_name_url_app_name_and_decrypt(
                      EntName, <<"github">>, Url) -> {ok, BasicAuth2}
    end),

    ?assertEqual({ok, BasicAuth2},
                 scm_basic_auth:load_basic_auth_credentials(EntName, <<"github">>, Url)),
    ?verifyAll.

to_ejson_with_self_hal_github() ->
    EntName = <<"ent_name">>,
    Url = <<"http://mock.url">>,
    UserId = <<"mockuserid">>,
    BasicAuth = deliv_basic_auth_application:'#new'(),
    BasicAuth2 = deliv_basic_auth_application:setvals([
                                                       {name, <<"github">>},
                                                       {root_api_url, Url},
                                                       {user_id,  UserId},
                                                       {password, <<"mockpw">>}], BasicAuth),
    EncUrl = deliv_web_utils:encode_url(Url),

    hoax:expect(receive
        deliv_web_utils:make_api_url_prefix(EntName) -> <<"http://http://192.168.33.66/api/v0/e/",
                                                          EntName/binary, "/">>;
        deliv_web_utils:encode_url(Url) -> EncUrl
      end),

    Expected = {[{<<"root_api_url">>, Url},
                 {<<"user_id">>, UserId},
                 {<<"_links">>, {[
                     {<<"self">>, {[
                         {<<"href">>, <<"http://http://192.168.33.66/api/v0/e/",
                                        EntName/binary, "/scm/github/servers/",
                                        EncUrl/binary>>}
                     ]}}
                 ]}}
                ]},
    ?assertEqual(Expected, scm_basic_auth:to_ejson_with_self_hal(EntName, BasicAuth2)).

%% delete_basic_auth_credentials/3
delete_basic_auth_credentials_deletes_github_link_when_no_projects_exist() ->
    EntName = <<"mockentname">>,
    Url = <<"http://mock.url">>,
    ScmType = <<"github">>,

    hoax:expect(receive
         scm_github_project_metadata:fetch_all_for_ent_name_url(EntName,
                                                               Url) -> {error, not_found};
         deliv_basic_auth_application:delete_by_enterprise_name_url_app_name(EntName,
                                                                             ScmType,
                                                                             Url) -> ok
      end),

    ?assertEqual(ok, scm_basic_auth:delete_basic_auth_credentials(EntName, Url, ScmType)),
    ?verifyAll.

delete_basic_auth_credentials_returns_error_projects_exist_when_there_are_existing_projects_github() ->
    EntName = <<"mockentname">>,
    Url = <<"http://mock.url">>,
    ScmType = <<"github">>,

    hoax:expect(receive
         scm_github_project_metadata:fetch_all_for_ent_name_url(EntName,
                                                               Url) -> {ok, [metadata]}
      end),

    ?assertEqual({error, projects_exist}, scm_basic_auth:delete_basic_auth_credentials(EntName, Url, ScmType)),
    ?verifyAll.

delete_basic_auth_credentials_forwards_db_error_when_checking_for_projects_github() ->
    EntName = <<"mockentname">>,
    Url = <<"http://mock.url">>,
    ScmType = <<"github">>,

    hoax:expect(receive
         scm_github_project_metadata:fetch_all_for_ent_name_url(EntName,
                                                               Url) -> {error, an_error}
      end),

    ?assertEqual({error, an_error}, scm_basic_auth:delete_basic_auth_credentials(EntName, Url, ScmType)),
    ?verifyAll.

delete_basic_auth_credentials_forwards_db_error_when_deleting_metadata_github() ->
    EntName = <<"mockentname">>,
    Url = <<"http://mock.url">>,
    ScmType = <<"github">>,

    hoax:expect(receive
         scm_github_project_metadata:fetch_all_for_ent_name_url(EntName,
                                                               Url) -> {error, not_found};
         deliv_basic_auth_application:delete_by_enterprise_name_url_app_name(EntName,
                                                                             <<"github">>,
                                                                             Url) -> {error, an_error}
      end),

    ?assertEqual({error, an_error}, scm_basic_auth:delete_basic_auth_credentials(EntName, Url, ScmType)),
    ?verifyAll.
