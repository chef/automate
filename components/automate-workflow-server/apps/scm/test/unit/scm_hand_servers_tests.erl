-module(scm_hand_servers_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

init_returns_correct_tuple_test() ->
    Actual = scm_hand_servers:init(ignored, req, #handler{}),

    Expected = {upgrade, protocol, cowboy_rest, req, #handler{}},

    ?assertEqual(Expected, Actual).

rest_init_returns_correct_tuple_test() ->
    Actual = scm_hand_servers:rest_init(req, #handler{}),

    Expected = {ok, req, #handler{}},

    ?assertEqual(Expected, Actual).

allowed_methods_allows_only_POST_GET_test() ->
    ?assertEqual({[<<"POST">>, <<"GET">>], req, #handler{}},
                 scm_hand_servers:allowed_methods(req, #handler{})).

content_types_accepted_accepts_json_test() ->
    hoax:test(fun() ->
                  hoax:mock(deliv_web_utils,
                            ?expect(content_type_json_map,
                                    ?withArgs([from_json]),
                                    ?andReturn(expected_map))),

                  Actual = scm_hand_servers:content_types_accepted(req, #handler{}),

                  ?assertEqual({expected_map, req, #handler{}}, Actual),
                  ?verifyAll
              end).

content_types_provided_provides_json_test() ->
  hoax:test(fun() ->
                hoax:mock(deliv_web_utils,
                          ?expect(content_type_json_map,
                                  ?withArgs([to_json]),
                                  ?andReturn(expected_map))),

                Actual = scm_hand_servers:content_types_provided(req, #handler{}),

                ?assertEqual({expected_map, req, #handler{}}, Actual),
                ?verifyAll
            end).

from_json_fixture_test_() ->
    hoax:fixture(?MODULE, "from_json_").

from_json_returns_error_with_bad_request() ->
    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, bitbucket_server]),
                      ?andReturn({{error, reason}, req})),
              ?expect(error_response,
                      ?withArgs([400, bad_request, req, #handler{}]),
                      ?andReturn(error))]),

    Actual = scm_hand_servers:from_json(req, #handler{}),

    ?assertEqual(error, Actual),
    ?verifyAll.

from_json_creates_bitbucket_credentials() ->
    EntId = 1,
    EntName = <<"enterprise">>,
    Url = <<"http://stash.my.co:8080">>,
    EncUrl = deliv_web_utils:encode_url(Url),
    UserId = <<"user">>,
    Password = <<"Password">>,
    Ejson = {[{<<"root_api_url">>, Url},
              {<<"user_id">>, UserId},
              {<<"password">>, Password}]},
    Ent = deliv_enterprise:fromlist([{id, EntId},
                                     {name, EntName}]),
    BasicAuth = deliv_basic_auth_application:fromlist([{name, <<"bitbucket">>},
                                                       {root_api_url, Url},
                                                       {user_id, UserId},
                                                       {password, Password},
                                                       {ent_id, EntId}]),
    State = #handler{ent_name = EntName},

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, bitbucket_server]),
                      ?andReturn({Ejson, req1})),
              ?expect(encode_url,
                      ?withArgs([Url]),
                      ?andReturn(EncUrl)),
              ?expect(make_api_url_prefix,
                      ?withArgs([EntName]),
                      ?andReturn(<<"http://api.url/">>)),
              ?expect(validate_url_is_well_formed,
                      ?withArgs([Url]),
                      ?andReturn(ok))]),
    hoax:mock(deliv_enterprise, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, Ent})),
              ?expect(getval,
                      ?withArgs([id, Ent]),
                      ?andReturn(EntId))]),
    hoax:mock(scm_basic_auth,
              ?expect(save_basic_auth_credentials,
                      ?withArgs([Url, UserId, Password, EntId, <<"bitbucket">>]),
                      ?andReturn({ok, BasicAuth}))),
    hoax:mock(scm_bitbucket_rest_api,
              ?expect(check_reachability,
                      ?withArgs([Url, UserId, Password]),
                      ?andReturn({ok, authenticated}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({<<"bitbucket">>, req2_ignored}))),

    Actual = scm_hand_servers:from_json(req, State),

    ?assertEqual({{true, <<"http://api.url/scm/bitbucket/servers/", EncUrl/binary>>}, req1, State}, Actual),
    ?verifyAll.

from_json_returns_400_bad_request_when_url_is_invalid() ->
    EntName = <<"enterprise">>,
    Url = <<"foobear">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    Ejson = {[{<<"root_api_url">>, Url},
              {<<"user_id">>, UserId},
              {<<"password">>, Password}]},
    State = #handler{ent_name = EntName},

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, bitbucket_server]),
                      ?andReturn({Ejson, req1}))]),
    hoax:mock(scm_cred_validation,
             ?expect(validate_auth_creds,
                     ?withArgs([?any, Url, UserId, Password,
                                <<"bitbucket">>, req1, State]),
                     ?andReturn({halt, req1, State}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({<<"bitbucket">>, req2_ignored}))),

    Actual = scm_hand_servers:from_json(req, State),

    ?assertEqual({halt, req1, State}, Actual),
    ?verifyAll.

from_json_returns_400_bad_request_when_url_is_unreachable() ->
    EntName = <<"enterprise">>,
    Url = <<"http://example.com">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    Ejson = {[{<<"root_api_url">>, Url},
              {<<"user_id">>, UserId},
              {<<"password">>, Password}]},
    State = #handler{ent_name = EntName},

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, bitbucket_server]),
                      ?andReturn({Ejson, req1}))]),
    hoax:mock(scm_cred_validation,
              ?expect(validate_auth_creds,
                      ?withArgs([?any, Url, UserId, Password,
                                 <<"bitbucket">>, req1, State]),
                      ?andReturn({halt, req1, State}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({<<"bitbucket">>, req2_ignored}))),

    Actual = scm_hand_servers:from_json(req, State),

    ?assertEqual({halt, req1, State}, Actual),
    ?verifyAll.

from_json_handles_conflict_on_new_bitbucket_credentials() ->
    EntId = 1,
    EntName = <<"enterprise">>,
    Url = <<"http://stash.my.co:8080">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    Ejson = {[{<<"root_api_url">>, Url},
              {<<"user_id">>, UserId},
              {<<"password">>, Password}]},
    Ent = deliv_enterprise:fromlist([{id, EntId},
                                     {name, EntName}]),
    State = #handler{ent_name = EntName},

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, bitbucket_server]),
                      ?andReturn({Ejson, req1})),
              ?expect(error_response,
                      ?withArgs([409, conflict, req1, State]),
                      ?andReturn(error)),
              ?expect(validate_url_is_well_formed,
                      ?withArgs([Url]),
                      ?andReturn(ok))]),
    hoax:mock(deliv_enterprise, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, Ent})),
              ?expect(getval,
                      ?withArgs([id, Ent]),
                      ?andReturn(EntId))]),
    hoax:mock(scm_basic_auth,
              ?expect(save_basic_auth_credentials,
                      ?withArgs([Url, UserId, Password, EntId, <<"bitbucket">>]),
                      ?andReturn({error, conflict}))),
    hoax:mock(scm_bitbucket_rest_api,
              ?expect(check_reachability,
                      ?withArgs([Url, UserId, Password]),
                      ?andReturn({ok, authenticated}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({<<"bitbucket">>, req2_ignored}))),

    Actual = scm_hand_servers:from_json(req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

from_json_handles_other_error_on_new_bitbucket_credentials() ->
    EntId = 1,
    EntName = <<"enterprise">>,
    Url = <<"url">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    Ejson = {[{<<"root_api_url">>, Url},
              {<<"user_id">>, UserId},
              {<<"password">>, Password}]},
    Ent = deliv_enterprise:fromlist([{id, EntId},
                                     {name, EntName}]),

    State = #handler{ent_name = EntName},

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, bitbucket_server]),
                      ?andReturn({Ejson, req1})),
              ?expect(error_response,
                      ?withArgs([500, internal_error, "There was a problem setting up your SCM.", req1, State]),
                      ?andReturn(error)),
              ?expect(validate_url_is_well_formed,
                      ?withArgs([Url]),
                      ?andReturn(ok))]),
    hoax:mock(deliv_enterprise, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, Ent})),
              ?expect(getval,
                      ?withArgs([id, Ent]),
                      ?andReturn(EntId))]),
    hoax:mock(scm_basic_auth,
              ?expect(save_basic_auth_credentials,
                      ?withArgs([Url, UserId, Password, EntId, <<"bitbucket">>]),
                      ?andReturn({error, reason}))),
    hoax:mock(scm_bitbucket_rest_api,
              ?expect(check_reachability,
                      ?withArgs([Url, UserId, Password]),
                      ?andReturn({ok, authenticated}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({<<"bitbucket">>, req2_ignored}))),

    Actual = scm_hand_servers:from_json(req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, "to_json_").

to_json_returns_empty_array_with_no_configured_server() ->
    EntName = <<"enterprise">>,
    State = #handler{ent_name = EntName},

    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([EntName, <<"bitbucket">>]),
                      ?andReturn({error, not_found}))),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([[], req, State]),
                      ?andReturn({body, req1, State}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req]),
                      ?andReturn({<<"bitbucket">>, req2_ignored}))),

    Actual = scm_hand_servers:to_json(req, State),
    ?assertEqual({body, req1, State}, Actual),
    ?verifyAll.

to_json_returns_server_array_with_configured_server() ->
    EntId = 1,
    EntName = <<"enterprise">>,
    Url = <<"http://stash.my.co:8080">>,
    EncUrl = deliv_web_utils:encode_url(Url),
    UserId = <<"user">>,
    Password = <<"Password">>,
    Ejson = {[{<<"root_api_url">>, Url},
              {<<"user_id">>, UserId},
              {<<"_links">>, {[
                  {<<"self">>, {[
                      {<<"href">>, <<"/bitbucket-servers/", EncUrl/binary>>}
                  ]}}
              ]}}
            ]},
    Body = chef_json:encode([Ejson]),
    BasicAuth = deliv_basic_auth_application:fromlist([{name, <<"bitbucket">>},
                                                       {root_api_url, Url},
                                                       {user_id, UserId},
                                                       {password, Password},
                                                       {ent_id, EntId}]),
    State = #handler{ent_name = EntName},

    hoax:mock(scm_basic_auth, [
              ?expect(load_basic_auth_credentials,
                      ?withArgs([EntName, <<"bitbucket">>]),
                      ?andReturn({ok, BasicAuth})),
              ?expect(to_ejson_with_self_hal,
                      ?withArgs([EntName, BasicAuth]),
                      ?andReturn(Ejson))]),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([[Ejson], req, State]),
                      ?andReturn({Body, req1, State}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req]),
                      ?andReturn({<<"bitbucket">>, req2_ignored}))),

    Actual = scm_hand_servers:to_json(req, State),
    ?assertEqual({Body, req1, State}, Actual),
    ?verifyAll.

to_json_returns_500_when_database_errors() ->
    EntName = <<"enterprise">>,
    State = #handler{ent_name = EntName},

    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([EntName, <<"bitbucket">>]),
                      ?andReturn({error, reason}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([500, internal_error, req, State]),
                      ?andReturn(error))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req]),
                      ?andReturn({<<"bitbucket">>, req2_ignored}))),

    Actual = scm_hand_servers:to_json(req, State),
    ?assertEqual(error, Actual),
    ?verifyAll.

%% github tests

to_json_returns_server_array_with_configured_server_github() ->
    EntId = 1,
    EntName = <<"enterprise">>,
    Url = <<"http://my.co:8080">>,
    EncUrl = deliv_web_utils:encode_url(Url),
    UserId = <<"user">>,
    Password = <<"Password">>,
    Ejson = {[{<<"root_api_url">>, Url},
              {<<"user_id">>, UserId},
              {<<"_links">>, {[
                  {<<"self">>, {[
                      {<<"href">>, <<"/github/servers/", EncUrl/binary>>}
                  ]}}
              ]}}
            ]},
    Body = chef_json:encode([Ejson]),
    BasicAuth = deliv_basic_auth_application:fromlist([{name, <<"github">>},
                                                       {root_api_url, Url},
                                                       {user_id, UserId},
                                                       {password, Password},
                                                       {ent_id, EntId}]),
    State = #handler{ent_name = EntName},

    hoax:mock(scm_basic_auth, [
              ?expect(load_basic_auth_credentials,
                      ?withArgs([EntName, <<"github">>]),
                      ?andReturn({ok, BasicAuth})),
              ?expect(to_ejson_with_self_hal,
                      ?withArgs([EntName, BasicAuth]),
                      ?andReturn(Ejson))]),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([[Ejson], req, State]),
                      ?andReturn({Body, req1, State}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req]),
                      ?andReturn({<<"github">>, req2_ignored}))),

    Actual = scm_hand_servers:to_json(req, State),
    ?assertEqual({Body, req1, State}, Actual),
    ?verifyAll.
