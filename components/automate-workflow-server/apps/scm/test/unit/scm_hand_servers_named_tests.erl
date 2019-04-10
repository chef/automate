-module(scm_hand_servers_named_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

scm_hand_servers_named_test_() ->
    [
    hoax:fixture(?MODULE, "content_types_provided_"),
    hoax:fixture(?MODULE, "content_types_accepted_"),
    hoax:fixture(?MODULE, "delete_resource_"),
    hoax:fixture(?MODULE, "from_json_"),
    hoax:fixture(?MODULE, "update_auth_creds")
    ].

init_returns_correct_tuple_test() ->
    Actual = scm_hand_servers_named:init(ignored, req, #handler{}),

    Expected = {upgrade, protocol, cowboy_rest, req, #handler{}},

    ?assertEqual(Expected, Actual).

rest_init_returns_correct_tuple_test() ->
    Actual = scm_hand_servers_named:rest_init(req, #handler{}),

    Expected = {ok, req, #handler{}},

    ?assertEqual(Expected, Actual).

allowed_methods_allows_put_delete_test() ->
    ?assertEqual({[<<"PUT">>, <<"DELETE">>], req, #handler{}},
                 scm_hand_servers_named:allowed_methods(req, #handler{})).

content_types_accepted_provides_json() ->
    hoax:mock(deliv_web_utils,
              ?expect(content_type_json_map,
                      ?withArgs([from_json]),
                      ?andReturn(expected_map))),

    Actual = scm_hand_servers_named:content_types_accepted(req, #handler{}),

    ?assertEqual({expected_map, req, #handler{}}, Actual),
    ?verifyAll.

delete_resource_returns_true_when_delete_is_successful() ->
    EntName = <<"enterprise">>,
    Url = <<"http://stash.my.co:8080">>,
    State = #handler{ent_name = EntName},
    ScmType = <<"bitbucket">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([server_url, req]),
                      ?andReturn({Url, req1}))),
    hoax:mock(scm_basic_auth,
              ?expect(delete_basic_auth_credentials,
                      ?withArgs([EntName, Url, ScmType]),
                      ?andReturn(ok))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({ScmType, req2}))),


    Actual = scm_hand_servers_named:delete_resource(req, State),
    ?assertEqual({true, req2, State}, Actual),
    ?verifyAll.

delete_resource_returns_404_when_basic_auth_creds_are_not_found() ->
    EntName = <<"enterprise">>,
    Url = <<"http://stash.my.co:8080">>,
    State = #handler{ent_name = EntName},
    ScmType = <<"bitbucket">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([server_url, req]),
                      ?andReturn({Url, req1}))),
    hoax:mock(scm_basic_auth,
              ?expect(delete_basic_auth_credentials,
                      ?withArgs([EntName, Url, ScmType]),
                      ?andReturn({error, not_found}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({ScmType, req2}))),

    ResponseMsg = <<"The Bitbucket SCM link you're trying to remove could not be found.">>,
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([404, not_found, ResponseMsg, req2, State]),
                      ?andReturn(error))),

    Actual = scm_hand_servers_named:delete_resource(req, State),
    ?assertEqual(error, Actual),
    ?verifyAll.

delete_resource_returns_412_when_there_are_bitbucket_projects() ->
    EntName = <<"enterprise">>,
    Url = <<"http://stash.my.co:8080">>,
    State = #handler{ent_name = EntName},
    ScmType = <<"bitbucket">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([server_url, req]),
                      ?andReturn({Url, req1}))),
    hoax:mock(scm_basic_auth,
              ?expect(delete_basic_auth_credentials,
                      ?withArgs([EntName, Url, ScmType]),
                      ?andReturn({error, projects_exist}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({ScmType, req2}))),

    ResponseMsg = <<"Some Automate projects are connected to Bitbucket ",
                    "repositories. Please remove those links before removing ",
                    "the SCM link to Bitbucket.">>,
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([412, precondition_failed, ResponseMsg, req2, State]),
                      ?andReturn(error))),

    Actual = scm_hand_servers_named:delete_resource(req, State),
    ?assertEqual(error, Actual),
    ?verifyAll.

delete_resource_returns_500_when_there_is_an_error_deleting_basic_auth_creds() ->
    EntName = <<"enterprise">>,
    Url = <<"http://stash.my.co:8080">>,
    State = #handler{ent_name = EntName},
    ScmType = <<"bitbucket">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([server_url, req]),
                      ?andReturn({Url, req1}))),
    hoax:mock(scm_basic_auth,
              ?expect(delete_basic_auth_credentials,
                      ?withArgs([EntName, Url, ScmType]),
                      ?andReturn({error, other_error}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({ScmType, req2}))),

    ResponseMsg = <<"There was a problem removing the Bitbucket SCM link.">>,
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, ResponseMsg, req2, State]),
                      ?andReturn(error))),

    Actual = scm_hand_servers_named:delete_resource(req, State),
    ?assertEqual(error, Actual),
    ?verifyAll.

from_json_returns_error_with_bad_request() ->
    State = #handler{},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, bitbucket_server) -> {{error, reason}, req1};
                    deliv_web_utils:error_response(400, bad_request, req1, State) -> error
                end),

    Actual = scm_hand_servers_named:from_json(req, State),

    ?assertEqual(error, Actual),
    ?verifyAll.

from_json_calls_validate_auth_creds_with_the_update_auth_cred_function() ->
    EntName = <<"enterprise">>,
    Url = <<"http://stash.my.co:8080">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    Ejson = {[{<<"root_api_url">>, Url},
              {<<"user_id">>, UserId},
              {<<"password">>, Password}]},
    ScmType = <<"bitbucket">>,
    State = #handler{ent_name = EntName},

    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, bitbucket_server) -> {Ejson, req1};
                    cowboy_req:binding(scm_type, req1) -> {ScmType, req2};
                    scm_cred_validation:validate_auth_creds(fun scm_hand_servers_named:update_auth_credentials/5, Url, UserId, Password, ScmType, req2, State) -> {true, req2, State}
                end),

    Actual = scm_hand_servers_named:from_json(req, State),

    ?assertEqual({true, req2, State}, Actual),
    ?verifyAll.

update_auth_creds_responds_with_error_when_bitbucket_auth_creds_not_found() ->
    EntId = 1,
    EntName = <<"enterprise">>,
    Url = <<"http://stash.my.co:8080">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    ScmType = <<"bitbucket">>,
    Ent = deliv_enterprise:fromlist([{id, EntId},
                                     {name, EntName}]),
    State = #handler{ent_name = EntName},

    hoax:expect(receive
                    deliv_enterprise:fetch(EntName) -> {ok, Ent};
                    deliv_enterprise:getval(id, Ent) -> EntId;
                    cowboy_req:binding(scm_type, req) -> {ScmType, req1};
                    scm_basic_auth:update_basic_auth_credentials(Url, UserId,
                                                                 Password, EntId,
                                                                 ScmType) -> {error, not_found};
                    deliv_web_utils:error_response(404, not_found, req1, State) -> error
                end),

    Actual = scm_hand_servers_named:update_auth_credentials(Url, UserId,
                                                            Password, req,
                                                            State),

    ?assertEqual(error, Actual),
    ?verifyAll.

update_auth_creds_handles_other_error_on_update_bitbucket_credentials() ->
    EntId = 1,
    EntName = <<"enterprise">>,
    Url = <<"url">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    ScmType = <<"bitbucket">>,
    Ent = deliv_enterprise:fromlist([{id, EntId},
                                     {name, EntName}]),

    State = #handler{ent_name = EntName},

    hoax:expect(receive
                    deliv_enterprise:fetch(EntName) -> {ok, Ent};
                    deliv_enterprise:getval(id, Ent) -> EntId;
                    cowboy_req:binding(scm_type, req) -> {ScmType, req1};
                    scm_basic_auth:update_basic_auth_credentials(Url, UserId,
                                                                 Password, EntId,
                                                                 ScmType) -> {error, reason};
                    deliv_web_utils:error_response(500, internal_error, "There was a problem updating your SCM.", req1, State) -> error
                end),

    Actual = scm_hand_servers_named:update_auth_credentials(Url, UserId,
                                                            Password, req,
                                                            State),

    ?assertEqual(error, Actual),
    ?verifyAll.

update_auth_creds_handles_enterprise_not_found() ->
    EntName = <<"enterprise">>,
    Url = <<"http://stash.my.co:8080">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    State = #handler{ent_name = EntName},

    hoax:expect(receive
                    deliv_enterprise:fetch(EntName) -> {error, not_found};
                    deliv_web_utils:error_response(404, not_found, req, State) -> error
                end),

    Actual = scm_hand_servers_named:update_auth_credentials(Url, UserId,
                                                            Password, req,
                                                            State),

    ?assertEqual(error, Actual),
    ?verifyAll.

update_auth_creds_handles_enterprise_other_error() ->
    EntName = <<"enterprise">>,
    Url = <<"http://stash.my.co:8080">>,
    UserId = <<"user">>,
    Password = <<"Password">>,
    State = #handler{ent_name = EntName},

    hoax:expect(receive
                    deliv_enterprise:fetch(EntName) -> {error, whateverrrr};
                    deliv_web_utils:error_response(500, internal_error, req, State) -> error
                end),

    Actual = scm_hand_servers_named:update_auth_credentials(Url, UserId,
                                                            Password, req,
                                                            State),

    ?assertEqual(error, Actual),
    ?verifyAll.

%% github tests

from_json_creates_github_credentials() ->
    EntName = <<"enterprise">>,
    Url = <<"http://github.my.co:8080">>,
    UserId = <<"user">>,
    Token = <<"t0k3n">>,
    Ejson = {[{<<"root_api_url">>, Url},
              {<<"user_id">>, UserId},
              {<<"password">>, Token}]},
    ScmType = <<"github">>,

    State = #handler{ent_name = EntName},
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, bitbucket_server) -> {Ejson, req1};
                    cowboy_req:binding(scm_type, req1) -> {ScmType, req2};
                    scm_cred_validation:validate_auth_creds(fun scm_hand_servers_named:update_auth_credentials/5,
                               Url, UserId, Token, ScmType, req2, State) -> {true, req3, State}
                end),

    Actual = scm_hand_servers_named:from_json(req, State),

    ?assertEqual({true, req3, State}, Actual),
    ?verifyAll.

delete_resource_returns_true_when_delete_is_successful_github() ->
    EntName = <<"enterprise">>,
    Url = <<"http://github.my.co:8080">>,
    State = #handler{ent_name = EntName},
    ScmType = <<"github">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([server_url, req]),
                      ?andReturn({Url, req1}))),
    hoax:mock(scm_basic_auth,
              ?expect(delete_basic_auth_credentials,
                      ?withArgs([EntName, Url, ScmType]),
                      ?andReturn(ok))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({ScmType, req2}))),

    Actual = scm_hand_servers_named:delete_resource(req, State),
    ?assertEqual({true, req2, State}, Actual),
    ?verifyAll.

delete_resource_returns_404_when_basic_auth_creds_are_not_found_github() ->
    EntName = <<"enterprise">>,
    Url = <<"http://github.my.co:8080">>,
    State = #handler{ent_name = EntName},
    ScmType = <<"github">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([server_url, req]),
                      ?andReturn({Url, req1}))),
    hoax:mock(scm_basic_auth,
              ?expect(delete_basic_auth_credentials,
                      ?withArgs([EntName, Url, ScmType]),
                      ?andReturn({error, not_found}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({ScmType, req2}))),

    ResponseMsg = <<"The GitHub SCM link you're trying to remove could not be found.">>,
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([404, not_found, ResponseMsg, req2, State]),
                      ?andReturn(error))),

    Actual = scm_hand_servers_named:delete_resource(req, State),
    ?assertEqual(error, Actual),
    ?verifyAll.

delete_resource_returns_412_when_there_are_bitbucket_projects_github() ->
    EntName = <<"enterprise">>,
    Url = <<"http://github.my.co:8080">>,
    State = #handler{ent_name = EntName},
    ScmType = <<"github">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([server_url, req]),
                      ?andReturn({Url, req1}))),
    hoax:mock(scm_basic_auth,
              ?expect(delete_basic_auth_credentials,
                      ?withArgs([EntName, Url, ScmType]),
                      ?andReturn({error, projects_exist}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({ScmType, req2}))),

    ResponseMsg = <<"Some Automate projects are connected to GitHub ",
                    "repositories. Please remove those links before removing ",
                    "the SCM link to GitHub.">>,
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([412, precondition_failed, ResponseMsg, req2, State]),
                      ?andReturn(error))),

    Actual = scm_hand_servers_named:delete_resource(req, State),
    ?assertEqual(error, Actual),
    ?verifyAll.

delete_resource_returns_500_when_there_is_an_error_deleting_basic_auth_creds_github() ->
    EntName = <<"enterprise">>,
    Url = <<"http://github.my.co:8080">>,
    State = #handler{ent_name = EntName},
    ScmType = <<"github">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([server_url, req]),
                      ?andReturn({Url, req1}))),
    hoax:mock(scm_basic_auth,
              ?expect(delete_basic_auth_credentials,
                      ?withArgs([EntName, Url, ScmType]),
                      ?andReturn({error, other_error}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([scm_type, req1]),
                      ?andReturn({ScmType, req2}))),

    ResponseMsg = <<"There was a problem removing the GitHub SCM link.">>,
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, ResponseMsg, req2, State]),
                      ?andReturn(error))),

    Actual = scm_hand_servers_named:delete_resource(req, State),
    ?assertEqual(error, Actual),
    ?verifyAll.
