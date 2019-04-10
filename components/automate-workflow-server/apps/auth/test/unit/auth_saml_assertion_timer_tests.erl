-module(auth_saml_assertion_timer_tests).

-include_lib("auth_types.hrl").
-include_lib("hoax/include/hoax.hrl").
-include_lib("esaml/include/esaml.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    [
     hoax:fixture(?MODULE, "check_and_set_digest_", setup, teardown),
     hoax:fixture(?MODULE, "check_known_id_", setup, teardown)
    ].

setup() ->
    {ok, Pid} = auth_saml_assertion_timer:start_link(),
    Pid.

teardown(_Pid) ->
    auth_saml_assertion_timer:stop(),
    ok.

check_and_set_digest_when_asked_twice_with_the_same_digest_returns_true() ->
    Digest = <<"hashsum">>,
    AnotherDigest = <<"hashsum2">>,
    Remember = 10*1000,
    ?assertEqual(false, auth_saml_assertion_timer:check_and_set_digest(Digest, Remember)),
    ?assertEqual(false, auth_saml_assertion_timer:check_and_set_digest(AnotherDigest, Remember)),
    ?assertEqual(true, auth_saml_assertion_timer:check_and_set_digest(Digest, Remember-1)),
    ?assertEqual(true, auth_saml_assertion_timer:check_and_set_digest(AnotherDigest, Remember-1)).

check_and_set_digest_when_timer_is_up_forgets_digest() ->
    Digest = <<"hashsum">>,
    AnotherDigest = <<"hashsum2">>,
    Remember = 10,
    ?assertEqual(false, auth_saml_assertion_timer:check_and_set_digest(Digest, Remember)),
    ?assertEqual(false, auth_saml_assertion_timer:check_and_set_digest(AnotherDigest, Remember)),
    timer:sleep(20),
    ?assertEqual(false, auth_saml_assertion_timer:check_and_set_digest(Digest, Remember)),
    ?assertEqual(false, auth_saml_assertion_timer:check_and_set_digest(AnotherDigest, Remember)).

check_known_id_returns_true_when_id_remembered()->
    Id = <<"some_id">>,
    hoax:mock(esaml_util,
              ?expect(unique_id,
                      ?withArgs([]),
                      ?andReturn(Id))),

    ReturnUd = auth_saml_assertion_timer:generate_and_track_unique_id(),
    ?assertEqual(Id, ReturnUd),
    ?assertEqual(true, auth_saml_assertion_timer:check_known_id(Id)).

check_known_id_returns_false_when_id_not_remembered()->
    Id = <<"some_unknown_id">>,
    ?assertEqual(false, auth_saml_assertion_timer:check_known_id(Id)).

check_known_id_cleans_up_id_after_use()->
    Id = <<"another_id">>,
    hoax:mock(esaml_util,
              ?expect(unique_id,
                      ?withArgs([]),
                      ?andReturn(Id))),

    auth_saml_assertion_timer:generate_and_track_unique_id(),
    ?assertEqual(true, auth_saml_assertion_timer:check_known_id(Id)),
    timer:sleep(10),
    ?assertEqual(false, auth_saml_assertion_timer:check_known_id(Id)).
