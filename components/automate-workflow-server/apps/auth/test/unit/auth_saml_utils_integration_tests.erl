-module(auth_saml_utils_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("esaml/include/esaml.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    [
     hoax:fixture(?MODULE, "duplicate_assertion_check_", setup, teardown)
    ].

setup() ->
    {ok, Pid} = auth_saml_assertion_timer:start_link(),
    Pid.

teardown(_Pid) ->
    auth_saml_assertion_timer:stop(),
    ok.

duplicate_assertion_check_when_assertion_was_already_used_returns_error() ->
    Assertion = #esaml_assertion{},
    Digest = <<"neverseenmebefore">>,
    Now  = 63633975360, % 2016-06-24, 08:16:00
    Soon = 63633975369, % 2016-06-24, 08:16:09
    hoax:mock(esaml,
              ?expect(stale_time,
                      ?withArgs([Assertion]),
                      ?andReturn(Soon))),
    hoax:mock(calendar, [
              ?expect(now_to_datetime,
                      ?withArgs([?any]), %  do this to make time predictable (can't mock os:timestamp/0)
                      ?andReturn(date_time_of_now)),
              ?expect(datetime_to_gregorian_seconds,
                      ?withArgs([date_time_of_now]),
                      ?andReturn(Now))]),

    Actual = auth_saml_utils:duplicate_assertion_check(Assertion, Digest),
    ?assertEqual(ok, Actual),
    Stored = auth_saml_utils:duplicate_assertion_check(Assertion, Digest),
    ?assertEqual({error, duplicate_assertion}, Stored),
    ?verifyAll.

duplicate_assertion_check_when_called_from_validate_assertions_does_correctly_identify_duplicate_assertions() ->
    Assertion = #esaml_assertion{},
    Digest = <<"hashsum">>,
    ConsumeUri = <<"https://delivery.com/saml/consume">>,
    EntityId = <<"https://delivery.com/saml">>,
    SP = #esaml_sp{idp_signs_envelopes = false,
                   idp_signs_assertions = false,
                   consume_uri = ConsumeUri,
                   metadata_uri = EntityId
                  },
    Assertion = #esaml_assertion{},
    Now  = 63633975360, % 2016-06-24, 08:16:00
    Soon = 63633975369, % 2016-06-24, 08:16:09
    Ns = [{"samlp", 'urn:oasis:names:tc:SAML:2.0:protocol'},
          {"saml", 'urn:oasis:names:tc:SAML:2.0:assertion'}],
    DuplicateFun = fun auth_saml_utils:duplicate_assertion_check/2,
    CheckKnownIdFun = fun auth_saml_assertion_timer:check_known_id/1,
    hoax:mock(xmerl_xpath,
              ?expect(string,
                      ?withArgs(["/samlp:Response/saml:Assertion", xml, [{namespace, Ns}]]),
                      ?andReturn([assertion_xml]))),
    hoax:mock(esaml,
              ?expect(validate_assertion,
                      ?withArgs([assertion_xml, ConsumeUri, EntityId, CheckKnownIdFun]),
                      ?andReturn({ok, Assertion}))),
    hoax:mock(xmerl_dsig,
              ?expect(digest,
                      ?withArgs([xml]),
                      ?andReturn(Digest))),
    hoax:mock(esaml,
              ?expect(stale_time,
                      ?withArgs([Assertion]),
                      ?andReturn(Soon))),
    hoax:mock(calendar, [
              ?expect(now_to_datetime,
                      ?withArgs([?any]),
                      ?andReturn(date_time_of_now)),
              ?expect(datetime_to_gregorian_seconds,
                      ?withArgs([date_time_of_now]),
                      ?andReturn(Now))]),

    ActualFirst = esaml_sp:validate_assertion(xml, DuplicateFun, CheckKnownIdFun, SP),
    ?assertEqual({ok, Assertion}, ActualFirst),

    ActualSecond = esaml_sp:validate_assertion(xml, DuplicateFun, CheckKnownIdFun, SP),
    ?assertEqual({error, duplicate}, ActualSecond),
    ?verifyAll.
