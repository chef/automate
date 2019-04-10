-module(audit_hand_audit_tests).

-include_lib("hoax/include/hoax.hrl").

-include("audit_events.hrl").

-compile([export_all]).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

content_types_provided_accepts_json() ->
    ExpectedContentType = {[{{<<"application">>, <<"json">>, '*'}, to_json}], request, state},

    ?assertEqual(ExpectedContentType, audit_hand_audit:content_types_provided(request, state)).

allowed_methods_returns_get() ->
    ?assertEqual({[<<"GET">>], request, state}, audit_hand_audit:allowed_methods(request, state)).

to_json_retrieves_state_from_audit_subscriptions() ->
    Payload = [#audit_stage_event{ stage_name = verify }],

    ExpectedJson = json,

    hoax:mock(audit_subscriptions,
              ?expect(audit_log,
                      ?withArgs([]),
                      ?andReturn(Payload))),

    hoax:mock(audit_web_utils,
              ?expect(to_json,
                      ?withArgs([Payload]),
                      ?andReturn(ExpectedJson))),


    ?assertEqual({json, request, state}, audit_hand_audit:to_json(request, state)),

    ?verifyAll.
