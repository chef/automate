-module(deliv_change_auth_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

authorized_change_action_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "authorized_change_action", setup,
        teardown).

setup() ->
    [
        <<"ent_name">>,
        <<"deliv_user">>,
        <<"org_name">>,
        <<"proj_name">>,
        <<"pipe_name">>
    ].

teardown(_) ->
    ok.

validate_roles_for_pipeline_action(Params, Action, ExpectedRoles) ->
    validate_scope_and_roles(Params, Action, Params, pipeline, ExpectedRoles).

validate_roles_for_enterprise_action(Params, Action, ExpectedRoles) ->
    [EntName, UserName | _] = Params,
    ScopedParams = [EntName, UserName],
    validate_scope_and_roles(Params, Action, ScopedParams, enterprise, ExpectedRoles).

validate_scope_and_roles(Params, Action, ScopedParams, ExpectedScope, ExpectedRoles) ->
    [EntName, UserName, OrgName, ProjName, PipeName] = Params,

    hoax:mock(deliv_user,
              ?expect(effective_roles,
                      ?withArgs([ExpectedScope, ScopedParams]),
                      ?andReturn({ok, user_roles}))),
    hoax:mock(deliv_authz,
              ?expect(roles_match,
                      ?withArgs([user_roles, ExpectedRoles]),
                      ?andReturn(allow))),

    Result = deliv_change_auth:authorized_change_action(EntName, OrgName,
        ProjName, PipeName, UserName, Action),
    ?assertEqual(allow, Result),
    ?verifyAll.

authorized_change_action_trigger_verify_scope_and_roles(Params) ->
    validate_roles_for_pipeline_action(
        Params,
        trigger_verify,
        [<<"admin">>, <<"reviewer">>, <<"committer">>]
    ).

authorized_change_action_delete_scope_and_roles(Params) ->
    validate_roles_for_pipeline_action(
        Params,
        delete,
        [<<"admin">>, <<"reviewer">>, <<"committer">>]
    ).

authorized_change_action_approve_scope_and_roles(Params) ->
    validate_roles_for_pipeline_action(
        Params,
        approve,
        [<<"admin">>, <<"reviewer">>]
    ).

authorized_change_action_trigger_build_scope_and_roles(Params) ->
    validate_roles_for_pipeline_action(
        Params,
        trigger_build,
        [<<"admin">>, <<"reviewer">>]
    ).

authorized_change_action_trigger_acceptance_scope_and_roles(Params) ->
    validate_roles_for_pipeline_action(
        Params,
        trigger_acceptance,
        [<<"admin">>, <<"reviewer">>]
    ).

authorized_change_action_deliver_scope_and_roles(Params) ->
    validate_roles_for_pipeline_action(
        Params,
        deliver,
        [<<"admin">>, <<"shipper">>]
    ).

authorized_change_action_trigger_union_scope_and_roles(Params) ->
    validate_roles_for_enterprise_action(
        Params,
        trigger_union,
        [<<"admin">>, <<"shipper">>]
    ).

authorized_change_action_trigger_rehearsal_scope_and_roles(Params) ->
    validate_roles_for_enterprise_action(
        Params,
        trigger_rehearsal,
        [<<"admin">>, <<"shipper">>]
    ).

authorized_change_action_trigger_delivered_scope_and_roles(Params) ->
    validate_roles_for_enterprise_action(
        Params,
        trigger_delivered,
        [<<"admin">>, <<"shipper">>]
    ).

authorized_change_action_forbids_when_user_effective_roles_errors(Params) ->
    [EntName, UserName, OrgName, ProjName, PipeName] = Params,
    Action = approve,
    Scope = pipeline,

    hoax:mock(deliv_user,
              ?expect(effective_roles,
                      ?withArgs([Scope, Params]),
                      ?andReturn({error, no_effective_roles}))),

    Result = deliv_change_auth:authorized_change_action(EntName, OrgName,
        ProjName, PipeName, UserName, Action),
    ?assertEqual(forbid, Result),
    ?verifyAll.
