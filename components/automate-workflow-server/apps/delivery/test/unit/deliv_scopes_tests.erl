-module(deliv_scopes_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

-define(SCOPING_NAMES, [<<"Ent">>, <<"Org">>, <<"Proj">>, <<"Pipe">>]).
-define(PROJECT_ID, 42).

scope_from_test_() ->
    hoax:fixture(?MODULE, "scope_from_", scope_from_setup, scope_from_teardown).

scope_from_setup() ->
    hoax:mock(deliv_change,
              ?expect(scoping_names,
                      ?withArgs([<<"ChangeId">>]),
                      ?andReturn(?SCOPING_NAMES))),
    ProjectRecord = deliv_project:'#new'(),
    hoax:mock(deliv_project, [
                              ?expect(fetch,
                                      ?withArgs([<<"Ent">>, <<"Org">>, <<"Proj">>]),
                                      ?andReturn({ok, ProjectRecord})),
                              ?expect(getval,
                                      ?withArgs([id, ProjectRecord]),
                                      ?andReturn(?PROJECT_ID)),
                              ?expect(getval,
                                      ?withArgs([scm_module, ProjectRecord]),
                                      ?andReturn(<<"deliv_scm_github">>))
                             ]).

scope_from_teardown(_) ->
    ok.

scope_from_change_id() ->
    Record = deliv_scopes:from_change_id(<<"ChangeId">>),
    assert_common_record_values(Record),
    ?verifyAll.

scope_from_change() ->
    hoax:mock(deliv_change, ?expect(getval,
                                    ?withArgs([id, change]),
                                    ?andReturn(<<"ChangeId">>))),
    Record = deliv_scopes:from_change(change),
    assert_common_record_values(Record),
    ?verifyAll.

scope_from_patchset() ->
    Patchset = deliv_patchset:'#new'(),
    hoax:mock(deliv_change,
              ?expect(scoping_names,
                      ?withArgs([<<"ChangeId">>]),
                      ?andReturn(?SCOPING_NAMES))),
    hoax:mock(deliv_patchset,
              ?expect(getval,
                      ?withArgs([change_id, Patchset]),
                      ?andReturn(<<"ChangeId">>))),
    Record = deliv_scopes:from_patchset(Patchset),
    assert_common_record_values(Record),
    ?verifyAll.

assert_common_record_values(Record) ->
    ?assertEqual([<<"Ent">>, <<"Org">>, <<"Proj">>, <<"ChangeId">>, <<"Pipe">>, ?PROJECT_ID, ?SCOPING_NAMES, deliv_scm_github],
                 deliv_scopes:'#get'([ent_name, org_name, proj_name, change_id, pipe_name, proj_id, scoping_names, scm_module], Record)).

scope_from_coords() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,
    Coords = #proj_coordinates{ent_name=EntName, org_name=OrgName, proj_name=ProjName},

    Record = deliv_scopes:from_coords(Coords),
    ?assertEqual([EntName, OrgName, ProjName, [<<"Ent">>, <<"Org">>, <<"Proj">>, undefined], deliv_scm_github],
                 deliv_scopes:'#get'([ent_name, org_name, proj_name, scoping_names, scm_module], Record)).
