-module(deliv_github_status_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_ () ->
    hoax:fixture(?MODULE).

sync_status_updates_github_statuses() ->
    Ent = <<"Ent">>,
    Org = <<"Org">>,
    Proj = <<"Proj">>,
    ChangeId = <<"mocked_change_id">>,
    PatchsetId = <<"patchset_id">>,
    Patchset = deliv_patchset:'#new'(),
    GHPatchset = deliv_github_patchset:'#new'(),
    GHPayload = ct_github:load_raw_payload_from_disk("pull_request_opened.json"),
    GHSha = deliv_github_pull_request:commit_sha(chef_json:decode(GHPayload)),
    PhaseRunSummary = eu_test_helpers:phase_run_summary_example(),
    Scope = deliv_scopes:'#new_common'([{ent_name, Ent},
                                        {org_name, Org},
                                        {proj_name, Proj},
                                        {scoping_names, [Ent, Org, Proj, <<"Pipe">>]},
                                        {change_id, ChangeId}]),

    hoax:mock(deliv_patchset,
              [?expect(latest_patchset_for_change,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, Patchset})),
              ?expect(getval,
                      ?withArgs([id, Patchset]),
                      ?andReturn(PatchsetId))]),
    hoax:mock(deliv_github_patchset,
              [?expect(fetch_by_patchset_id,
                      ?withArgs([PatchsetId]),
                      ?andReturn([GHPatchset])),
               ?expect(getval,
                       ?withArgs([payload, GHPatchset]),
                       ?andReturn(GHPayload))]),
    hoax:mock(deliv_change,
              ?expect(get_phase_run_summary,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, PhaseRunSummary}))),
    hoax:mock(deliv_github_api,
              [?expect(set_pull_request_status,
                       ?withArgs([Scope, GHSha, success,
                                 <<"Delivery phase passed">>,
                                 <<"http://callback.co/status/verify">>,
                                 <<"chef_delivery/verify/unit">>]),
                       ?andReturn(ok)),
               ?expect(set_pull_request_status,
                       ?withArgs([Scope, GHSha, success,
                                  <<"Delivery phase passed">>,
                                  <<"http://callback.co/status/verify">>,
                                  <<"chef_delivery/verify/lint">>]),
                       ?andReturn(ok)),
               ?expect(set_pull_request_status,
                       ?withArgs([Scope, GHSha, success,
                                  <<"Delivery phase passed">>,
                                  <<"http://callback.co/status/verify">>,
                                  <<"chef_delivery/verify/syntax">>]),
                       ?andReturn(ok)),
               ?expect(set_pull_request_status,
                       ?withArgs([Scope, GHSha, success,
                                  <<"Delivery phase passed">>,
                                  <<"http://callback.co/status/build">>,
                                  <<"chef_delivery/build/unit">>]),
                       ?andReturn(ok)),
               ?expect(set_pull_request_status,
                       ?withArgs([Scope, GHSha, success,
                                  <<"Delivery phase passed">>,
                                  <<"http://callback.co/status/build">>,
                                  <<"chef_delivery/build/lint">>]),
                       ?andReturn(ok)),
               ?expect(set_pull_request_status,
                       ?withArgs([Scope, GHSha, success,
                                  <<"Delivery phase passed">>,
                                  <<"http://callback.co/status/build">>,
                                  <<"chef_delivery/build/syntax">>]),
                       ?andReturn(ok)),
               ?expect(set_pull_request_status,
                       ?withArgs([Scope, GHSha, success,
                                  <<"Delivery phase passed">>,
                                  <<"http://callback.co/status/build">>,
                                  <<"chef_delivery/build/security">>]),
                       ?andReturn(ok)),
               ?expect(set_pull_request_status,
                       ?withArgs([Scope, GHSha, success,
                                  <<"Delivery phase passed">>,
                                  <<"http://callback.co/status/build">>,
                                  <<"chef_delivery/build/quality">>]),
                       ?andReturn(ok)),
               ?expect(set_pull_request_status,
                       ?withArgs([Scope, GHSha, success,
                                  <<"Delivery phase passed">>,
                                  <<"http://callback.co/status/build">>,
                                  <<"chef_delivery/build/publish">>]),
                       ?andReturn(ok))]),
    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([Ent, Org, Proj, ChangeId]),
                      ?andReturn(<<"http://callback.co">>))),

    deliv_github_status:sync_status(Scope, ChangeId),
    ?verifyAll.
