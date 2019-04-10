-module(deliv_github_ui_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

hoax_fixture_test_ () ->
    hoax:fixture(?MODULE).

init_subcribes_to_correct_events() ->
    Events = [build_event_for_change,
              github_pull_request_opened,
              github_pull_request_synchronize,
              github_comment_created],

    hoax:mock(deliv_event,
              ?expect(subscribe,
                      ?withArgs([Events]),
                      ?andReturn(true))),
    deliv_github_ui:init([]),
    ?verifyAll.

handle_github_pull_request_opened() ->
    Ent = <<"Ent">>,
    Org = <<"Org">>,
    Proj = <<"Proj">>,
    ChangeId = <<"ChangeId">>,
    Payload = ct_github:load_payload_from_disk("pull_request_opened.json"),
    PRNum = deliv_github_pull_request:pull_request_number(Payload),
    MockScope = deliv_scopes:'#new_common'([{ent_name, Ent},
                                            {org_name, Org},
                                            {proj_name, Proj},
                                            {scoping_names, [Ent, Org, Proj, <<"Pipe">>]},
                                            {change_id, ChangeId},
                                            {scm_module, deliv_scm_local}]),

    hoax:mock(deliv_scopes,
              [?expect(from_change_id,
                      ?withArgs([ChangeId]),
                      ?andReturn(MockScope)),
               ?expect('#get',
                       ?withArgs([change_id, MockScope]),
                       ?andReturn(ChangeId))]),
    hoax:mock(deliv_github_api,
              ?expect(add_label_to_issue,
                      ?withArgs([MockScope, PRNum, <<"Delivery Change Created">>]),
                      ?andReturn(ok))),
    hoax:mock(deliv_github_status,
              ?expect(sync_status,
                      ?withArgs([MockScope, ChangeId]),
                      ?andReturn(ok))),
    deliv_github_ui:handle_info({self(), github_pull_request_opened,
                                      {<<"ChangeId">>, Payload}}, undefined),
    ?verifyAll.

handle_github_pull_request_synchronize() ->
    Ent = <<"Ent">>,
    Org = <<"Org">>,
    Proj = <<"Proj">>,
    ChangeId = <<"ChangeId">>,
    Payload = ct_github:load_payload_from_disk("pull_request_synchronize.json"),
    MockScope = deliv_scopes:'#new_common'([{ent_name, Ent},
                                            {org_name, Org},
                                            {proj_name, Proj},
                                            {scoping_names, [Ent, Org, Proj, <<"Pipe">>]},
                                            {change_id, ChangeId},
                                            {scm_module, deliv_scm_local}]),

    hoax:mock(deliv_scopes,
              ?expect(from_change_id,
                      ?withArgs([ChangeId]),
                      ?andReturn(MockScope))),
    hoax:mock(deliv_github_status,
              ?expect(sync_status,
                      ?withArgs([MockScope, ChangeId]),
                      ?andReturn(ok))),
    deliv_github_ui:handle_info({self(), github_pull_request_synchronize,
                                      {ChangeId, Payload}}, undefined),
    ?verifyAll.

handle_github_build_event_for_change_syncs_comments_and_status() ->
    Ent = <<"Ent">>,
    Org = <<"Org">>,
    Proj = <<"Proj">>,
    ChangeId = <<"ChangeId">>,
    MockChange0 = deliv_change:'#new'(),
    MockChange = deliv_change:setvals([{id, ChangeId}], MockChange0),
    MockScope = deliv_scopes:'#new_common'([{ent_name, Ent},
                                            {org_name, Org},
                                            {proj_name, Proj},
                                            {scoping_names, [Ent, Org, Proj, <<"Pipe">>]},
                                            {change_id, ChangeId},
                                            {scm_module, deliv_scm_local}]),
    hoax:mock(deliv_github_status_comment,
              ?expect(sync_comment,
                      ?withArgs([MockScope, ChangeId]))),
    hoax:mock(deliv_github_status,
              ?expect(sync_status,
                      ?withArgs([MockScope, ChangeId]),
                      ?andReturn(ok))),
    hoax:mock(deliv_github_chatops,
              ?expect(handle_build_event_for_change,
                      ?withArgs([MockScope]),
                      ?andReturn(ok))),
    hoax:mock(deliv_change,
              ?expect(getval,
                      ?withArgs([id, MockChange]),
                      ?andReturn(ChangeId))),
    hoax:mock(deliv_scopes,
              [?expect(from_change_id,
                       ?withArgs([ChangeId]),
                       ?andReturn(MockScope)),
               ?expect('#get',
                       ?withArgs([scm_module, MockScope]),
                       ?andReturn(deliv_scm_github))]),
    deliv_github_ui:handle_info({self(), build_event_for_change,
                                MockChange}, undefined),
    ?verifyAll.
