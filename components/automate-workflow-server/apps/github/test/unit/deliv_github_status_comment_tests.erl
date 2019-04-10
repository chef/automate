-module(deliv_github_status_comment_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

hoax_fixture_test_ () ->
    hoax:fixture(?MODULE).

sync_comment_creates_new_comment() ->
    Scope = deliv_scopes:'#new_common'([{ent_name, <<"Ent">>},
                                        {org_name, <<"Org">>},
                                        {proj_name, <<"Proj">>},
                                        {scoping_names, [<<"Ent">>, <<"Org">>,
                                                         <<"Proj">>, <<"Pipe">>]},
                                        {change_id, <<"ChangeId">>}]),
    ChangeId = <<"mocked_change_id">>,
    PatchsetId = <<"patchset_id">>,
    Patchset = deliv_patchset:'#new'(),
    StatusCommentId = undefined,
    GHPatchset = deliv_github_patchset:'#new'(),
    GHPayload = ct_github:load_raw_payload_from_disk("pull_request_opened.json"),
    GHSha = deliv_github_pull_request:commit_sha(chef_json:decode(GHPayload)),
    PhaseRunSummary = eu_test_helpers:phase_run_summary_example(),
    CommentBody = status_comment_example(),
    GHCreateCommentResp = <<"{\"id\": 1}">>,

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
                      ?withArgs([status_comment_id, GHPatchset]),
                      ?andReturn(StatusCommentId)),
              ?expect(getval,
                      ?withArgs([payload, GHPatchset]),
                      ?andReturn(GHPayload)),
               ?expect(setvals,
                       ?withArgs([[{status_comment_id, 1}], GHPatchset]),
                       ?andReturn(GHPatchset)),
               ?expect(update,
                       ?withArgs([GHPatchset]),
                       ?andReturn({ok, GHPatchset}))]),
    hoax:mock(deliv_change,
              ?expect(get_phase_run_summary,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, PhaseRunSummary}))),
    hoax:mock(deliv_github_api,
              ?expect(create_commit_comment,
                      ?withArgs([Scope, GHSha, CommentBody]),
                      ?andReturn({ok, GHCreateCommentResp}))),
    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([<<"Ent">>, <<"Org">>, <<"Proj">>, <<"ChangeId">>]),
                      ?andReturn(<<"http://callback.co">>))),
    deliv_github_status_comment:sync_comment(Scope, ChangeId),
    ?verifyAll.

sync_comment_updates_existing_comments() ->
    Scope = deliv_scopes:'#new_common'([{ent_name, <<"Ent">>},
                                        {org_name, <<"Org">>},
                                        {proj_name, <<"Proj">>},
                                        {scoping_names, [<<"Ent">>, <<"Org">>,
                                                         <<"Proj">>, <<"Pipe">>]},
                                        {change_id, <<"ChangeId">>}]),
    ChangeId = <<"mocked_change_id">>,
    PatchsetId = <<"patchset_id">>,
    Patchset = deliv_patchset:'#new'(),
    StatusCommentId = "defined",
    GHPatchset = deliv_github_patchset:'#new'(),
    PhaseRunSummary = eu_test_helpers:phase_run_summary_example(),
    CommentBody = status_comment_example(),

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
                      ?withArgs([status_comment_id, GHPatchset]),
                      ?andReturn(StatusCommentId))]),
    hoax:mock(deliv_change,
              ?expect(get_phase_run_summary,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, PhaseRunSummary}))),
    hoax:mock(deliv_github_api,
              ?expect(update_commit_comment,
                      ?withArgs([Scope, <<"defined">>, CommentBody]),
                      ?andReturn({ok, all_good}))),
    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([<<"Ent">>, <<"Org">>, <<"Proj">>, <<"ChangeId">>]),
                      ?andReturn(<<"http://callback.co">>))),
    deliv_github_status_comment:sync_comment(Scope, ChangeId),
    ?verifyAll.

build_comment_plist_creates_plist_map_of_phase_runs() ->
    Input = eu_test_helpers:phase_run_summary_example(),
    Expected = phase_run_plist_map_example(),
    Result = deliv_github_status_comment:build_comment_plist(Input),
    ?assertEqual(Expected, Result),
    ?verifyAll.

build_comment_table_produces_md_table() ->
    Scope = deliv_scopes:'#new_common'([{ent_name, <<"Ent">>},
                                        {org_name, <<"Org">>},
                                        {proj_name, <<"Proj">>},
                                        {scoping_names, [<<"Ent">>, <<"Org">>,
                                                         <<"Proj">>, <<"Pipe">>]},
                                        {change_id, <<"ChangeId">>}]),
    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([?any, ?any, ?any, ?any]),
                      ?andReturn(<<"http://callback.co">>))),

    Expected = status_comment_example(),
    Result =
      deliv_github_status_comment:build_comment_table(Scope, phase_run_plist_map_example()),
    ?assertEqual(Expected, Result),
    ?verifyAll.

build_status_comment_from_produces_md_table() ->
        Scope = deliv_scopes:'#new_common'([{ent_name, <<"Ent">>},
                                            {org_name, <<"Org">>},
                                            {proj_name, <<"Proj">>},
                                            {scoping_names, [<<"Ent">>, <<"Org">>,
                                                             <<"Proj">>, <<"Pipe">>]},
                                            {change_id, <<"ChangeId">>}]),
        hoax:mock(deliv_web_utils,
                  ?expect(make_web_url_for_change,
                          ?withArgs([?any, ?any, ?any, ?any]),
                          ?andReturn(<<"http://callback.co">>))),

        Expected = status_comment_example(),
        Result =
          deliv_github_status_comment:build_status_comment_from(
                            Scope, eu_test_helpers:phase_run_summary_example()),
        ?assertEqual(Expected, Result),
        ?verifyAll.

zipn_swaps_array_axises() ->
    Input = [[1,2,3],[a,b,c]],
    Expected = [[1,a],[2,b],[3,c]],
    Result = deliv_github_status_comment:zipn(Input),
    ?assertEqual(Expected, Result),
    ?verifyAll.

list_to_table_row_returns_md_table_row() ->
    List = ["1","2","3","4"],
    Expected = "|1|2|3|4|\n",
    Result = deliv_github_status_comment:list_to_table_row(List),
    ?assertEqual(Expected, Result),
    ?verifyAll.

build_status_shield_returns_properly_formatted_shield() ->
    PhaseRunSummary = #phase_run_summary{stage = <<"verify">>,
                                         phase = <<"unit">>,
                                         phase_status = <<"running">>},
    Scope = deliv_scopes:'#new_common'([{ent_name, <<"Ent">>},
                                        {org_name, <<"Org">>},
                                        {proj_name, <<"Proj">>},
                                        {scoping_names, [<<"Ent">>, <<"Org">>,
                                                         <<"Proj">>, <<"Pipe">>]},
                                        {change_id, <<"ChangeId">>}]),
    [EntName, OrgName, ProjName, ChangeId] =
        deliv_scopes:'#get'([ent_name, org_name, proj_name, change_id], Scope),

    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, ChangeId]),
                      ?andReturn(<<"http://callback.co">>))),

    Expected = "<a href='http://callback.co/status/verify', target='_blank'><img src='https://img.shields.io/badge/Unit-Running-blue.svg', alt='Unit'></a>",
    Result = deliv_github_status_comment:build_status_shield(Scope, PhaseRunSummary),
    ?assertEqual(Expected, Result),
    ?verifyAll.

phase_run_plist_map_example() ->
    [{<<"verify">>,
      [#phase_run_summary{stage_id = 1,stage = <<"verify">>,
                          stage_status = <<"passed">>,phase_id = 3,phase = <<"unit">>,
                          phase_status = <<"passed">>, search_query = <<"[builder*]">>,
                          search_description = <<"An awesome search">>},
       #phase_run_summary{stage_id = 1,stage = <<"verify">>,
                          stage_status = <<"passed">>,phase_id = 1,phase = <<"lint">>,
                          phase_status = <<"passed">>, search_query = <<"[builder*]">>,
                          search_description = <<"An awesome search">>},
       #phase_run_summary{stage_id = 1,stage = <<"verify">>,
                          stage_status = <<"passed">>,phase_id = 2,
                          phase = <<"syntax">>,phase_status = <<"passed">>,
                          search_query = <<"[builder*]">>,
                          search_description = <<"An awesome search">>}]},
     {<<"build">>,
      [#phase_run_summary{stage_id = 2,stage = <<"build">>,
                          stage_status = <<"passed">>,phase_id = 10,
                          phase = <<"unit">>,phase_status = <<"passed">>,
                          search_query = <<"[builder*]">>,
                          search_description = <<"An awesome search">>},
       #phase_run_summary{stage_id = 2,stage = <<"build">>,
                          stage_status = <<"passed">>,phase_id = 11,
                          phase = <<"lint">>,phase_status = <<"passed">>,
                          search_query = <<"[builder*]">>,
                          search_description = <<"An awesome search">>},
       #phase_run_summary{stage_id = 2,stage = <<"build">>,
                          stage_status = <<"passed">>,phase_id = 12,
                          phase = <<"syntax">>,phase_status = <<"passed">>,
                          search_query = <<"[builder*]">>,
                          search_description = <<"An awesome search">>},
       #phase_run_summary{stage_id = 2,stage = <<"build">>,
                          stage_status = <<"passed">>,phase_id = 14,
                          phase = <<"quality">>,phase_status = <<"passed">>,
                          search_query = <<"[builder*]">>,
                          search_description = <<"An awesome search">>},
       #phase_run_summary{stage_id = 2,stage = <<"build">>,
                          stage_status = <<"passed">>,phase_id = 15,
                          phase = <<"security">>,phase_status = <<"passed">>,
                          search_query = <<"[builder*]">>,
                          search_description = <<"An awesome search">>},
       #phase_run_summary{stage_id = 2,stage = <<"build">>,
                          stage_status = <<"passed">>,phase_id = 16,
                          phase = <<"publish">>,phase_status = <<"passed">>,
                          search_query = <<"[builder*]">>,
                          search_description = <<"An awesome search">>}]}].

status_comment_example() ->
    erlang:iolist_to_binary(
      ["Delivery Status:\n",
       "----------------\n",
       "|Verify|Build|\n",
       "|:--|:--|\n",
       "|", image_link("https://img.shields.io/badge/Unit-Passed-brightgreen.svg",
                       "http://callback.co/status/verify", "Unit"),
       "|", image_link("https://img.shields.io/badge/Unit-Passed-brightgreen.svg",
                       "http://callback.co/status/build", "Unit"), "|\n",
       "|", image_link("https://img.shields.io/badge/Lint-Passed-brightgreen.svg",
                       "http://callback.co/status/verify",  "Lint"),
       "|", image_link("https://img.shields.io/badge/Lint-Passed-brightgreen.svg",
                       "http://callback.co/status/build", "Lint"), "|\n",
       "|", image_link("https://img.shields.io/badge/Syntax-Passed-brightgreen.svg",
                       "http://callback.co/status/verify", "Syntax"),
       "|", image_link("https://img.shields.io/badge/Syntax-Passed-brightgreen.svg",
                       "http://callback.co/status/build", "Syntax"), "|\n",
       "| |", image_link("https://img.shields.io/badge/Quality-Passed-brightgreen.svg",
                         "http://callback.co/status/build", "Quality"), "|\n",
       "| |", image_link("https://img.shields.io/badge/Security-Passed-brightgreen.svg",
                         "http://callback.co/status/build", "Security"), "|\n",
       "| |", image_link("https://img.shields.io/badge/Publish-Passed-brightgreen.svg",
                         "http://callback.co/status/build", "Publish"), "|\n"]).

image_link(ImageUrl, LinkTarget, Phase) ->
  erlang:iolist_to_binary(["<a href='", LinkTarget, "', target='_blank'><img src='",
                           ImageUrl, "', alt='", Phase, "'></a>"]).
