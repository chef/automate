-module(deliv_github_patchset_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

fixture_test_() ->
    hoax:fixture(?MODULE).

clone_url_returns_git_url() ->
    hoax:mock(deliv_db,
        ?expect(select, ?withArgs([deliv_github_patchset, get_clone_url, [42]]),
        ?andReturn({ok, [[{<<"clone_url">>, <<"result">>}]]}))),
    Patchset = deliv_patchset:setvals([{id, 42}], deliv_patchset:'#new'()),
    Actual = deliv_github_patchset:clone_url(Patchset),
    ?assertEqual(<<"result">>, Actual),
    ?verifyAll.

insert_stores_payload_in_db_with_foreign_key_to_patchset() ->
    FakeRecord = {deliv_github_patchset,undefined,undefined,undefined,undefined},
    hoax:mock(deliv_db, ?expect(insert, ?withArgs([FakeRecord]))),
    deliv_github_patchset:insert(FakeRecord),
    ?verifyAll.

save_generates_record_and_inserts_it() ->
    FakeRecord = {deliv_github_patchset,undefined,42,<<"{}">>,undefined},
    Patchset = deliv_patchset:setvals([{id, 42}], deliv_patchset:'#new'()),
    Payload = {[]},
    hoax:mock(deliv_db, ?expect(insert, ?withArgs([FakeRecord]))),
    deliv_github_patchset:save(Payload, Patchset),
    ?verifyAll.

update_updates_record_in_db() ->
    FakeRecord = {deliv_github_patchset,undefined,42,<<"{}">>,undefined},
    hoax:mock(deliv_db, ?expect(update, ?withArgs([FakeRecord]))),
    deliv_github_patchset:update(FakeRecord),
    ?verifyAll.

get_change_id_by_patchset() ->
    PatchsetId = <<"fake_id">>,
    GithubPatchset = deliv_github_patchset:setvals([{patchset_id, PatchsetId}],
            deliv_github_patchset:'#new'()),
    Patchset = deliv_patchset:'#new'(),
    hoax:mock(deliv_patchset,
             [?expect(fetch,
                      ?withArgs([PatchsetId]),
                      ?andReturn({ok, Patchset})),
              ?expect(getval,
                      ?withArgs([change_id, Patchset]),
                      ?andReturn(42))]),

    deliv_github_patchset:get_change_id_by_patchset(GithubPatchset),
    ?verifyAll.

pr_url_returns_pr_url_for_patchset() ->
    Patchset = deliv_patchset:fromlist([{id, 42}]),

    hoax:mock(deliv_db,
              ?expect(select,
                      ?withArgs([deliv_github_patchset, get_pr_url, [42]]),
                      ?andReturn({ok, [[{<<"pr_url">>, <<"result">>}]]}))),

    ?assertEqual(<<"result">>, deliv_github_patchset:pr_url(Patchset)),
    ?verifyAll.
