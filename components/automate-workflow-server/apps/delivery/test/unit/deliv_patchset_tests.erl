-module(deliv_patchset_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

deliv_patchset_fixture_test_() ->
    [
     hoax:fixture(?MODULE, new_),
     hoax:fixture(?MODULE, latest_patchset_for_change_)
    ].

% mock delivery event publish
% expect published event to look like new patchset record
new_patchset_sends_deliv_event() ->
    Patchset = deliv_patchset:'#new'(),
    EntName = <<"ent_name">>,
    UserName = <<"user_name">>,
    OrgName = <<"org_name">>,
    ProjName = <<"proj_name">>,
    PipeName = <<"pipe_name">>,
    FeatureBranch = <<"feature_branch">>,
    Sha = <<"sha">>,

    hoax:expect(receive
                    deliv_db:qfetch(deliv_patchset, create_patchset_and_change,
                                    [ EntName, UserName, OrgName, ProjName, PipeName, FeatureBranch, Sha ]) -> [Patchset]
                end),

    deliv_patchset:new(EntName, UserName, OrgName, ProjName, PipeName, FeatureBranch, Sha),

    ?verifyAll.

latest_patchset_for_change_when_there_is_an_error_returns_error() ->
    ChangeId = <<"62794f41-7a0a-40fc-ae15-13b4aae99e8d">>,
    hoax:expect(receive
                    sqerl_rec:qfetch(deliv_patchset, get_latest_patchset, [ChangeId]) -> {error, reason}
                end),

    ?assertEqual({error, reason}, deliv_patchset:latest_patchset_for_change(ChangeId)),
    ?verifyAll.

latest_patchset_for_change_when_there_are_no_patchsets_found_returns_error() ->
    ChangeId = <<"62794f41-7a0a-40fc-ae15-13b4aae99e8d">>,
    hoax:expect(receive
                    sqerl_rec:qfetch(deliv_patchset, get_latest_patchset, [ChangeId]) -> []
                end),

    ?assertEqual({error, patchset_not_found}, deliv_patchset:latest_patchset_for_change(ChangeId)),
    ?verifyAll.

latest_patchset_for_change_when_there_is_a_patchset_returns_it() ->
    ChangeId = <<"62794f41-7a0a-40fc-ae15-13b4aae99e8d">>,
    Patchset = deliv_patchset:'#new'(),
    hoax:expect(receive
                    sqerl_rec:qfetch(deliv_patchset, get_latest_patchset, [ChangeId]) -> [Patchset]
                end),

    ?assertEqual({ok, Patchset}, deliv_patchset:latest_patchset_for_change(ChangeId)),
    ?verifyAll.
