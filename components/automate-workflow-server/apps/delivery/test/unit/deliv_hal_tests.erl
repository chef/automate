-module(deliv_hal_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

%% suspicious of the amount of random mocking that this requires, but
%% don't currently have any good cleanup suggestions
authorized_hal_test_() ->
    meck:expect(deliv_authz, authorize,
                fun(_, _, _, _, _) -> allow end),

    OrgNames = ["X-Men", "Justice League"],
    RespData = {[{<<"orgs">>, OrgNames}]},
    OrgLink = case OrgNames of
                  [] -> [];
                  _ -> [{show_org,
                         {t, deliv_web_utils:href("marvel",
                                                  "/orgs/{org_name}")},
                         [], ignore_authz}]
              end,
    HalLinks = [{create_org,
                 deliv_web_utils:href("marvel", "/orgs"), []} |
                OrgLink],
    %% unless we factor out the interactions with the state and the
    %% request, I am not sure that this test is going to tell us what
    %% we want to know.  it'll also be a bit fragile, if the
    %% definition of #handler{} or cowboy_req changes.
    Hal = deliv_hal:add_authorized_hal(RespData,
                                       HalLinks,
                                       cowboy_req:new(undefined, undefined,
                                                      undefined, <<"GET">>,
                                                      undefined, undefined,
                                                      'HTTP/1.1', undefined,
                                                      undefined, undefined,
                                                      undefined, false,
                                                      undefined, undefined),
                                       #handler{user_name = <<"Spawn">>,
                                                hal_authz = [{bar, baz}],
                                                hal_map = [{<<"GET">>, [{<<"create_org">>, foo, bar}]}],
                                                authz = []}),
    ct_meck:unload(deliv_authz),
    ?_assertEqual({[{<<"orgs">>,["X-Men","Justice League"]},
                    {<<"_links">>,
                     {[{<<"create_org">>,{[{<<"href">>,<<"/api/v0/e/marvel/orgs">>}]}},
                       {<<"show_org">>,{[{<<"href">>,<<"/api/v0/e/marvel/orgs/{org_name}">>},
                                         {<<"templated">>,true}]}}]}}]},
                  Hal).


hal_map_test_() ->
    Map = deliv_hal:hal_map(),
    Elt = lists:nth(1, Map),
    %% hard to do specific tests here, so verify the structure a
    %% little at least
    [
     ?_assert(is_list(Map)),
     ?_assert(is_tuple(Elt)),
     ?_assertEqual(2, size(Elt)),
     ?_assert(is_atom(element(1, Elt))),
     ?_assert(is_list(element(2, Elt)))
    ].


hal_mapping_test_() ->
    Map = deliv_hal:hal_map(),
    Expected = [{<<"GET">>,[{<<"create_org">>,<<"POST">>,deliv_hand_orgs},
                            {<<"show_org">>,<<"GET">>,deliv_hand_orgs_named}]},
                {<<"POST">>,[{<<"full">>,<<"GET">>,deliv_hand_orgs_named}]}],
    [
     ?_assertEqual(Expected, deliv_hal:hal_mapping_for(deliv_hand_orgs, Map)),
     ?_assertException(error, {undefined_mapping, _},
                       deliv_hal:hal_mapping_for(argle_bargle, Map))
    ].
