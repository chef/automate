-module(notification_hand_watch_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

from_json_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "from_json", setup, teardown).

to_json_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "to_json", setup, teardown).

setup() ->
    #handler{user_name = <<"hankventure">>, ent_name = <<"HankCo">>}.

teardown(_) ->
    ok.

allowed_methods_allows_DELETE_GET_and_PUT_test() ->
    ?assertEqual({[<<"DELETE">>, <<"GET">>, <<"PUT">>], req, #handler{}},
                 notification_hand_watch:allowed_methods(req, #handler{})).

content_types_provided_provides_json_test() ->
    hoax:test(fun() ->
        hoax:mock(deliv_web_utils,
                  ?expect(content_type_json_map,
                          ?withArgs([to_json]),
                          ?andReturn(expected_map))),

        Actual = notification_hand_watch:content_types_provided(req, #handler{}),

        ?assertEqual({expected_map, req, #handler{}}, Actual),
        ?verifyAll
    end).

to_json_returns_subscription_categories_when_a_user_subscription(#handler{ent_name = EntName,
                                                                          user_name = UserName} = State) ->

    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Subscription = [<<"review">>, <<"observe">>],
    Ejson = {[ {<<"categories">>, Subscription} ]},

    hoax:mock(notification_subscriptions,
              ?expect(subscription,
                      ?withArgs([EntName, OrgName, ProjName, UserName]),
                      ?andReturn(Subscription))),

    hoax:mock(deliv_web_utils,[
              ?expect(extract_bindings,
                      ?withArgs([[org_name, proj_name], req]),
                      ?andReturn({[OrgName, ProjName], req1})),
              ?expect(content,
                      ?withArgs([Ejson, req1, State]),
                      ?andReturn({body, req2, State}))]),

    Result = notification_hand_watch:to_json(req, State),

    ?assertEqual({body, req2, State}, Result),
    ?verifyAll.

to_json_returns_empty_list_when_a_user_is_not_watching_a_project(#handler{ent_name = EntName,
                                                                          user_name = UserName} = State) ->
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Subscription = [],
    Ejson = {[ {<<"categories">>, Subscription} ]},


    hoax:mock(notification_subscriptions,
              ?expect(subscription,
                      ?withArgs([EntName, OrgName, ProjName, UserName]),
                      ?andReturn(Subscription))),

    hoax:mock(deliv_web_utils,[
              ?expect(extract_bindings,
                      ?withArgs([[org_name, proj_name], req]),
                      ?andReturn({[OrgName, ProjName], req2})),
              ?expect(content,
                      ?withArgs([Ejson, req2, State]),
                      ?andReturn({body, req3, State}))]),

    Result = notification_hand_watch:to_json(req, State),

    ?assertEqual({body, req3, State}, Result),
    ?verifyAll.

to_json_returns_500_when_subscription_returns_an_error(#handler{ent_name = EntName,
                                                                user_name = UserName} = State) ->
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,

    hoax:mock(notification_subscriptions,
              ?expect(subscription,
                      ?withArgs([EntName, OrgName, ProjName, UserName]),
                      ?andReturn({ error, reason }))),

    hoax:mock(deliv_web_utils,[
              ?expect(extract_bindings,
                      ?withArgs([[org_name, proj_name], req]),
                      ?andReturn({[OrgName, ProjName], req2})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req2, State]),
                      ?andReturn({halt, req2, State}))]),

    Result = notification_hand_watch:to_json(req, State),

    ?assertEqual({halt, req2, State}, Result),
    ?verifyAll.

from_json_returns_200_on_success(#handler{ent_name = EntName,
                                          user_name = UserName} = State) ->
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Subscriptions = [<<"review">>],
    CategoryEjson = {[{<<"categories">>, Subscriptions}]},
    JsonSpec = chef_json:rigid_object_spec([{<<"categories">>, <<"[binary()]">>}]),

    hoax:mock(deliv_web_utils,[
              ?expect(parse_json_req,
                      ?withArgs([req, JsonSpec]),
                      ?andReturn({CategoryEjson, req1})),
              ?expect(extract_bindings,
                      ?withArgs([[org_name, proj_name], req1]),
                      ?andReturn({[OrgName, ProjName], req2}))]),

    hoax:mock(notification_subscriptions,
              ?expect(subscribe,
                      ?withArgs([EntName, OrgName, ProjName, UserName, Subscriptions]),
                      ?andReturn(ok))),

    Result = notification_hand_watch:from_json(req, State),

    ?assertEqual({true, req2, State}, Result),
    ?verifyAll.

from_json_returns_400_bad_request_for_incomplete_json(State) ->
    JsonSpec = chef_json:rigid_object_spec([{<<"categories">>, <<"[binary()]">>}]),

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, JsonSpec]),
                      ?andReturn({{error, badbadbad}, req1})),
              ?expect(error_response,
                      ?withArgs([400, bad_request, req1, State]),
                      ?andReturn({halt, req1, State}))]),

    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([deliv_web_utils, parse_json_req, [req1, JsonSpec], badbadbad]))),

    Result = notification_hand_watch:from_json(req, State),

    ?assertEqual({halt, req1, State}, Result),
    ?verifyAll.

from_json_returns_412_if_no_project_is_found(#handler{ent_name = EntName,
                                                      user_name = UserName} = State) ->
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Subscriptions = [<<"review">>],
    CategoryEjson = {[{<<"categories">>, Subscriptions}]},
    JsonSpec = chef_json:rigid_object_spec([{<<"categories">>, <<"[binary()]">>}]),

    hoax:mock(notification_subscriptions,
              ?expect(subscribe,
                      ?withArgs([EntName, OrgName, ProjName, UserName, Subscriptions]),
                      ?andReturn({error, cannot_subscribe}))),

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, JsonSpec]),
                      ?andReturn({CategoryEjson, req1})),
              ?expect(extract_bindings,
                      ?withArgs([[org_name, proj_name], req1]),
                      ?andReturn({[OrgName, ProjName], req2})),
              ?expect(error_response,
                      ?withArgs([412, precondition_failed, req2, State]),
                      ?andReturn({halt, req3, State}))]),

    Result = notification_hand_watch:from_json(req, State),

    ?assertEqual({halt, req3, State}, Result),
    ?verifyAll.

from_json_returns_500_if_watch_fails(#handler{ent_name = EntName,
                                              user_name = UserName} = State) ->
    OrgName = <<"HankCo Detective Agency">>,
    ProjName = <<"Find Dermotts Dad">>,
    Subscriptions = [<<"review">>],
    CategoryEjson = {[{<<"categories">>, Subscriptions}]},
    JsonSpec = chef_json:rigid_object_spec([{<<"categories">>, <<"[binary()]">>}]),

    hoax:mock(notification_subscriptions,
              ?expect(subscribe,
                      ?withArgs([EntName, OrgName, ProjName, UserName, Subscriptions]),
                      ?andReturn({error, golly_gee_willikers}))),

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, JsonSpec]),
                      ?andReturn({CategoryEjson, req1})),
              ?expect(extract_bindings,
                      ?withArgs([[org_name, proj_name], req1]),
                      ?andReturn({[OrgName, ProjName], req2})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req2, State]),
                      ?andReturn({halt, req3, State}))]),


    Result = notification_hand_watch:from_json(req, State),

    ?assertEqual({halt, req3, State}, Result),
    ?verifyAll.
