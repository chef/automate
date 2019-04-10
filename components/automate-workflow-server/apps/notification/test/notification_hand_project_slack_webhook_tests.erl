-module(notification_hand_project_slack_webhook_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_types.hrl").

-compile([export_all]).

from_json_fixture_test_() ->
    hoax:fixture(?MODULE, "from_json").

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, "to_json").

delete_resource_fixture_test_() ->
    hoax:fixture(?MODULE, "delete_resource_").


allowed_methods_allows_DELETE_GET_PUT_test() ->
    AllowedMethods =
        notification_hand_project_slack_webhook:allowed_methods(req, #handler{}),
    ?assertEqual({[<<"DELETE">>, <<"GET">>, <<"PUT">>], req, #handler{}}, AllowedMethods).

content_types_accepted_accepts_json_test() ->
    hoax:test(fun() ->
        hoax:mock(deliv_web_utils,
                    ?expect(content_type_json_map,
                          ?withArgs([from_json]),
                          ?andReturn(expected_map))),

        Actual =
            notification_hand_project_slack_webhook:content_types_accepted(req, #handler{}),

        ?assertEqual({expected_map, req, #handler{}}, Actual),
        ?verifyAll
    end).

content_types_provided_provides_json_test() ->
    hoax:test(fun() ->
        hoax:mock(deliv_web_utils,
                    ?expect(content_type_json_map,
                          ?withArgs([to_json]),
                          ?andReturn(expected_map))),

        Actual =
            notification_hand_project_slack_webhook:content_types_provided(req, #handler{}),

        ?assertEqual({expected_map, req, #handler{}}, Actual),
        ?verifyAll
    end).

from_json_returns_400_bad_request_for_incomplete_json() ->
    ExpectedEjsonSpec = chef_json:rigid_object_spec([{<<"enabled">>, boolean},
                                                      {<<"name">>, string},
                                                      {<<"url">>, string}]),

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, ExpectedEjsonSpec]),
                      ?andReturn({{error, bad_request}, req})),
              ?expect(error_response,
                      ?withArgs([400, bad_request, ?any, ?any]),
                      ?andReturn({error, bad_request}))]),

    Response = notification_hand_project_slack_webhook:from_json(req, state),
    ?assertEqual({error, bad_request}, Response),
    ?verifyAll.

from_json_returns_201_on_notification_configuration_success() ->
    ExpectedEjsonSpec = chef_json:rigid_object_spec([{<<"enabled">>, boolean},
                                                      {<<"name">>, string},
                                                      {<<"url">>, string}]),
    Name = <<"IP Freely">>,
    Url = <<"https://my.slack.com/webhook">>,
    Enabled = true,

    ReqEjson = {[
        {<<"name">>, Name},
        {<<"url">>, Url},
        {<<"enabled">>, Enabled}
    ]},

    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgOrg">>,
    ProjName = <<"TheUnicomplex">>,

    hoax:mock(deliv_web_utils,[
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name, proj_name], req]),
                      ?andReturn({[EntName, OrgName, ProjName], req3})),
              ?expect(parse_json_req,
                      ?withArgs([req, ExpectedEjsonSpec]),
                      ?andReturn({ReqEjson, req}))]),

    hoax:mock(notification_config, [
              ?expect(save,
                      ?withArgs([slack_webhook, [EntName, OrgName, ProjName], ReqEjson]),
                      ?andReturn([inserted_config]))]),

    Response = notification_hand_project_slack_webhook:from_json(req, state),
    ?assertEqual({true, req3, state}, Response),
    ?verifyAll.

from_json_returns_412_when_project_not_found() ->
    ExpectedEjsonSpec = chef_json:rigid_object_spec([{<<"enabled">>, boolean},
                                                      {<<"name">>, string},
                                                      {<<"url">>, string}]),
    Name = <<"IP Freely">>,
    Url = <<"https://my.slack.com/webhook">>,
    Enabled = true,

    ReqEjson = {[
        {<<"name">>, Name},
        {<<"url">>, Url},
        {<<"enabled">>, Enabled}
    ]},

    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgBorg">>,
    ProjName = <<"TheUnicomplex">>,

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name, proj_name], req]),
                      ?andReturn({[EntName, OrgName, ProjName], req3})),
              ?expect(parse_json_req,
                      ?withArgs([req, ExpectedEjsonSpec]),
                      ?andReturn({ReqEjson, req})),
              ?expect(error_response,
                      ?withArgs([412, precondition_failed, req3, state]),
                      ?andReturn({halt, req4, state}))]),

    hoax:mock(notification_config,
              ?expect(save,
                      ?withArgs([slack_webhook, [EntName, OrgName, ProjName], ReqEjson]),
                      ?andReturn({error, proj_not_found}))),

    Response = notification_hand_project_slack_webhook:from_json(req, state),
    ?assertEqual({halt, req4, state}, Response),
    ?verifyAll.

from_json_returns_500_when_insert_fails() ->
    ExpectedEjsonSpec = chef_json:rigid_object_spec([{<<"enabled">>, boolean},
                                                      {<<"name">>, string},
                                                      {<<"url">>, string}]),
    Name = <<"IP Freely">>,
    Url = <<"https://my.slack.com/webhook">>,
    Enabled = true,

    ReqEjson = {[
        {<<"name">>, Name},
        {<<"url">>, Url},
        {<<"enabled">>, Enabled}
    ]},

    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgBorg">>,
    ProjName = <<"TheUnicomplex">>,

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name, proj_name], req]),
                      ?andReturn({[EntName, OrgName, ProjName], req3})),
              ?expect(parse_json_req,
                      ?withArgs([req, ExpectedEjsonSpec]),
                      ?andReturn({ReqEjson, req})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req3, state]),
                      ?andReturn({halt, req4, state}))]),

    hoax:mock(notification_config,
              ?expect(save,
                      ?withArgs([slack_webhook, [EntName, OrgName, ProjName], ReqEjson]),
                      ?andReturn({error, reason}))),

    Response = notification_hand_project_slack_webhook:from_json(req, state),
    ?assertEqual({halt, req4, state}, Response),
    ?verifyAll.

to_json_returns_404_when_no_config_exists() ->
    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgBorg">>,
    ProjName = <<"TheUnicomplex">>,

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name, proj_name], req]),
                      ?andReturn({[EntName, OrgName, ProjName], req2})),
              ?expect(error_response,
                      ?withArgs([404, not_found, req2, state]),
                      ?andReturn({body, req3, state}))]),
    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn([]))),

    Actual = notification_hand_project_slack_webhook:to_json(req, state),
    ?verifyAll,
    ?assertEqual({body, req3, state}, Actual).

to_json_when_notifications_config_exists_for_project_returns_ejson_of_settings_and_enabled() ->
    EntName = <<"bentent">>,
    OrgName = <<"borgorg">>,
    ProjName = <<"TheUnicomplex">>,

    WebhookURL = <<"https://my.slack-webhook.url">>,
    ProjId = 1,
    WebhookName = <<"helper">>,
    Enabled = true,
    Config = #notification_config{notification_type = slack_webhook,
                                  name = WebhookName,
                                  settings = {[{<<"url">>, WebhookURL}]},
                                  enabled = Enabled,
                                  project_id = ProjId},

    Ejson = {[
              {<<"webhook">>,
                {[
                  {<<"url">>, WebhookURL},
                  {<<"name">>, WebhookName},
                  {<<"enabled">>, Enabled}
                ]}
              }
            ]},

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name, proj_name], req]),
                      ?andReturn({[EntName, OrgName, ProjName], req1})),
              ?expect(content,
                      ?withArgs([Ejson, req1, state]),
                      ?andReturn({body, req2, state}))]),

    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn([Config]))),

    hoax:mock(notification_web_utils,
              ?expect(to_json,
                      ?withArgs([Config]),
                      ?andReturn(Ejson))),

    Actual = notification_hand_project_slack_webhook:to_json(req, state),
    ?verifyAll,
    ?assertEqual({body, req2, state}, Actual).

to_json_returns_500_on_failure_to_fetch_config() ->
    EntName = <<"bentent">>,
    OrgName = <<"borgorg">>,
    ProjName = <<"TheUnicomplex">>,

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name, proj_name], req]),
                      ?andReturn({[EntName, OrgName, ProjName], req1})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req1, state]),
                      ?andReturn({body, req2, state}))]),

    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn({error, why}))),

    Actual = notification_hand_project_slack_webhook:to_json(req, state),
    ?verifyAll,
    ?assertEqual({body, req2, state}, Actual).

delete_resource_deletes_slack_configuration_for_project_returns_204_on_success() ->
    State = #handler{},
    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgBorg">>,
    ProjName = <<"TheUnicomplex">>,

    hoax:mock(deliv_web_utils,
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name, proj_name], req]),
                      ?andReturn({[EntName, OrgName, ProjName], req2}))),
    hoax:mock(notification_config_db,
              ?expect(delete,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn(ok))),

    Actual = notification_hand_project_slack_webhook:delete_resource(req, State),

    ?assertEqual({true, req2, State}, Actual),
    ?verifyAll.

delete_resource_returns_500_on_failure() ->
    State = #handler{},
    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgBorg">>,
    ProjName = <<"TheUnicomplex">>,

    hoax:mock(notification_config_db,
              ?expect(delete,
                      ?withArgs([EntName, OrgName, ProjName]),
                      ?andReturn({error, oops}))),
    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name, proj_name], req]),
                      ?andReturn({[EntName, OrgName, ProjName], req2})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req2, State]),
                      ?andReturn({error, internal_server_error}))]),

    Actual = notification_hand_project_slack_webhook:delete_resource(req, State),

    ?assertEqual({error, internal_server_error}, Actual),
    ?verifyAll.
