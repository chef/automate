-module(notification_hand_slack_webhook_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_types.hrl").

-compile([export_all]).

from_json_fixture_test_() ->
    hoax:fixture(?MODULE, "from_json").

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, "to_json").

content_types_provided_test_() ->
    hoax:fixture(?MODULE, "content_types_provided_").

content_types_accepted_test_() ->
    hoax:fixture(?MODULE, "content_types_accepted_").

delete_resource_fixture_test_() ->
    hoax:fixture(?MODULE, "delete_resource_").

allowed_methods_allows_DELETE_GET_PUT_test() ->
    ?assertEqual({[<<"DELETE">>, <<"GET">>, <<"PUT">>], req, #handler{}},
                 notification_hand_slack_webhook:allowed_methods(req, #handler{})).

content_types_accepted_provides_json() ->
    hoax:mock(deliv_web_utils,
                ?expect(content_type_json_map,
                      ?withArgs([from_json]),
                      ?andReturn(expected_map))),

    Actual = notification_hand_slack_webhook:content_types_accepted(req, #handler{}),

    ?assertEqual({expected_map, req, #handler{}}, Actual),
    ?verifyAll.

content_types_provided_provides_json() ->
    hoax:mock(deliv_web_utils,
        ?expect(content_type_json_map,
                ?withArgs([to_json]),
                ?andReturn(expected_map))),

    Actual = notification_hand_slack_webhook:content_types_provided(req, #handler{}),

    ?assertEqual({expected_map, req, #handler{}}, Actual),
    ?verifyAll.

delete_resource_deletes_slack_configuration_for_organization_returns_204_on_success() ->
    State = #handler{},
    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgBorg">>,

    hoax:mock(deliv_web_utils,
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name], req]),
                      ?andReturn({[EntName, OrgName], req2}))),
    hoax:mock(notification_config_db,
              ?expect(delete,
                      ?withArgs([EntName, OrgName]),
                      ?andReturn(ok))),

    Actual = notification_hand_slack_webhook:delete_resource(req, State),

    ?assertEqual({true, req2, State}, Actual),
    ?verifyAll.

delete_resource_returns_412_on_org_not_found() ->
    State = #handler{},
    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgBorg">>,

    hoax:mock(notification_config_db,
              ?expect(delete,
                      ?withArgs([EntName, OrgName]),
                      ?andReturn({error, org_not_found}))),
    hoax:mock(chef_log,
              ?expect(error,
                      ?withArgs(["Cannot delete notification configuration for organization ~s in enterprise ~s: organization not found",
                                 [OrgName, EntName]]))),
    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name], req]),
                      ?andReturn({[EntName, OrgName], req2})),
              ?expect(error_response,
                      ?withArgs([412, precondition_failed, req2, State]),
                      ?andReturn({error, precondition_failed}))]),

    Actual = notification_hand_slack_webhook:delete_resource(req, State),

    ?assertEqual({error, precondition_failed}, Actual),
    ?verifyAll.

delete_resource_returns_500_on_failure() ->
    State = #handler{},
    EntName = <<"NCC-1701">>,
    OrgName = <<"BorgBorg">>,

    hoax:mock(notification_config_db,
              ?expect(delete,
                      ?withArgs([EntName, OrgName]),
                      ?andReturn({error, oops}))),
    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([notification_config_db, delete,
                                 [EntName, OrgName], oops]))),
    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name], req]),
                      ?andReturn({[EntName, OrgName], req2})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req2, State]),
                      ?andReturn({error, internal_server_error}))]),

    Actual = notification_hand_slack_webhook:delete_resource(req, State),

    ?assertEqual({error, internal_server_error}, Actual),
    ?verifyAll.

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

    Response = notification_hand_slack_webhook:from_json(req, state),
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
    OrgName = <<"BorgBorg">>,

    hoax:mock(deliv_web_utils,[
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name], req]),
                      ?andReturn({[EntName, OrgName], req2})),
              ?expect(parse_json_req,
                      ?withArgs([req, ExpectedEjsonSpec]),
                      ?andReturn({ReqEjson, req}))]),


    hoax:mock(notification_config, [
              ?expect(save,
                      ?withArgs([slack_webhook, [EntName, OrgName], ReqEjson]),
                      ?andReturn([inserted_config]))]),

    Response = notification_hand_slack_webhook:from_json(req, state),
    ?assertEqual({true, req2, state}, Response),
    ?verifyAll.

from_json_returns_412_when_org_not_found() ->
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

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name], req]),
                      ?andReturn({[EntName, OrgName], req2})),
              ?expect(parse_json_req,
                      ?withArgs([req, ExpectedEjsonSpec]),
                      ?andReturn({ReqEjson, req})),
              ?expect(error_response,
                      ?withArgs([412, precondition_failed, req2, state]),
                      ?andReturn({error, precondition_failed}))]),

    hoax:mock(notification_config,
              ?expect(save,
                      ?withArgs([slack_webhook, [EntName, OrgName], ReqEjson]),
                      ?andReturn({error, org_not_found}))),

    Response = notification_hand_slack_webhook:from_json(req, state),
    ?assertEqual({error, precondition_failed}, Response),
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

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name], req]),
                      ?andReturn({[EntName, OrgName], req2})),
              ?expect(parse_json_req,
                      ?withArgs([req, ExpectedEjsonSpec]),
                      ?andReturn({ReqEjson, req})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req2, state]),
                      ?andReturn({error, internal_server_error}))]),

    hoax:mock(notification_config,
              ?expect(save,
                      ?withArgs([slack_webhook, [EntName, OrgName], ReqEjson]),
                      ?andReturn({error, reason}))),

    Response = notification_hand_slack_webhook:from_json(req, state),
    ?assertEqual({error, internal_server_error}, Response),
    ?verifyAll.

to_json_when_there_is_no_notifications_config_returns_404_error_not_found() ->
    EntName = <<"bentent">>,
    OrgName = <<"borgorg">>,
    State = #handler{ent_name = EntName},

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name], req]),
                      ?andReturn({[EntName, OrgName], req2})),
              ?expect(error_response,
                      ?withArgs([404, not_found, req2, State]),
                      ?andReturn({body, req3, State}))]),
    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName]),
                      ?andReturn([]))),

    Actual = notification_hand_slack_webhook:to_json(req, State),
    ?verifyAll,
    ?assertEqual({body, req3, State}, Actual).

to_json_when_notifications_config_exists_returns_ejson_of_settings_and_enabled() ->
    EntName = <<"bentent">>,
    OrgName = <<"borgorg">>,
    State = #handler{ent_name = EntName},
    WebhookURL = <<"https://my.slack-webhook.url">>,
    OrgId = 1,
    WebhookName = <<"helper">>,
    Enabled = true,
    Config = #notification_config{notification_type = slack_webhook,
                                          name = WebhookName,
                                          settings = {[
                                              {<<"url">>, WebhookURL}
                                          ]},
                                          enabled = Enabled,
                                          organization_id = OrgId},
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
                      ?withArgs([[ent_name, org_name], req]),
                      ?andReturn({[EntName, OrgName], req2})),
              ?expect(content,
                      ?withArgs([Ejson, req2, State]),
                      ?andReturn({body, req3, State}))]),

    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName]),
                      ?andReturn([Config]))),

    hoax:mock(notification_web_utils,
              ?expect(to_json,
                      ?withArgs([Config]),
                      ?andReturn(Ejson))),

    Actual = notification_hand_slack_webhook:to_json(req, State),
    ?verifyAll,
    ?assertEqual({body, req3, State}, Actual).


to_json_when_db_fetch_returns_error() ->
    EntName = <<"bentent">>,
    OrgName = <<"borgorg">>,
    State = #handler{ent_name = EntName},

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name, org_name], req]),
                      ?andReturn({[EntName, OrgName], req2})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req2, State]),
                      ?andReturn(error))]),
    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName, OrgName]),
                      ?andReturn({error, why}))),

    Actual = notification_hand_slack_webhook:to_json(req, State),
    ?verifyAll,
    ?assertEqual(error, Actual).
