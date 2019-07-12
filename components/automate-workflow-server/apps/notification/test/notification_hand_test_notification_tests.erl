-module(notification_hand_test_notification_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_macros.hrl").

-compile([export_all]).

from_json_fixture_test_() ->
    hoax:fixture(?MODULE, "from_json").

from_json_posts_test_notification_to_slack_webhook() ->
    WebhookURL = <<"https://hooks.slack.com/services/FAKE/FAKE/THISISFAKE">>,

    ReqEjson = {[
        {<<"url">>, WebhookURL}
    ]},

    ExpectedEjsonSpec = chef_json:simple_string_dict_spec([<<"url">>]),

    ExpectedNotification =  {[
        {<<"username">>, <<"Chef_Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, <<"Test message from Chef Automate!">>},
                {<<"text">>, <<"Test message from Chef Automate!">>}
            ]}
        ]}
    ]},

    ExpectedHeaders = [{"Content-Type", "application/json"}],

    hoax:mock(deliv_web_utils,
              ?expect(parse_json_req,
                      ?withArgs([req, ExpectedEjsonSpec]),
                      ?andReturn({ReqEjson, req}))),

    %% Mock under test %%
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, WebhookURL,
                                 ExpectedNotification, ExpectedHeaders]),
                      ?andReturn({ok, 200, resp_headers, resp_body}))),

    Result = notification_hand_test_notification:from_json(req, state),
    ?assertEqual({true, req, state}, Result),
    ?verifyAll.

from_json_slack_failure_returns_failure_response() ->
    WebhookURL = <<"https://hooks.slack.com/services/FAKE/FAKE/THISISFAKE">>,

    ReqEjson = {[
        {<<"url">>, WebhookURL}
    ]},

    ExpectedEjsonSpec = chef_json:simple_string_dict_spec([<<"url">>]),

    ExpectedNotification =  {[
        {<<"username">>, <<"Chef_Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, <<"Test message from Chef Automate!">>},
                {<<"text">>, <<"Test message from Chef Automate!">>}
            ]}
        ]}
    ]},

    ExpectedHeaders = [{"Content-Type", "application/json"}],

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, ExpectedEjsonSpec]),
                      ?andReturn({ReqEjson, req})),
              ?expect(error_response,
                      ?withArgs([404, ?any, ?any, ?any]),
                      ?andReturn(error))
    ]),

    %% Mock under test %%
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, WebhookURL,
                                 ExpectedNotification, ExpectedHeaders]),
                      ?andReturn({ok, 404, resp_headers, resp_body}))),

    Result = notification_hand_test_notification:from_json(req, state),
    ?assertEqual(error, Result),
    ?verifyAll.

from_json_returns_504_when_post_from_server_fails() ->
    WebhookURL = <<"https://hooks.slack.com/services/FAKE/FAKE/THISISFAKE">>,

    ReqEjson = {[
        {<<"url">>, WebhookURL}
    ]},

    ExpectedEjsonSpec = chef_json:simple_string_dict_spec([<<"url">>]),

    ExpectedNotification =  {[
        {<<"username">>, <<"Chef_Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, <<"Test message from Chef Automate!">>},
                {<<"text">>, <<"Test message from Chef Automate!">>}
            ]}
        ]}
    ]},

    ExpectedHeaders = [{"Content-Type", "application/json"}],

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, ExpectedEjsonSpec]),
                      ?andReturn({ReqEjson, req})),
              ?expect(error_response,
                      ?withArgs([504, gateway_timeout, ?any, ?any]),
                      ?andReturn(error))
    ]),

    %% Mock under test %%
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, WebhookURL,
                                 ExpectedNotification, ExpectedHeaders]),
                      ?andReturn({error, noooope}))),

    Result = notification_hand_test_notification:from_json(req, state),
    ?assertEqual(error, Result),
    ?verifyAll.


from_json_returns_400_when_parse_json_req_fails() ->
    ExpectedEjsonSpec = chef_json:simple_string_dict_spec([<<"url">>]),

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, ExpectedEjsonSpec]),
                      ?andReturn({{error, why}, req})),
              ?expect(error_response,
                      ?withArgs([400, bad_request, req, state]),
                      ?andReturn(error))
    ]),

    Result = notification_hand_test_notification:from_json(req, state),
    ?assertEqual(error, Result),
    ?verifyAll.
