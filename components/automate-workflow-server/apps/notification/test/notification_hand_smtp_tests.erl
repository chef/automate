-module(notification_hand_smtp_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_types.hrl").

-compile([export_all]).

delete_fixture_test_() ->
    hoax:fixture(?MODULE, "delete").

from_json_fixture_test_() ->
    hoax:fixture(?MODULE, "from_json").

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, "to_json").

allowed_methods_allows_PUT_test() ->
    ?assertEqual({[<<"DELETE">>, <<"GET">>, <<"PUT">>], req, #handler{}},
                 notification_hand_smtp:allowed_methods(req, #handler{})).

content_types_accepted_provides_json_test() ->
    hoax:test(fun() ->
        hoax:mock(deliv_web_utils,
                  ?expect(content_type_json_map,
                          ?withArgs([from_json]),
                          ?andReturn(expected_map))),

        Actual = notification_hand_smtp:content_types_accepted(req, #handler{}),

        ?assertEqual({expected_map, req, #handler{}}, Actual),
        ?verifyAll
    end).

content_types_provided_provides_json_test() ->
    hoax:test(fun() ->
        hoax:mock(deliv_web_utils,
                  ?expect(content_type_json_map,
                          ?withArgs([to_json]),
                          ?andReturn(expected_map))),

        Actual = notification_hand_smtp:content_types_provided(req, #handler{}),

        ?assertEqual({expected_map, req, #handler{}}, Actual),
        ?verifyAll
    end).

delete_resource_deletes_smtp_configuration_for_enterprise_returns_204_on_success() ->
    EntName = <<"NCC-1701">>,
    State = #handler{ent_name = EntName},

    hoax:mock(notification_config_db,
              ?expect(delete,
                      ?withArgs([EntName]),
                      ?andReturn(ok))),

    Actual = notification_hand_smtp:delete_resource(req, State),

    ?assertEqual({true, req, State}, Actual),
    ?verifyAll.

delete_resource_returns_500_on_failure() ->
    EntName = <<"NCC-1701">>,
    State = #handler{ent_name = EntName},

    hoax:mock(notification_config_db,
              ?expect(delete,
                      ?withArgs([EntName]),
                      ?andReturn({error, internal_server_error}))),
    hoax:mock(deliv_web_utils,
            ?expect(error_response,
                    ?withArgs([500, internal_server_error, req, State]),
                    ?andReturn({error, not_found}))),

    Actual = notification_hand_smtp:delete_resource(req, State),

    ?assertEqual({error, not_found}, Actual),
    ?verifyAll.

from_json_returns_400_bad_request_for_incomplete_json() ->

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, notification_smtp_config]),
                      ?andReturn({{error, bad_request}, req})),
              ?expect(error_response,
                      ?withArgs([400, bad_request, ?any, ?any]),
                      ?andReturn({error, bad_request}))]),

    Response = notification_hand_smtp:from_json(req, state),
    ?assertEqual({error, bad_request}, Response),
    ?verifyAll.

from_json_returns_201_on_notification_configuration_success() ->
    EntName = <<"Gizmonics">>,
    ReqEjson = {[
        {<<"host">>, <<"smtp.host.whatever">>},
        {<<"port">>, 25},
        {<<"smtp_login">>, <<"tomservo">>},
        {<<"password">>, <<"crowstinks">>},
        {<<"sender_email">>, <<"beep@boop.com">>},
        {<<"sender_name">>, <<"">>}
    ]},
    deliv_json:init_schemas(),
    Data = chef_json:encode(ReqEjson),

    hoax:mock(cowboy_req, [
              ?expect(body,
                      ?withArgs([req]),
                      ?andReturn({ok, Data, req1})),
              ?expect(binding,
                      ?withArgs([ent_name, req1]),
                      ?andReturn({EntName, req2}))]),
    hoax:mock(notification_config,
              ?expect(save,
                      ?withArgs([smtp, [EntName], ReqEjson]),
                      ?andReturn(config))),

    Response = notification_hand_smtp:from_json(req, state),
    ?assertEqual({true, req2, state}, Response),
    ?verifyAll.

from_json_returns_201_on_notification_configuration_success_with_no_password() ->
    EntName = <<"Gizmonics">>,
    ReqEjson = {[
        {<<"host">>, <<"smtp.host.whatever">>},
        {<<"port">>, 25},
        {<<"smtp_login">>, <<"tomservo">>},
        {<<"sender_email">>, <<"beep@boop.com">>},
        {<<"sender_name">>, <<>>}
    ]},
    deliv_json:init_schemas(),
    Data = chef_json:encode(ReqEjson),

    hoax:mock(cowboy_req, [
              ?expect(body,
                      ?withArgs([req]),
                      ?andReturn({ok, Data, req1})),
              ?expect(binding,
                      ?withArgs([ent_name, req1]),
                      ?andReturn({EntName, req2}))]),
    hoax:mock(notification_config,
              ?expect(save,
                      ?withArgs([smtp, [EntName], ReqEjson]),
                      ?andReturn(config))),

    Response = notification_hand_smtp:from_json(req, state),
    ?assertEqual({true, req2, state}, Response),
    ?verifyAll.

from_json_returns_201_on_notification_configuration_success_with_no_password_and_no_user() ->
    EntName = <<"Gizmonics">>,
    ReqEjson = {[
        {<<"host">>, <<"smtp.host.whatever">>},
        {<<"port">>, 25},
        {<<"sender_email">>, <<"beep@boop.com">>},
        {<<"sender_name">>, <<>>}
    ]},
    deliv_json:init_schemas(),
    Data = chef_json:encode(ReqEjson),

    hoax:mock(cowboy_req, [
              ?expect(body,
                      ?withArgs([req]),
                      ?andReturn({ok, Data, req1})),
              ?expect(binding,
                      ?withArgs([ent_name, req1]),
                      ?andReturn({EntName, req2}))]),
    hoax:mock(notification_config,
              ?expect(save,
                      ?withArgs([smtp, [EntName], ReqEjson]),
                      ?andReturn(config))),

    Response = notification_hand_smtp:from_json(req, state),
    ?assertEqual({true, req2, state}, Response),
    ?verifyAll.

%% Note: this is what happens when the user had saved an SMTP config with a
%% login and password, but has chosen to remove it in the text boxes -- the UI
%% will send empty string, and we will store those in the database.
%% It still has to be decided if "empty string as username/password" is
%% something we want to support.
from_json_returns_201_on_notification_configuration_success_with_no_password_and_no_user_because_it_was_removed_in_the_ui() ->
    EntName = <<"Gizmonics">>,
    ReqEjson = {[
        {<<"host">>, <<"smtp.host.whatever">>},
        {<<"port">>, 25},
        {<<"smtp_login">>, <<>>},
        {<<"password">>, <<>>},
        {<<"sender_email">>, <<"beep@boop.com">>},
        {<<"sender_name">>, <<>>}
    ]},
    deliv_json:init_schemas(),
    Data = chef_json:encode(ReqEjson),

    hoax:mock(cowboy_req, [
              ?expect(body,
                      ?withArgs([req]),
                      ?andReturn({ok, Data, req1})),
              ?expect(binding,
                      ?withArgs([ent_name, req1]),
                      ?andReturn({EntName, req2}))]),
    hoax:mock(notification_config,
              ?expect(save,
                      ?withArgs([smtp, [EntName], ReqEjson]),
                      ?andReturn(config))),

    Response = notification_hand_smtp:from_json(req, state),
    ?assertEqual({true, req2, state}, Response),
    ?verifyAll.

from_json_returns_500_when_insert_fails() ->
    EntName = <<"Gizmonics">>,
    ReqEjson = {[
        {<<"host">>, <<"smtp.host.whatever">>},
        {<<"port">>, 25},
        {<<"smtp_login">>, <<"tomservo">>},
        {<<"password">>, <<"crowstinks">>},
        {<<"sender_email">>, <<"beep@boop.com">>},
        {<<"sender_name">>, <<"">>}
    ]},

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, notification_smtp_config]),
                      ?andReturn({ReqEjson, req})),
                  ?expect(error_response,
                      ?withArgs([500, internal_server_error, req1, state]),
                      ?andReturn({error, internal_server_error}))]),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(notification_config,
              ?expect(save,
                      ?withArgs([smtp, [EntName], ReqEjson]),
                      ?andReturn({error, reason}))),

    Response = notification_hand_smtp:from_json(req, state),
    ?assertEqual({error, internal_server_error}, Response),
    ?verifyAll.

from_json_returns_400_when_attempting_to_retrieve_a_config_when_no_password() ->
    EntName = <<"Gizmonics">>,
    ReqEjson = {[
        {<<"host">>, <<"smtp.host.whatever">>},
        {<<"port">>, 25},
        {<<"smtp_login">>, <<"tomservo">>},
        {<<"sender_email">>, <<"beep@boop.com">>},
        {<<"sender_name">>, <<"">>}
    ]},

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req, notification_smtp_config]),
                      ?andReturn({ReqEjson, req})),
                  ?expect(error_response,
                      ?withArgs([400, bad_request, req1, state]),
                      ?andReturn({error, bad_request}))]),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(notification_config,
              ?expect(save,
                      ?withArgs([smtp, [EntName], ReqEjson]),
                      ?andReturn({error, no_password}))),

    Response = notification_hand_smtp:from_json(req, state),
    ?assertEqual({error, bad_request}, Response),
    ?verifyAll.

to_json_when_there_is_no_smtp_config_returns_404_error_not_found() ->
    EntName = <<"bentent">>,
    State = #handler{ent_name = EntName},

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([404, not_found, req1, State]),
                      ?andReturn({body, req2, State}))),
    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn([]))),

    Actual = notification_hand_smtp:to_json(req, State),
    ?verifyAll,
    ?assertEqual({body, req2, State}, Actual).

to_json_when_fetch_is_an_error_returns_500_internal_error() ->
    EntName = <<"bentent">>,
    State = #handler{ent_name = EntName},

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([500, internal_error, req1, State]),
                      ?andReturn({body, req2, State}))),
    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({error, boooooo}))),

    Actual = notification_hand_smtp:to_json(req, State),
    ?verifyAll,
    ?assertEqual({body, req2, State}, Actual).

to_json_when_config_is_found_return_ejson_of_settings() ->
    EntName = <<"bentent">>,
    State = #handler{ent_name = EntName},
    Host = <<"it.happening.one.night">>,
    Port = 25,
    Login = <<"mangrenade">>,
    Password = <<"encryptme">>,
    Email = <<"hatred@venturetec.com">>,
    SenderName = <<"Sgt Hatred">>,

    Config = #notification_config{notification_type = smtp,
                                  settings = {[
                                    {<<"host">>, Host},
                                    {<<"port">>, Port},
                                    {<<"smtp_login">>, Login},
                                    {<<"password">>, Password},
                                    {<<"sender_email">>, Email},
                                    {<<"sender_name">>, SenderName}
                                  ]}},

    Ejson = {[
              {<<"host">>, Host},
              {<<"port">>, Port},
              {<<"smtp_login">>, Login},
              {<<"sender_email">>, Email},
              {<<"sender_name">>, SenderName}
            ]},

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Ejson, req1, State]),
                      ?andReturn({body, req2, State}))),
    hoax:mock(notification_config_db,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn([Config]))),
    hoax:mock(notification_web_utils,
              ?expect(to_json,
                      ?withArgs([Config]),
                      ?andReturn(Ejson))),

    Actual = notification_hand_smtp:to_json(req, State),
    ?verifyAll,
    ?assertEqual({body, req2, State}, Actual).
