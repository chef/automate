-module(auth_hand_saml_users_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

from_json_fixture_test_() ->
    hoax:fixture(?MODULE, "from_json").

allowed_methods_allows_POST_test() ->
    ?assertEqual({[<<"POST">>], req, state},
                 auth_hand_saml_users:allowed_methods(req, state)).

content_types_accepted_accepts_map_test() ->
    hoax:test(fun() ->
        hoax:mock(deliv_web_utils,
                  ?expect(content_type_json_or_any_map,
                          ?withArgs([from_json]),
                          ?andReturn(expected_map))),

        Actual = deliv_hand_phase_run_log_objects:content_types_accepted(req, state),

        ?assertEqual({expected_map, req, state}, Actual),
        ?verifyAll
    end).

from_json_returns_400_bad_request_for_incomplete_data() ->
    deliv_json:init_schemas(),

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req]),
                      ?andReturn({{[]}, req1})),
              ?expect(error_response,
                      ?withArgs([400, bad_request, req1, state]),
                      ?andReturn({halt, req2, state}))]),

    Actual = auth_hand_saml_users:from_json(req, state),

    ?assertEqual({halt, req2, state}, Actual),
    ?verifyAll.

from_json_returns_400_bad_request_for_erroneous_data() ->
    deliv_json:init_schemas(),

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req]),
                      ?andReturn({{error, {why, ejson}}, req1})),
              ?expect(error_response,
                      ?withArgs([400, bad_request, req1, state]),
                      ?andReturn({halt, req2, state}))]),

    Actual = auth_hand_saml_users:from_json(req, state),

    ?assertEqual({halt, req2, state}, Actual),
    ?verifyAll.

from_json_returns_a_500_when_data_is_valid_but_there_is_an_error_saving_data() ->
    deliv_json:init_schemas(),
    EntName = <<"foo">>,
    State = #handler{ent_name = EntName},
    Json = {[
        {<<"name">>,<<"someone">>}
        ]},
    SamlPropList =  translated_prop_list_with_saml_user_type,
    {UpdatedPropList} = ej:set({"user_type"}, Json, <<"saml">>),

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req]),
                      ?andReturn({Json, req1})),
              ?expect(translate_proplist, % why is this mock needed?
                      ?withArgs([UpdatedPropList, fun deliv_user_json:translate_json_key/1]),
                      ?andReturn({ok, SamlPropList})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req1, State]),
                      ?andReturn({halt, req2, State}))]),
    hoax:mock(deliv_user,
              ?expect(insert,
                      ?withArgs([EntName, SamlPropList]),
                      ?andReturn({error, why}))),

    Actual = auth_hand_saml_users:from_json(req, State),

    ?verifyAll,
    ?assertEqual({halt, req2, State}, Actual).

from_json_returns_409_conflict_if_a_user_with_this_name_already_exists() ->
    deliv_json:init_schemas(),
    EntName = <<"foo">>,
    State = #handler{ent_name = EntName},
    Json = {[
        {<<"name">>,<<"someone">>}
        ]},
    SamlPropList =  translated_prop_list_with_saml_user_type,
    {UpdatedPropList} = ej:set({"user_type"}, Json, <<"saml">>),

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req]),
                      ?andReturn({Json, req1})),
              ?expect(translate_proplist, % why is this mock needed?
                      ?withArgs([UpdatedPropList, fun deliv_user_json:translate_json_key/1]),
                      ?andReturn({ok, SamlPropList})),
              ?expect(error_response,
                      ?withArgs([409, conflict, req1, State]),
                      ?andReturn({halt, req2, State}))]),
    hoax:mock(deliv_user,
              ?expect(insert,
                      ?withArgs([EntName, SamlPropList]),
                      ?andReturn({error, conflict}))),

    Actual = auth_hand_saml_users:from_json(req, State),

    ?assertEqual({halt, req2, State}, Actual),
    ?verifyAll.

from_json_is_successful_if_there_are_no_errors() ->
    deliv_json:init_schemas(),
    EntName = <<"foo">>,
    State = #handler{ent_name = EntName},
    Json = {[
                {<<"name">>,<<"someone">>},
                {<<"ssh_pub_key">>, <<"zomg">>}
            ]},

    SamlPropList =  translated_prop_list_with_saml_user_type,
    {UpdatedPropList} = ej:set({"user_type"}, Json, <<"saml">>),

    hoax:mock(deliv_web_utils, [
              ?expect(parse_json_req,
                      ?withArgs([req]),
                      ?andReturn({Json, req1})),
              ?expect(translate_proplist,
                      ?withArgs([UpdatedPropList, fun deliv_user_json:translate_json_key/1]),
                      ?andReturn({ok, SamlPropList})),
              ?expect(href,
                      ?withArgs([<<"foo">>, <<"/users/someone">>]),
                      ?andReturn(link)),
              ?expect(make_hal,
                      ?withArgs([[{<<"full">>, link}]]),
                      ?andReturn(hal)),
              ?expect(set_json_body,
                      ?withArgs([{[{<<"_links">>, hal}]}, req1]),
                      ?andReturn(req2))]),
    hoax:mock(deliv_user, [
              ?expect(insert,
                      ?withArgs([EntName, SamlPropList]),
                      ?andReturn([user])),
              ?expect(getval,
                      ?withArgs([name, user]),
                      ?andReturn(<<"someone">>))]),

    Actual = auth_hand_saml_users:from_json(req, State),

    ?assertEqual({{true, link}, req2, State}, Actual),
    ?verifyAll.
