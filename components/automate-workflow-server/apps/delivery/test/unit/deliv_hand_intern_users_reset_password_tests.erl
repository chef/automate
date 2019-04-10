-module(deliv_hand_intern_users_reset_password_tests).

-include_lib("hoax/include/hoax.hrl").
-include("deliv_types.hrl").

-compile(export_all).

content_types_accepted_accepts_json_test() ->
    ExpectedContentType = [{{<<"application">>,<<"json">>,'*'}, handle}],
    ?assertEqual({ExpectedContentType, req, state},
                 deliv_hand_intern_users_reset_password:content_types_accepted(req, state)).

allowed_methods_returns_post_test() ->
    ?assertEqual({[<<"POST">>], req, state},
                 deliv_hand_intern_users_reset_password:allowed_methods(req, state)).

handle_test_() ->
    hoax:fixture(?MODULE, "handle_").

reset_password_test_() ->
    hoax:fixture(?MODULE, "reset_password_").

handle_when_json_is_invalid_returns_bad_request() ->
    UserName = <<"Alice">>,
    EntName = <<"ENT">>,
    hoax:expect(receive
                    deliv_web_utils:extract_bindings([ent_name, user_name], req) -> {[EntName, UserName], req1};
                    deliv_web_utils:parse_json_req(req1, ?any) -> {{error, reason}, req2};
                    deliv_web_utils:error_response(400, bad_request, req2, state) -> returned
                end),

    Actual = deliv_hand_intern_users_reset_password:handle(req, state),
    ?assertEqual(returned, Actual),
    ?verifyAll.

handle_when_json_is_valid_and_deliv_reset_password_returns_token_not_found_error_returns_401() ->
    UserName = <<"Alice">>,
    EntName = <<"ENT">>,
    Password = <<"@li<e">>,
    Token = <<"TOKEN">>,
    Json = {[{<<"password">>, Password}, {<<"token">>, Token}]},
    hoax:expect(receive
                    deliv_web_utils:extract_bindings([ent_name, user_name], req) -> {[EntName, UserName], req1};
                    deliv_web_utils:parse_json_req(req1, ?any) -> {Json, req2};
                    deliv_reset_password:consume_reset_token(EntName, UserName, Token, Password) -> {error, token_not_found};
                    deliv_web_utils:error_response(401, not_authorized, req2, state) -> returned
                end),

    Actual = deliv_hand_intern_users_reset_password:handle(req, state),
    ?assertEqual(returned, Actual),
    ?verifyAll.

handle_when_json_is_valid_and_deliv_reset_password_returns_password_error_returns_400_with_message() ->
    UserName = <<"Alice">>,
    EntName = <<"ENT">>,
    Password = <<"@li<e">>,
    Token = <<"TOKEN">>,
    Json = {[{<<"password">>, Password}, {<<"token">>, Token}]},
    Message = <<"Password length must be greater than 3 characters.">>,
    hoax:expect(receive
                    deliv_web_utils:extract_bindings([ent_name, user_name], req) -> {[EntName, UserName], req1};
                    deliv_web_utils:parse_json_req(req1, ?any) -> {Json, req2};
                    deliv_reset_password:consume_reset_token(EntName, UserName, Token, Password) ->
                        {error, bad_password, reason, Message};
                    deliv_web_utils:error_response(400, reason, Message, req2, state) -> returned
                end),

    Actual = deliv_hand_intern_users_reset_password:handle(req, state),
    ?assertEqual(returned, Actual),
    ?verifyAll.

handle_when_json_is_valid_and_deliv_reset_password_returns_some_other_error_returns_500() ->
    UserName = <<"Alice">>,
    EntName = <<"ENT">>,
    Password = <<"@li<e">>,
    Token = <<"TOKEN">>,
    Json = {[{<<"password">>, Password}, {<<"token">>, Token}]},
    hoax:expect(receive
                    deliv_web_utils:extract_bindings([ent_name, user_name], req) -> {[EntName, UserName], req1};
                    deliv_web_utils:parse_json_req(req1, ?any) -> {Json, req2};
                    deliv_reset_password:consume_reset_token(EntName, UserName, Token, Password) -> not_expected;
                    deliv_web_utils:error_response(500, internal_server_error, req2, state) -> returned
                end),

    Actual = deliv_hand_intern_users_reset_password:handle(req, state),
    ?assertEqual(returned, Actual),
    ?verifyAll.

handle_when_token_is_consumed_successfully_returns_true() ->
    UserName = <<"Alice">>,
    EntName = <<"ENT">>,
    Password = <<"@li<e">>,
    Token = <<"TOKEN">>,
    Json = {[{<<"password">>, Password}, {<<"token">>, Token}]},
    hoax:expect(receive
                    deliv_web_utils:extract_bindings([ent_name, user_name], req) -> {[EntName, UserName], req1};
                    deliv_web_utils:parse_json_req(req1, ?any) -> {Json, req2};
                    deliv_reset_password:consume_reset_token(EntName, UserName, Token, Password) -> {ok, user}
                end),

    Actual = deliv_hand_intern_users_reset_password:handle(req, state),
    ?assertEqual({true, req2, state}, Actual),
    ?verifyAll.
