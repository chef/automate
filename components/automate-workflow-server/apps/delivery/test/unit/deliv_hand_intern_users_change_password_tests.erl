-module(deliv_hand_intern_users_change_password_tests).

-include_lib("hoax/include/hoax.hrl").
-include("deliv_types.hrl").

-compile(export_all).

content_types_accepted_accepts_json_test() ->
    ExpectedContentType = [{{<<"application">>,<<"json">>,'*'}, handle}],
    ?assertEqual({ExpectedContentType, req, state},
                 deliv_hand_intern_users_change_password:content_types_accepted(req, state)).

allowed_methods_returns_post_test() ->
    ?assertEqual({[<<"POST">>], req, state},
                 deliv_hand_intern_users_change_password:allowed_methods(req, state)).

handle_test_() ->
    hoax:fixture(?MODULE, "handle_").

reset_password_test_() ->
    hoax:fixture(?MODULE, "reset_password_").

handle_when_json_is_invalid_returns_bad_request() ->
    UserName = <<"Alice">>,
    EntName = <<"ENT">>,
    State = #handler{ent_name = EntName},
    hoax:expect(receive
                    cowboy_req:binding(user_name, req) -> {UserName, req1};
                    deliv_web_utils:parse_json_req(req1, ?any) -> {{error, reason}, req2};
                    deliv_web_utils:error_response(400, bad_request, req2, State) -> returned
                end),

    Actual = deliv_hand_intern_users_change_password:handle(req, State),
    ?assertEqual(returned, Actual),
    ?verifyAll.

handle_when_json_is_valid_and_deliv_intern_user_returns_not_found_error_returns_404() ->
    UserName = <<"Alice">>,
    EntName = <<"ENT">>,
    State = #handler{ent_name = EntName},
    Password = <<"@li<e">>,
    Json = {[{<<"password">>, Password}]},
    hoax:expect(receive
                    cowboy_req:binding(user_name, req) -> {UserName, req1};
                    deliv_web_utils:parse_json_req(req1, ?any) -> {Json, req2};
                    deliv_intern_user:reset_password(EntName, UserName, Password) -> {error, user_not_found};
                    deliv_web_utils:error_response(404, not_found, req2, State) -> returned
                end),

    Actual = deliv_hand_intern_users_change_password:handle(req, State),
    ?assertEqual(returned, Actual),
    ?verifyAll.

handle_when_json_is_valid_and_deliv_intern_user_returns_bad_password_returns_400() ->
    UserName = <<"Alice">>,
    EntName = <<"ENT">>,
    State = #handler{ent_name = EntName},
    Password = <<"@li<e">>,
    Json = {[{<<"password">>, Password}]},
    hoax:expect(receive
                    cowboy_req:binding(user_name, req) -> {UserName, req1};
                    deliv_web_utils:parse_json_req(req1, ?any) -> {Json, req2};
                    deliv_intern_user:reset_password(EntName, UserName, Password) -> {error, bad_password, reason, message};
                    deliv_web_utils:error_response(400, reason, message, req2, State) -> returned
                end),

    Actual = deliv_hand_intern_users_change_password:handle(req, State),
    ?assertEqual(returned, Actual),
    ?verifyAll.

handle_when_json_is_valid_and_deliv_intern_user_returns_some_other_error_returns_500() ->
    UserName = <<"Alice">>,
    EntName = <<"ENT">>,
    State = #handler{ent_name = EntName},
    Password = <<"@li<e">>,
    Json = {[{<<"password">>, Password}]},
    hoax:expect(receive
                    cowboy_req:binding(user_name, req) -> {UserName, req1};
                    deliv_web_utils:parse_json_req(req1, ?any) -> {Json, req2};
                    deliv_intern_user:reset_password(EntName, UserName, Password) -> not_expected;
                    deliv_web_utils:error_response(500, internal_server_error, req2, State) -> returned
                end),

    Actual = deliv_hand_intern_users_change_password:handle(req, State),
    ?assertEqual(returned, Actual),
    ?verifyAll.

handle_when_password_is_changed_successfully_returns_true() ->
    UserName = <<"Alice">>,
    EntName = <<"ENT">>,
    State = #handler{ent_name = EntName},
    Password = <<"@li<e">>,
    Json = {[{<<"password">>, Password}]},
    hoax:expect(receive
                    cowboy_req:binding(user_name, req) -> {UserName, req1};
                    deliv_web_utils:parse_json_req(req1, ?any) -> {Json, req2};
                    deliv_intern_user:reset_password(EntName, UserName, Password) -> {ok, new_user}
                end),

    Actual = deliv_hand_intern_users_change_password:handle(req, State),
    ?assertEqual({true, req2, State}, Actual),
    ?verifyAll.
