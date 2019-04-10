-module(deliv_reset_password_tests).

-include_lib("hoax/include/hoax.hrl").
-include("deliv_types.hrl").

-compile(export_all).

default_expiry() -> {{2, 0, 0}, 0, 0}.

handle_test_() ->
    [
     hoax:fixture(?MODULE, generate_reset_token_),
     hoax:fixture(?MODULE, consume_reset_token_),
     hoax:fixture(?MODULE, reset_url)
    ].

generate_reset_token_when_token_assignment_succeeds_returns_ok_token() ->
    Token = <<"token_base64url">>,
    EntName = <<"enterprise">>,
    UserName = <<"alice">>,
    hoax:expect(receive
                    deliv_token:new() -> Token;
                    sqerl_rec:scalar_fetch(deliv_intern_user, assign_password_reset_token,
                                           [EntName, UserName, Token, default_expiry()]) -> [ignored]
                end),
    Actual = deliv_reset_password:generate_reset_token(EntName, UserName),
    ?assertEqual({ok, Token}, Actual),
    ?verifyAll.

generate_reset_token_when_token_assignment_fails_with_not_found_returns_error() ->
    Token = <<"token_base64url">>,
    EntName = <<"enterprise">>,
    UserName = <<"alice">>,
    hoax:expect(receive
                    deliv_token:new() -> Token;
                    sqerl_rec:scalar_fetch(deliv_intern_user, assign_password_reset_token,
                                           [EntName, UserName, Token, default_expiry()]) -> []
                end),
    Actual = deliv_reset_password:generate_reset_token(EntName, UserName),
    ?assertEqual({error, token_not_added}, Actual),
    ?verifyAll.

generate_reset_token_when_token_assignment_fails_with_some_db_error_returns_that_error() ->
    Token = <<"token_base64url">>,
    EntName = <<"enterprise">>,
    UserName = <<"alice">>,
    hoax:expect(receive
                    deliv_token:new() -> Token;
                    sqerl_rec:scalar_fetch(deliv_intern_user, assign_password_reset_token,
                                           [EntName, UserName, Token, default_expiry()]) -> {error, reason}
                end),
    Actual = deliv_reset_password:generate_reset_token(EntName, UserName),
    ?assertEqual({error, reason}, Actual),
    ?verifyAll.

consume_reset_token_when_token_is_found_will_try_to_reset_password() ->
    Token = <<"token_base64url">>,
    NewPassword = <<":wee:">>,
    EntName = <<"enterprise">>,
    UserName = <<"alice">>,
    hoax:expect(receive
                    sqerl_rec:scalar_fetch(deliv_intern_user, use_password_reset_token,
                                           [EntName, UserName, Token]) -> {ok, [tok1, tok2, Token]};
                    deliv_intern_user:reset_password(EntName, UserName, NewPassword) -> {ok, user}
                end),
    Actual = deliv_reset_password:consume_reset_token(EntName, UserName, Token, NewPassword),
    ?assertEqual({ok, user}, Actual),
    ?verifyAll.

consume_reset_token_when_token_is_not_found_will_return_error() ->
    Token = <<"token_base64url">>,
    NewPassword = <<":wee:">>,
    EntName = <<"enterprise">>,
    UserName = <<"alice">>,
    hoax:expect(receive
                    sqerl_rec:scalar_fetch(deliv_intern_user, use_password_reset_token,
                                           [EntName, UserName, Token]) -> []
                end),
    Actual = deliv_reset_password:consume_reset_token(EntName, UserName, Token, NewPassword),
    ?assertEqual({error, token_not_found}, Actual),
    ?verifyAll.

consume_reset_token_when_token_is_not_found_postgres_96_style_will_return_error() ->
    Token = <<"token_base64url">>,
    NewPassword = <<":wee:">>,
    EntName = <<"enterprise">>,
    UserName = <<"alice">>,
    hoax:expect(receive
                    sqerl_rec:scalar_fetch(deliv_intern_user, use_password_reset_token,
                                           [EntName, UserName, Token]) -> [null]
                end),
    Actual = deliv_reset_password:consume_reset_token(EntName, UserName, Token, NewPassword),
    ?assertEqual({error, token_not_found}, Actual),
    ?verifyAll.

consume_reset_token_when_db_error_occurs_returns_that_error() ->
    Token = <<"token_base64url">>,
    NewPassword = <<":wee:">>,
    EntName = <<"enterprise">>,
    UserName = <<"alice">>,
    hoax:expect(receive
                    sqerl_rec:scalar_fetch(deliv_intern_user, use_password_reset_token,
                                           [EntName, UserName, Token]) -> {error, reason}
                end),
    Actual = deliv_reset_password:consume_reset_token(EntName, UserName, Token, NewPassword),
    ?assertEqual({error, reason}, Actual),
    ?verifyAll.

reset_url_returns_url_of_password_reset_page() ->
    EntName = <<"enterprise">>,
    UserName = <<"alice">>,
    Token = <<"token_base64url">>,
    hoax:expect(receive
                    deliv_web_utils:protocol() -> "https";
                    deliv_web_utils:hostname() -> "delivery.fqdn"
                end),
    ?assertEqual(<<"https://delivery.fqdn/e/enterprise/#/reset-password/alice/token_base64url">>,
                 deliv_reset_password:reset_url(EntName, UserName, Token)),
    ?verifyAll.
