-module(deliv_intern_user_tests).

-include_lib("hoax/include/hoax.hrl").
-include("deliv_types.hrl").

-compile(export_all).

deliv_intern_user_test_() ->
    [
     hoax:fixture(?MODULE, disable_login_and_generate_password_reset_token_),
     hoax:fixture(?MODULE, reset_password_),
     hoax:fixture(?MODULE, unset_password_)
    ].

disable_login_and_generate_password_reset_token_when_reset_token_generation_fails_returns_error() ->
    EntName = <<"Ent">>,
    UserName = <<"Ada">>,
    hoax:expect(receive
                    deliv_reset_password:generate_reset_token(EntName, UserName) -> {error, reason}
                end),
    Actual = deliv_intern_user:disable_login_and_generate_password_reset_token(EntName, UserName),
    ?assertEqual({error, reason}, Actual),
    ?verifyAll.

disable_login_and_generate_password_reset_token_when_user_is_not_found_returns_error() ->
    EntName = <<"Ent">>,
    UserName = <<"Ada">>,
    hoax:expect(receive
                    deliv_reset_password:generate_reset_token(EntName, UserName) -> {ok, token};
                    sqerl_rec:scalar_fetch(deliv_intern_user, invalidate_passwords, [EntName, UserName]) -> {error, reason}
                end),
    Actual = deliv_intern_user:disable_login_and_generate_password_reset_token(EntName, UserName),
    ?assertEqual({error, reason}, Actual),
    ?verifyAll.

reset_password_when_user_is_not_found_returns_error() ->
    EntName = <<"Ent">>,
    UserName = <<"Ada">>,
    Password = <<"@d@">>,
    hoax:expect(receive
                    deliv_db:fetch(deliv_intern_user, [EntName], UserName) -> {error, not_found}
                end),

    Actual = deliv_intern_user:reset_password(EntName, UserName, Password),
    ?assertEqual({error, user_not_found}, Actual),
    ?verifyAll.

reset_password_when_password_hash_fails_returns_error() ->
    EntName = <<"Ent">>,
    UserName = <<"Ada">>,
    Password = <<"@d@">>,
    hoax:expect(receive
                    deliv_db:fetch(deliv_intern_user, [EntName], UserName) -> {ok, user};
                    user_password:hash(Password) -> {error, reason, message}
                end),

    Actual = deliv_intern_user:reset_password(EntName, UserName, Password),
    ?assertEqual({error, bad_password, reason, message}, Actual),
    ?verifyAll.

reset_password_saves_the_updated_user_record_and_returns_the_result() ->
    EntName = <<"Ent">>,
    UserName = <<"Ada">>,
    Password = <<"@d@">>,
    OldPassHashed = <<"oldhash">>,
    NewPassHashed = <<"Haaaaaash">>,
    User = deliv_intern_user:fromlist([{hashed_pass, OldPassHashed}]),
    UpdatedUser = deliv_intern_user:fromlist([{hashed_pass, NewPassHashed},
                                              {hash_type, <<"bcrypt">>}]),
    hoax:expect(receive
                    deliv_db:fetch(deliv_intern_user, [EntName], UserName) -> {ok, User};
                    user_password:hash(Password) -> NewPassHashed;
                    deliv_db:update(UpdatedUser) -> returned
                end),

    Actual = deliv_intern_user:reset_password(EntName, UserName, Password),
    ?assertEqual(returned, Actual),
    ?verifyAll.

unset_password_calls_stored_function_and_when_successful_returns_ok() ->
    EntName = <<"Ent">>,
    UserName = <<"Ada">>,
    hoax:expect(receive
                    sqerl_rec:scalar_fetch(deliv_intern_user, invalidate_passwords, [EntName, UserName]) -> [<<>>]
                end),
    Actual = deliv_intern_user:unset_password(EntName, UserName),
    ?assertEqual(ok, Actual),
    ?verifyAll.

unset_password_calls_stored_function_and_when_unsuccessful_returns_that_error() ->
    EntName = <<"Ent">>,
    UserName = <<"Ada">>,
    hoax:expect(receive
                    sqerl_rec:scalar_fetch(deliv_intern_user, invalidate_passwords, [EntName, UserName]) -> {error, reason}
                end),
    Actual = deliv_intern_user:unset_password(EntName, UserName),
    ?assertEqual({error, reason}, Actual),
    ?verifyAll.
