-module(deliv_intern_user_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

reset_password_test_() ->
    [
     eunit_sugar:parameterized_fixture(?MODULE, disable_login_and_generate_password_reset_token_, setup, teardown),
     eunit_sugar:parameterized_fixture(?MODULE, reset_password_, setup, teardown),
     eunit_sugar:parameterized_fixture(?MODULE, unset_password_, setup, teardown)
    ].

setup() ->
    application:set_env(delivery, erlpass_work_factor, 4),
    ok = delivery_app:start_app_with_deps(erlpass),
    error_logger:tty(false),
    eu_database:setup(),
    eu_data:with_enterprise(test_enterprise_name(),
        fun(Enterprise) ->
            UserName = <<"deliv_hand_reset_password_test_user">>,
            eu_data:fetch_or_create_user(Enterprise, UserName),
            {test_enterprise_name(), UserName}
        end).

teardown(_) ->
    eu_database:teardown(),
    application:stop(erlpass),
    error_logger:tty(true).

test_enterprise_name() ->
    <<"deliv_hand_reset_password_test_enterprise">>.

disable_login_and_generate_password_reset_token_generates_a_token_and_unsets_the_password_and_returns_the_token_that_can_be_used_to_reset_the_password({EntName, UserName}) ->
    %% setup
    OldPassword = <<"password">>,
    deliv_intern_user:reset_password(EntName, UserName, OldPassword),

    {ok, Token} = deliv_intern_user:disable_login_and_generate_password_reset_token(EntName, UserName),
    ?assertEqual(denied, deliv_user:verify_password(<<"internal">>, UserName, EntName, OldPassword)),
    NewPassword = <<"newpassword">>,
    ?assertMatch({ok, _}, deliv_reset_password:consume_reset_token(EntName, UserName, Token, NewPassword)),
    ?assertEqual(verified, deliv_user:verify_password(<<"internal">>, UserName, EntName, NewPassword)).

reset_password_should_change_password({EntName, UserName}) ->
    deliv_intern_user:reset_password(EntName, UserName, <<"newpassword">>),
    Actual = deliv_user:verify_password(<<"internal">>, UserName, EntName, <<"newpassword">>),
    ?assertEqual(verified, Actual).

unset_password_makes_it_impossible_to_verify_the_old_password({EntName, UserName}) ->
    NewPassword = <<"newpassword">>,
    deliv_intern_user:reset_password(EntName, UserName, NewPassword),
    ?assertEqual(ok, deliv_intern_user:unset_password(EntName, UserName)),
    ?assertEqual(denied, deliv_user:verify_password(<<"internal">>, UserName, EntName, NewPassword)).
