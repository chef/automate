-module(deliv_reset_password_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

reset_test_() ->
    [
     eunit_sugar:parameterized_fixture(?MODULE, generate_reset_token_, setup, teardown),
     eunit_sugar:parameterized_fixture(?MODULE, consume_reset_token_, setup, teardown)
    ].

setup() ->
    application:set_env(delivery, erlpass_work_factor, 4),
    ok = delivery_app:start_app_with_deps(erlpass),
    error_logger:tty(false),
    eu_database:setup(),
    eu_data:with_enterprise(test_enterprise_name(),
        fun(Enterprise) ->
            UserName = <<"deliv_reset_password_test_user">>,
            User = eu_data:fetch_or_create_user(Enterprise, UserName),
            {test_enterprise_name(), UserName, User}
        end).

teardown(_) ->
    eu_database:teardown(),
    application:stop(erlpass),
    error_logger:tty(true).

test_enterprise_name() ->
    <<"deliv_reset_password_test_enterprise">>.

generate_reset_token_returns_token_that_can_be_used_to_set_a_new_password({EntName, UserName, _}) ->
    NewPassword = <<":wee:">>,
    {ok, Token} = deliv_reset_password:generate_reset_token(EntName, UserName),
    {ok, _NewUser} = deliv_reset_password:consume_reset_token(EntName, UserName, Token, NewPassword),
    Actual = deliv_user:verify_password(<<"internal">>, UserName, EntName, NewPassword),
    ?assertMatch(verified, Actual).

generate_reset_token_can_be_used_multiple_times_and_first_token_can_be_used({EntName, UserName, _}) ->
    NewPassword = <<":wee:">>,
    {ok, FirstToken} = deliv_reset_password:generate_reset_token(EntName, UserName),
    {ok, _SecondToken} = deliv_reset_password:generate_reset_token(EntName, UserName),
    {ok, _NewUser} = deliv_reset_password:consume_reset_token(EntName, UserName, FirstToken, NewPassword),
    Actual = deliv_user:verify_password(<<"internal">>, UserName, EntName, NewPassword),
    ?assertMatch(verified, Actual).

generate_reset_token_can_be_used_multiple_times_and_second_token_can_be_used({EntName, UserName, _}) ->
    NewPassword = <<":wee:">>,
    {ok, _FirstToken} = deliv_reset_password:generate_reset_token(EntName, UserName),
    {ok, SecondToken} = deliv_reset_password:generate_reset_token(EntName, UserName),
    {ok, _NewUser} = deliv_reset_password:consume_reset_token(EntName, UserName, SecondToken, NewPassword),
    Actual = deliv_user:verify_password(<<"internal">>, UserName, EntName, NewPassword),
    ?assertMatch(verified, Actual).

generate_reset_token_when_used_with_nonexisting_user_returns_error({EntName, _UserName, _}) ->
    UserName = <<"alice">>,
    Actual = deliv_reset_password:generate_reset_token(EntName, UserName),
    ?assertMatch({error, token_not_added}, Actual).

consume_reset_token_with_unknown_token_returns_error({EntName, UserName, _}) ->
    NewPassword = <<":wee:">>,
    Token = <<"good guess">>,
    ?assertEqual({error, token_not_found}, deliv_reset_password:consume_reset_token(EntName, UserName, Token, NewPassword)).

consume_reset_token_with_token_that_was_already_used_returns_error({EntName, UserName, _}) ->
    NewPassword = <<":wee:">>,
    NewerPassword = <<":woohoo:">>,
    {ok, Token} = deliv_reset_password:generate_reset_token(EntName, UserName),
    {ok, _NewUser} = deliv_reset_password:consume_reset_token(EntName, UserName, Token, NewPassword),
    Actual = deliv_reset_password:consume_reset_token(EntName, UserName, Token, NewerPassword),
    ?assertEqual({error, token_not_found}, Actual),
    ?assertEqual(verified, deliv_user:verify_password(<<"internal">>, UserName, EntName, NewPassword)),
    ?assertMatch(denied, deliv_user:verify_password(<<"internal">>, UserName, EntName, NewerPassword)).

consume_reset_token_with_token_when_another_token_was_already_used_returns_error({EntName, UserName, _}) ->
    NewPassword = <<":wee:">>,
    {ok, UseToken} = deliv_reset_password:generate_reset_token(EntName, UserName),
    {ok, NotUseToken} = deliv_reset_password:generate_reset_token(EntName, UserName),
    {ok, _NewUser} = deliv_reset_password:consume_reset_token(EntName, UserName, UseToken, NewPassword),
    Actual = deliv_reset_password:consume_reset_token(EntName, UserName, NotUseToken, NewPassword),
    ?assertEqual({error, token_not_found}, Actual).

consume_reset_token_with_token_that_has_expired_returns_error({EntName, UserName, User}) ->
    Id = deliv_user:getval(id, User),
    NewPassword = <<":wee:">>,
    Token = <<"sometokenvalue">>,
    Query = <<"INSERT INTO password_reset_tokens (user_id, token, expiry)",
              "VALUES ( ", (erlang:integer_to_binary(Id))/binary, ", ",
                        "'", Token/binary, "', ",
                        "'2016-01-01'::timestamp )">>,
    {ok, _} = sqerl:execute(Query),
    Actual = deliv_reset_password:consume_reset_token(EntName, UserName, Token, NewPassword),
    ?assertEqual({error, token_not_found}, Actual).

consume_reset_token_with_fresh_token_when_there_already_is_a_token_that_has_expired_is_successful({EntName, UserName, User}) ->
    Id = deliv_user:getval(id, User),
    NewPassword = <<":wee:">>,
    Token = <<"sometokenvalue">>,
    Query = <<"INSERT INTO password_reset_tokens (user_id, token, expiry)",
              "VALUES ( ", (erlang:integer_to_binary(Id))/binary, ", ",
                        "'", Token/binary, "', ",
                        "'2016-01-01'::timestamp )">>,
    {ok, _} = sqerl:execute(Query),
    {ok, NewToken} = deliv_reset_password:generate_reset_token(EntName, UserName),
    {ok, _NewUser} = deliv_reset_password:consume_reset_token(EntName, UserName, NewToken, NewPassword),
    Actual = deliv_user:verify_password(<<"internal">>, UserName, EntName, NewPassword),
    ?assertEqual(verified, Actual).
