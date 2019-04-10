%% Copyright 2014 Opscode, Inc. All Rights Reserved.

-module(user_password_tests).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun setup/0, fun cleanup/1, F}).

setup() ->
    %% turn the number of rounds way down so that running the tests
    %% doesn't take too long.
    application:set_env(delivery, erlpass_work_factor, 4),
    ok = delivery_app:start_app_with_deps(erlpass) .

cleanup(_) ->
    error_logger:tty(false),
    application:stop(erlpass),
    error_logger:tty(true).

encrypt_empty_test_() ->
    ?setup([
        {"refuses to encrypt an empty string nor atom `undefined'",
        [?_assertMatch(
            {error, invalid_password, _Message},
            user_password:hash(Password)
        ) || Password <- ["", undefined]]}
    ]).

%% tests that the result looks like a legit bcrypt hash
encrypt_password_test_() ->
    ?setup(
        fun(_) ->
            EncryptedPassword = user_password:hash("password"),
            [ Version, CostFactor, Cipher ] = string:tokens(erlang:binary_to_list(EncryptedPassword), "$"),
            [
                ?_assert(erlang:is_binary(EncryptedPassword)),
                ?_assertEqual(["2a", "04"], [Version, CostFactor]),
                ?_assertEqual(53, length(Cipher))
            ]
        end).

round_trip_test_() ->
    ?setup([
        ?_assertMatch(ok, user_password:match("password", user_password:hash("password")))
    ]).

wrong_password_test_() ->
    ?setup([
        ?_assertMatch({error, bad_password, _Message}, user_password:match("wrong", user_password:hash("password")))
    ]).

decrypt_test_() ->
    %% successfully verifies a previously recorded hash
    ?setup(
        fun(_) ->
            ?_assertMatch(
                ok,
                user_password:match("password",
                                    <<"$2a$04$Ug.Q3/FyJIBo3OSqhGmMP.4Ff743yqjHX53NqXvAwfmZxf0Bfx4RC">>))
        end).

empty_password_hash_match_test_() ->
    ?setup([
       {"never verifies an empty of `undefined' hash",
        [?_assertMatch(
            {error, bad_password, _Message},
            user_password:match("password", Hash)
        ) || Hash <- ["", <<"">>, undefined]]}
    ]).

empty_password_match_test_() ->
    ?setup([
       {"never verifies an empty of `undefined' password",
        [?_assertMatch(
            {error, bad_password, _Message},
            user_password:match(Password, user_password:hash("password"))
        ) || Password <- ["", <<"">>, undefined]]}
    ]).

change_password_empty_hash_test_() ->
    ?setup([
       {"never changes with an `undefined' hash",
        [?_assertMatch(
            {error, bad_password, _Message},
            user_password:change("password", Hash, "password")
        ) || Hash <- ["", <<"">>, undefined]]}
    ]).

change_password_empty_password_test_() ->
    ?setup([
        {"never changes with an `undefined' password",
         [?_assertMatch(
             {error, bad_password, _Message},
             user_password:change(Password,
                                  user_password:hash("password"),
                                  "new_password")
         ) || Password <- ["", <<"">>, undefined]]}
    ]).

change_password_empty_new_password_test_() ->
    ?setup([
        {"never changes with an `undefined' new password",
         [?_assertMatch(
             {error, invalid_password, _Message},
             user_password:change("password",
                                  user_password:hash("password"),
                                  NewPassword)
         ) || NewPassword <- ["", <<"">>, undefined]]}
    ]).

change_password_wrong_password_test_() ->
    ?setup([
       ?_assertMatch(
           {error, bad_password, _Message},
           user_password:change("wrong", user_password:hash("password"), "new_password")
       )
     ]).

change_password_round_trip_test_() ->
  ?setup(
      fun(_) ->
          EncryptedPassword = user_password:change("password", user_password:hash("password"), "new_password"),
          [ Version, CostFactor, Cipher ] = string:tokens(erlang:binary_to_list(EncryptedPassword), "$"),
          [
              ?_assert(erlang:is_binary(EncryptedPassword)),
              ?_assertEqual(["2a", "04"], [Version, CostFactor]),
              ?_assertEqual(53, length(Cipher))
          ]
      end).

%%tests that it returns binary
output_type_test_() ->
    ?setup([
        ?_assert(erlang:is_binary(user_password:hash("password"))),
        ?_assert(erlang:is_binary(user_password:hash(<<"password">>)))
    ]).
