%% Copyright 2014 Opscode, Inc. All Rights Reserved.

%% @doc Handles generating and checking the users' passwords
%% Uses bcrypt, accepts both lists and binary strings, and returns whatever type was passed.
%% Shamelessly copied from https://github.com/opscode/chef_wm/blob/master/src/chef_wm_password.erl

-module(user_password).

-export([hash/1,
         match/2,
         change/2,
         change/3,
         random/1]).

-include("deliv_types.hrl").

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(ERLPASS_WORK_FACTOR, 12).
-define(MIN_PASSWORD_LENGTH, 3).

%% @doc Return hashed password and salt. Uses bcrypt with
%% the number of rounds as configured in the bcrypt application key
%% `default_log_rounds'. Encrypting an empty password returns an empty string.
-spec hash(Password) -> binary() | {error, _Reason, _Message} when
      Password :: str_or_binary().
hash(Password) ->
  case validate_password_constraints(Password) of
    ok ->
      erlpass:hash(Password, work_factor_from_config());
    {error, Reason, Message} ->
      {error, Reason, Message}
  end.

work_factor_from_config() ->
    application:get_env(delivery, erlpass_work_factor,
                        ?ERLPASS_WORK_FACTOR).

%% @doc Return `true' if the plain password matches with the hash and salt
-spec match(Password, Hash) -> ok | {error, _Reason, _Message} when
      Password :: str_or_binary(),
      Hash :: binary().
% an empty hash doesn't match anything
match(_Password, Hash) when Hash =:= "";
                            Hash =:= <<"">>;
                            Hash =:= undefined;
                            Hash =:= null ->
    {error, bad_password, <<"Password incorrect.">>};
match(Password, _Hash) when Password =:= "";
                            Password =:= <<"">>;
                            Password =:= undefined ->
    {error, bad_password, <<"Password incorrect.">>};
match(Password, Hash) when erlang:is_binary(Password);erlang:is_list(Password)
                       andalso erlang:is_binary(Hash) ->
    case erlpass:match(Password, Hash) of
        false ->
            {error, bad_password, <<"Password incorrect.">>};
        true ->
            ok
    end.

%% @doc rehashes the password with a new work factor.
-spec change(Password, Hash) -> binary() | {error, _Reason, _Message} when
      Password :: str_or_binary(),
      Hash :: binary().
change(Password, Hash) ->
    change(Password, Hash, Password).

%% @doc changes the password if the old password matches the hash.
-spec change(OldPassword, Hash, NewPassword) ->
      binary() | {error, _Reason, _Message} when OldPassword :: str_or_binary(),
                                                 Hash :: binary(),
                                                 NewPassword :: str_or_binary().
change(_Password, Hash, _NewPassword) when Hash =:= "";
                                           Hash =:= <<"">>;
                                           Hash =:= undefined ->
    {error, bad_password, <<"Password incorrect.">>};
change(Password, _Hash, _NewPassword) when Password =:= "";
                                           Password =:= <<"">>;
                                           Password =:= undefined ->
    {error, bad_password, <<"Password incorrect.">>};
change(OldPassword, Hash, NewPassword) ->
    case validate_password_constraints(NewPassword) of
      ok ->
          case erlpass:change(OldPassword, Hash, NewPassword,
                              work_factor_from_config()) of
          {error, bad_password} ->
            {error, bad_password, <<"Password incorrect.">>};
          HashedPass ->
            HashedPass
        end;
      {error, Reason, Message} ->
        {error, Reason, Message}
    end.

%% @doc Generate a random (not hashed) password of given length
-spec random(Length) -> binary() when
      Length :: integer().
random(Length) ->
    base64:encode(crypto:rand_bytes(Length)).

-spec validate_password_constraints(NewPassword) -> ok | {error, _Reason, _Message} when
      NewPassword :: str_or_binary().
validate_password_constraints(NewPassword) when NewPassword =:= "";
                                                NewPassword =:= <<"">>;
                                                NewPassword =:= undefined ->
    {error, invalid_password, <<"Password cannot be empty.">>};
validate_password_constraints(NewPassword) when erlang:is_list(NewPassword) ->
  validate_password_constraints(erlang:iolist_to_binary(NewPassword));
validate_password_constraints(NewPassword) when erlang:is_binary(NewPassword) ->
  case byte_size(NewPassword) < ?MIN_PASSWORD_LENGTH of
      true ->
          BinMinLength = integer_to_binary(?MIN_PASSWORD_LENGTH),
          {error, invalid_password, <<"Password length must be greater than ", BinMinLength/binary, " characters.">>};
      false ->
          ok
  end.
