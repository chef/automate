%% @doc This module is used by `deliv_ssh_git' to manage both the server's and
%% the users' keys
%% see http://www.erlang.org/doc/man/ssh_server_key_api.html
-module(deliv_ssh_key_cb).

-include("deliv_types.hrl").

-behaviour(ssh_server_key_api).

-export([
        extract_user_ent/1
       ]).

%% behavior callbacks
-export([
         host_key/2,
         is_auth_key/3
        ]).

%% @doc Nothing fancy here, we just re-use the vanilla implementation
host_key(Algorithm, Opts) ->
    ssh_file:host_key(Algorithm, Opts).

is_auth_key(PubKey, SshUserName, _DaemonOptions) ->
    case extract_user_ent(SshUserName) of
        {UserName, EntName} ->
            chef_log:debug("Fetching ~p @ ~p", [UserName, EntName]),
            is_auth_key_for_user(
                deliv_user:fetch(EntName, UserName),
                SshUserName,
                PubKey
            );
        error ->
            chef_log:info("Could not extract ent and user from SSH username ~p",
                           [SshUserName]),
            false
    end.

%% @doc Extract user's name and ent from their SSH login
%% which should be of the form user@ent
%% Reminder: users need to encode (in the `deliv_encode' sense) their user and/or
%% ent name if those contain any fancy character
-spec extract_user_ent(str_or_binary()) -> {UserName, EntName} | error when
    UserName :: binary(),
    EntName :: binary().
extract_user_ent(SshUserName) ->
    DecodedSshUserName = deliv_encode:decode(SshUserName),
    case lists:reverse(string:tokens(chef_utils:to_str(DecodedSshUserName), "@")) of
        [EntName | Rest] ->
            Username = string:join(lists:reverse(Rest), "@"),
            {chef_utils:to_bin(Username), chef_utils:to_bin(EntName)};
        _ ->
            error
    end.

is_auth_key_for_user({ok, User}, SshUserName, PubKey) ->
    chef_log:debug("Found user # ~p for SSH username ~p",
                    [deliv_user:getval(id, User), SshUserName]),
    case pubkey_from_user(User) of
        undefined -> false;
        UserPubKey -> UserPubKey =:= PubKey
    end;
is_auth_key_for_user(Other, SshUserName, _PubKey) ->
    chef_log:info("Could not find user for SSH username ~p (~p)",
                   [SshUserName, Other]),
    false.

%% @private
%% @doc Returns a `public_key:public_key()' record, or `undefined'
pubkey_from_user(User) ->
    extract_key(deliv_user:getval(ssh_pub_key, User), User).

extract_key(undefined, _) ->
    undefined;
extract_key(KeyData, User) ->
    case deliv_public_key:parse(KeyData) of
        {error, bad_key} ->
            Name = deliv_user:getval(ssh_pub_key, User),
            chef_log:info("unable to parse public key data for "
                           "user '~s': ~p", [Name, KeyData]),
            undefined;
        ParsedKey ->
            ParsedKey
    end.
