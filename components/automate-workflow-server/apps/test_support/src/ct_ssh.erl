-module(ct_ssh).

-export([
        ssh_key_for_user/3
        ]).

-include_lib("delivery/include/deliv_types.hrl").

%% @doc Creates a SSH key pair on the disk, saves the pub key as being
%% `User''s in the DB, and returns an absolute path to the private key
%% Also returns a path to a script that you can point the `GIT_SSH' env var
%% to when using git to use that key (see `man git')
%% `TempDir' is where to put the stuff we generate
-spec ssh_key_for_user(User, Algo, TempDir) -> {PrivateKeyPath, GitSshScriptPath} when
    User :: d_user(),
    Algo :: dsa | rsa,
    TempDir :: string(),
    PrivateKeyPath :: string(),
    GitSshScriptPath :: string().
ssh_key_for_user(User, Algo, TempDir) ->
    %% create the key
    {PrivateKeyPath, DirPath} = create_ssh_key_pair(Algo, TempDir),
    %% then update the user with it
    {ok, PubKeyBin} = file:read_file(PrivateKeyPath ++ ".pub"),
    %% ugly, but eh :-/ we want this to work for both internal & external users
    Module = erlang:element(1, User),
    NewUser = Module:setvals([{ssh_pub_key, PubKeyBin}], User),
    {ok, NewUser} = Module:update(NewUser),
    %% and finally create the script for `GIT_SSH'
    ScriptPath = create_git_ssh_script(PrivateKeyPath, DirPath),
    {PrivateKeyPath, ScriptPath}.

%% @private
-spec create_ssh_key_pair(dsa | rsa, string()) -> {PrivateKeyPath, DirPath} when
    PrivateKeyPath :: string(),
    DirPath :: string().
create_ssh_key_pair(Algo, TempDir) when Algo =:= dsa orelse Algo =:= rsa ->
    RandomString = chef_utils:to_str(ct_utils:unique_string()),
    DirPath = filename:join([TempDir, "ssh_keys", RandomString]),
    %% usual trick for `filelib:ensure_dir' : trailing slash
    ok = filelib:ensure_dir(DirPath ++ "/"),
    %% then we actually create the pair
    KeyPath = filename:join(DirPath, "test_ssh_key"),
    StrAlgo = chef_utils:to_str(Algo),
    Cmd = ["ssh-keygen", "-t", StrAlgo, "-N", "''", "-f", KeyPath],
    {0, _} = chef_utils:run_cmd(Cmd),
    {KeyPath, DirPath}.

%% @private
-spec create_git_ssh_script(string(), string()) -> string().
create_git_ssh_script(PrivateKeyPath, DirPath) ->
    Content = ["#!/bin/sh\n"
               "ssh -o UserKnownHostsFile=/dev/null "
               "-o StrictHostKeyChecking=no "
               "-i '", PrivateKeyPath, "' "
               "\"$@\"\n"],
    ScriptPath = filename:join(DirPath, "git_ssh.sh"),
    ok = file:write_file(ScriptPath, Content),
    ok = file:change_mode(ScriptPath, 8#755),
    ScriptPath.
