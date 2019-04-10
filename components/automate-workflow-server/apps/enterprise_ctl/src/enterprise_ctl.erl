-module(enterprise_ctl).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([main/1]).

-define(DOCTOR_SSH_PUB_KEY, <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDBAODllGk/jNjA6xZ+IqgUC1wOm7pqqHoF3d06AmBXix0KtEn3SYacfJHsk+dQQP0OYtWinBQf5I+OUi7fPSrRhEHsSUjrJl7HRh4SkcQ2r8wIIHCyBk4Qb6DezOwuuq7TgP0Ac7HCpHBXFVlAWGhFbLu7ydYSpXSishgHRTLh4Q2NbxUXu4VORMIdFd3nGqai/bqAmV9fKCBbSz8ebfEw+yV8lju2s+wWgQyNDToIJtLdxSdXWxxjkPyBb9VpAJohXcvFHSe9fA7YZGvfHm2ixqT4sl6Whd7JAjCsHt5h4MI5e6KncSUjg0kEM3W1i+FYE9Ej7KOFGt+SCEYKOJG1 builder@delivery">>).
%% Note: this will become node name cdx_$PID@127.0.0.1
-define(SELF_PREFIX, "cdx_").
-define(SELF_HOST, "127.0.0.1").
-define(CD, 'delivery@127.0.0.1').
-define(TIMEOUT, infinity).

main(Input) ->
    {Arguments, Options} = parse_options(Input),
    ok = init_network(),
    Opts = set_option_values(Options),
    % When we generate a reasonably detectable
    % error reply, ensure that we exit non-zero
    case parse_input2(Arguments, Opts) of
        {error, _Any} ->
            halt(1);
        error ->
            halt(1);
        _ ->
            halt(0)
    end.

parse_options(Input) ->
    OptSpecList = [
                   {password, $p, "password", string, "Password"},
                   {password_file, $f, "password-file", string, "Password File"},
                   {builder_password, $b, "builder-password", string, "Builder Password"},
                   {builder_password_file, $B, "builder-password-file", string, "Builder Password File"},
                   {ssh_pub_key_file, $s, "ssh-pub-key-file", string, "SSH Public Key File"},
                   {user_type, $t, "user_type", {binary, <<"internal">>}, "User Type <internal (default)|external>"},
                   {roles, $r, "roles", {string, "observer"}, "Comma-delimited user roles"}
                  ],
    case getopt:parse(OptSpecList, Input) of
        {ok, {Options, Arguments}} ->
            {Arguments, Options};
        {error, {invalid_option, Option}} ->
            io:format("Invalid option: ~s~n", [Option]),
            halt(1)
    end.

set_option_values(Options) ->
    Options1 = set_password_from_file(password_file, password, Options),
    Options2 = set_password_randomly(password, Options1),
    Options3 = set_password_from_file(builder_password_file, builder_password, Options2),
    Options4 = set_password_randomly(builder_password, Options3),
    set_ssh_key_from_file(Options4).

set_option_unless(Key, FunValue, Options) ->
    case proplists:is_defined(Key, Options) of
        true ->
            Options;
        false ->
            [{Key, FunValue()}|Options]
    end.

safe_read_file(Description, File) ->
    case file:read_file(File) of
        {error, enoent} ->
            io:format("The ~s file ~s could not be found.~n", [Description, File]),
            halt(1);
        {error, Other} ->
            io:format("The ~s file ~s could not be read: ~p.~n", [Description, File, Other]),
            halt(1);
        {ok, Value } ->
            Value
    end.

set_password_from_file(FileKey, PasswordKey, Options) ->
    case proplists:get_value(FileKey, Options) of
        undefined ->
            Options;
        PasswordFile ->
            Fun = fun() -> safe_read_file("password", PasswordFile) end,
            set_option_unless(PasswordKey, Fun, Options)
    end.

set_password_randomly(PasswordKey, Options) ->
    Fun = fun() ->
                  case rpc_call(?CD, user_password, random, [26]) of
                      {error, Any} ->
                          io:format("Could not generate random password.~n"),
                          io:format("Error: ~p~n", [Any]),
                          halt(1);
                      Result ->
                          Result
                  end
          end,
    set_option_unless(PasswordKey, Fun, Options).

set_ssh_key_from_file(Options) ->
    case proplists:get_value(ssh_pub_key_file, Options) of
        undefined ->
            Options;
        SshPubKeyFile ->
            Fun = fun() -> safe_read_file("SSH public key", SshPubKeyFile) end,
            set_option_unless(ssh_pub_key, Fun, Options)
    end.

get_option(Key, Opts) ->
    case lists:keyfind(Key, 1, Opts) of
        false ->
            option_not_found;
        {_Key, Value} ->
            Value
    end.

usage_error(Proper) ->
    io:format("This command was not used with the correct arguments.~n"),
    io:format("Usage: automate-ctl ~s~n", [Proper]),
    {error, usage}.

-spec parse_input2(list(string()), proplists:proplist()) -> ok | error | {error, any()}.
parse_input2(["doctor"], _Opts) ->
    io:format("Validating configuration files..~n"),
    validate_configs(),
    io:format("Everything seems ok.~n");
parse_input2(["doctor", _|_], _Opts) ->
    usage_error("doctor");

parse_input2(["create", "enterprise", Enterprise], Opts) ->
    case get_option(ssh_pub_key, Opts) of
        option_not_found ->
            io:format("--ssh-pub-key-file=<file> is a required option for 'create-enterprise'.~n"),
            {error, missing_option};
        _ ->
            create_enterprise(Enterprise, Opts)
    end;
parse_input2(["create", "enterprise" | _], _Opts) ->
    usage_error("create-enterprise enterprise-name --ssh-pub-key-file=PATH-TO-PUBLIC-KEY-FILE [--builder-password PASSWORD]");


parse_input2(["delete", "enterprise", Enterprise], _Opts) ->
    delete_enterprise(Enterprise);
parse_input2(["delete", "enterprise" | _], _Opts) ->
    usage_error("delete-enterprise ENTERPRISE-NAME");


parse_input2(["rename", "enterprise", Enterprise, NewName], _Opts) ->
    rename_enterprise(Enterprise, NewName);
parse_input2(["rename", "enterprise" | _], _Opts) ->
    usage_error("rename-enterprise ENTERPRISE-NAME NEW-ENTERPRISE-NAME");

parse_input2(["list", "enterprises"], _Opts) ->
    list_enterprises();
parse_input2(["list", "enterprises" | _], _Opts) ->
    usage_error("list-enterprises");

parse_input2(["create", "user", Enterprise, User], Opts) ->
    case create_user(Enterprise, User, Opts) of
        {error, Error} ->
            {error, Error};
        UserData ->
            log_user_creation(Enterprise, UserData),
            ok
    end;
parse_input2(["create", "user" | _], _Opts) ->
    usage_error("create-user ENTERPRISE-NAME USER-NAME [--password PASSWORD] [--password-file FILE] [--roles \"COMMA-SEPARATED-LIST\"] [--ssh-pub-key-file=PATH-TO-PULIC-KEY-FILE]");

parse_input2(["create", "users", UsersTsvFilename], _Opts) ->
    create_users(UsersTsvFilename);
parse_input2(["create", "users" | _], _Opts) ->
    usage_error("create-users TSV-FILE-PATH");

parse_input2(["delete", "user", Enterprise, User], _Opts) ->
    delete_user(Enterprise, User);
parse_input2(["delete", "user" | _], _Opts) ->
    usage_error("delete-user ENTERPRISE-NAME USER-NAME");

parse_input2(["list", "users", Enterprise], _Opts) ->
    list_users(Enterprise);
parse_input2(["list", "users" | _], _Opts) ->
    usage_error("list-users ENTERPRISE-NAME");

parse_input2(["generate", "password", "reset", "token", EntName, UserName], _Opts) ->
    reset_password_with_token(EntName, UserName);
parse_input2(["generate", "password", "reset", "token" | _], _Opts) ->
    usage_error("generate-password-reset-token ENTERPRISE-NAME USER-NAME");

parse_input2(["reset", "password", Enterprise, User, Password], _Opts) ->
    reset_password(Enterprise, User, Password);
parse_input2(["reset", "password" | _], _Opts) ->
    usage_error("reset-password ENTERPRISE-NAME USER-NAME PASSWORD");


parse_input2(["revoke-token", EntName, Username, Token], _Opts) ->
    revoke_token(EntName, Username, Token);
parse_input2(["revoke-token" | _], _Opts) ->
    usage_error("revoke-token ENTERPRISE-NAME USER-NAME TOKEN");

parse_input2(["update-project-hooks"], _Opts) ->
    update_project_hooks();
parse_input2(["update-project-hooks" | _], _Opts) ->
    usage_error("update-project-hoooks");


parse_input2(["delete", "project", Enterprise, Organization, Project], _Opts) ->
    delete_project(Enterprise, Organization, Project);
parse_input2(["delete", "project" | _], _Opts) ->
    usage_error("delete-project ENTERPRISE-NAME ORGANIZATION PROJECT");

%% This is a short lived migration call for updating the deliv_change db records
%% to retroactively have a title and description generated from their commit
%% history.
%%
%% WARNING: Only use this if you know what you are doing. Consider a dry
%% run first.
parse_input2(["migrate-change-description"], _Opts) ->
    migrate_change_description();
parse_input2(["migrate-change-description" | _], _Opts) ->
    usage_error("migrate-change-description");

%% Same as above, but performs a dry run instead. There are no write actions,
%% but there are read actions, which could in theory have side effects, though
%% they should be negligible.
%%
%% WARNING: You should always make a backup first!
parse_input2(["migrate-change-description-dry-run"], _Opts) ->
    migrate_change_description_dry_run();
parse_input2(["migrate-change-description-dry-run" | _], _Opts) ->
    usage_error("migrate-change-description-dry-run");

parse_input2(["migrate-patchset-diffs"], _Opts) ->
    migrate_change_patchset_diffs();
parse_input2(["migrate-patchset-diffs" | _], _Opts) ->
    usage_error("migrate-patchset-diffs");


parse_input2(["migrate-patchset-diffs-dry-run"], _Opts) ->
    migrate_change_patchset_diffs_dry_run();
parse_input2(["migrate-patchset-diffs-dry-run" | _], _Opts) ->
    usage_error("migrate-patchset-diffs-dry-run");


%% This probably isn't the ideal way to let the user know that they typed
%% something wrong, but it's better than getting no output at all, which is
%% super confusing.
%%
%% Also, to avoid further confusion, I specifically avoided printing out the
%% command they entered, since its parsed version looks different from what is
%% entered on the command line, e.g. "delete-project" vs "delete project"
parse_input2(Command, _) ->
    io:format("~s is not a valid command.", [Command]),
    {error, invalid}.

create_enterprise(Enterprise, Opts) ->
    case re:run(Enterprise, "\s") of
        nomatch ->
            create_only_enterprise(Enterprise, Opts,
                                   rpc_call(?CD, deliv_enterprise, list_all, []));
        _ ->
            io:format("ENTERPRISE-NAME name may not contain whitespace.~n"),
            {error, usage}
    end.

create_only_enterprise(Enterprise, _, [_|_] = Ents) ->
    io:format("Error creating enterprise ~p: an enterprise already exists (~s)~n",
              [Enterprise,
               string:join([erlang:binary_to_list(EntName) || {deliv_enterprise, _, EntName} <- Ents],
                           ",")]),
    {error, already_has_enterprise};
create_only_enterprise(Enterprise, _, {error, Why} = Error) ->
    io:format("Error creating enterprise ~p: ~p~n", [Enterprise, Why]),
    Error;
create_only_enterprise(Enterprise, Opts, []) ->
    EntName = list_to_binary(Enterprise),
    [{deliv_enterprise, _EntID, EntName}] =
        rpc_call(?CD, deliv_enterprise, insert, [EntName]),

    UserName = <<"admin">>,
    Password = get_option(password, Opts),
    Roles = get_option(roles, Opts),
    SshPubKey = get_option(ssh_pub_key, Opts),
    create_user(EntName, [{username, UserName}, {password, Password}, {roles, Roles}]),
    set_admin_privs(EntName, UserName),

    BuilderPassword = get_option(builder_password, Opts),
    create_user(EntName, [{username, <<"builder">>},
                          {password, BuilderPassword},
                          {ssh_pub_key, SshPubKey},
                          {roles, Roles}]),
    set_admin_privs(EntName, <<"builder">>),

    WebLogin = rpc_call(?CD, deliv_web_utils, make_web_url_for_base,
                        [EntName]),
    io:format("Created enterprise: ~s~n", [EntName]).

delete_enterprise(Enterprise) ->
    EnterpriseB = list_to_binary(Enterprise),
    case rpc_call(?CD, deliv_enterprise, delete, [EnterpriseB]) of
        ok ->
            io:format("~s deleted successfully.~n", [EnterpriseB]);
        {ok, _} ->
            io:format("~s deleted successfully.~n", [EnterpriseB]);
        {error, Error}  ->
            io:format("Error deleting ~s: ~p~n", [EnterpriseB, Error])
    end.

rename_enterprise(OldName, NewName) ->
    OldNameB = list_to_binary(OldName),
    NewNameB = list_to_binary(NewName),
    case rpc_call(?CD, deliv_enterprise, rename, [OldNameB, NewNameB]) of
        {ok, {deliv_enterprise, _ , NewNameB}} ->
            io:format("~s successfully renamed to ~s.~n", [OldNameB, NewNameB]);
        {error, Error}  ->
            io:format("Error renaming ~s: ~p~n", [OldNameB, Error])
    end.

list_enterprises() ->
    case rpc_call(?CD, deliv_enterprise, list_all, []) of
        {error, Error}  ->
            io:format("Error listing enterprises: ~p~n", [Error]),
            {error, Error};
        Enterprises ->
            [io:format("~s~n", [Name]) || {_ , _, Name} <- Enterprises]
    end.

%% Internal use. PropList is expected to include all required
%% values: [
%%  {username, ListOrBinary},
%%  {password, ListOrBinary},
%%  {roles, List}, % comma delimited string or "".
%% ]
%% Optional:
%%  {ssh_pub_key, binary()}
%%  public key as read from file
%%  This is pre-parsed from the common option handling
%%  which converts --ssh-pub-key-file to {ssh_pub_key, <<data>>}
-spec create_user(binary(), [tuple()]) -> {error, any()} | ok.
create_user(EntName, PropList) ->
    {username, UserName} = lists:keyfind(username, 1, PropList),
    {password, Password} = lists:keyfind(password, 1, PropList),
    {roles, R} = lists:keyfind(roles, 1, PropList),
    Roles = [list_to_binary(E) || E <- string:tokens(R, ",")],
    UserType = get_user_type(PropList),
    UserData = user_data(UserType, UserName, Password),
    UserData2 = maybe_merge_key_val(ssh_pub_key, UserData, PropList),
    case rpc_call(?CD, user_create_mod(UserType), insert, [EntName, UserData2]) of
        {error, Err} ->
            io:format("Error creating user ~p, error: ~p~n", [UserName, Err]),
            {error, Err};
        _ ->
            set_user_privs(EntName, UserName, Roles)
    end.

%% Invoked when create-user ctl command is used. This wrapper to
%% create_user/2 ensures that it is invoked with the expected options
%% and data types.
-spec create_user(string(), string(), [tuple()]) -> {error, atom()} | [tuple()].
create_user(EnterpriseName, User, Opts) ->
    Arguments1 = Opts ++ [{username, User}],
    Arguments2 = case get_option(roles, Opts) of
                     option_not_found ->
                         Arguments1 ++ [{roles, ""}];
                     _ ->
                         Arguments1
                 end,
    case create_user(list_to_binary(EnterpriseName), Arguments2) of
        ok -> Arguments2;
        {error, _} = Error -> Error
    end.

create_users(UsersTsvFilename) ->
    Input = safe_read_file("users TSV file", UsersTsvFilename),
    case rpc_call(?CD, deliv_intern_user, import_from_tsv_string, [Input]) of
        {error, Error} ->
            io:format("Error creating users from TSV file: ~p~n", [Error]),
            {error, Error};
        Other ->
            Other
    end.

delete_user(Enterprise, User) ->
    EnterpriseB = list_to_binary(Enterprise),
    UserB = list_to_binary(User),
    case rpc_call(?CD, deliv_user, delete, [EnterpriseB, UserB]) of
        ok ->
            io:format("~s deleted successfully.~n", [UserB]);
        {error, Error}  ->
            io:format("Error deleting ~s: ~p~n", [UserB, Error]),
            {error, Error}
    end.

list_users(Enterprise) ->
    case rpc_call(?CD, deliv_user, fetch_user_names, [Enterprise]) of
        {error, Error}  ->
            io:format("Error listing ~p users: ~p~n", [Enterprise, Error]),
            {error, Error};
        UserNames ->
            [io:format("~s~n", [UserName]) || UserName <- UserNames]
    end.

reset_password_with_token(EntName, UserName) ->
    case rpc_call(?CD, deliv_intern_user, disable_login_and_generate_password_reset_token, [EntName, UserName]) of
        {error, Reason} ->
            io:format("Error while requesting password reset token for  ~p: ~p~n",
                      [UserName, Reason]),
            {error, Reason};
        {ok, Token} ->
            URL = rpc_call(?CD, deliv_reset_password, reset_url, [EntName, UserName, Token]),
            io:format("Password reset with token successful. Go to this URL to set a new password:~n"),
            io:format("URL: ~s~n", [URL])
    end.

reset_password(Enterprise, UserName, Password) ->
    case rpc_call(?CD, deliv_intern_user, reset_password, [Enterprise, UserName, Password]) of
        {error, Type} ->
            io:format("Error while changing ~p's password: ~p~n", [UserName, Type]),
            {error, Type};
        {error, Type, Reason, Message} ->
            io:format("Error while changing ~p's password: ~p~n", [UserName, Type]),
            io:format("Reason: ~p~n", [Reason]),
            io:format("Message: ~p~n", [Message]),
            {error, Type};
        {ok, _NewUser} ->
            io:format("Password changed successfully.")
    end.

revoke_token(EntName, UserName, Token) ->
    case rpc_call(?CD, deliv_user, revoke_token, [EntName, UserName, Token]) of
        {error, Error}  ->
            io:format("Error revoking ~p's token: ~p~n", [UserName, Error]),
            {error, Error};
        ok ->
            io:format("Token revoked successfully.")
    end.

update_project_hooks() ->
   case rpc_call(?CD, deliv_enterprise, list_all, []) of
       {error, Error}  ->
           io:format("Error listing enterprises: ~p~n", [Error]);
       Enterprises ->
           io:format("Updating git hooks for all projects...~n"),
           lists:foreach(fun(Ent) ->
                             update_project_hooks_for_ent(Ent)
                         end, Enterprises)
   end.

delete_project(Ent, Org, Project) ->
    case rpc_call(?CD, deliv_project, delete, [Ent, Org, Project, true]) of
        {error, {foreign_key, <<"update or delete on table \"pipelines\" "
                                 "violates foreign key constraint "
                                 "\"dependency_failures_pipeline_id_fkey\" "
                                 "on table \"dependency_failures\"">>}} ->
            io:format("The project '~s/~s/~s' has failures in the pipeline. /
                       Please fix the errors and try again.",
                       [Ent, Org, Project] );
        {error, Error} ->
            io:format("Error deleting: '~s/~s/~s': ~p~n",
                      [Ent, Org, Project, Error]);
        {ok, 0} ->
            io:format("Project not found: '~s/~s/~s'~n", [Ent, Org, Project]);
        {ok, 1} ->
            io:format("Successfully deleted project: '~s/~s/~s'~n",
                      [Ent, Org, Project])
    end.

%% @doc Connect to the node actually running Delivery.
init_network() ->
    Name = lists:concat([?SELF_PREFIX, os:getpid(), "@", ?SELF_HOST]),
    net_kernel_start(net_kernel:start([erlang:list_to_atom(Name),
                                       longnames])).

finish_network() ->
    true = erlang:set_cookie(node(), get_cookie()),
    case net_adm:ping(?CD) of
        pong ->
            ok;
        pang ->
            perror(io_lib:format("Unable to connect to Delivery node: ~p~n"
                                 "Try automate-ctl reconfigure~n", [?CD]))
    end.

net_kernel_start({ok, _}) ->
    finish_network();
%% Network might be started if this isn't being run as an escript.
net_kernel_start({error,{{already_started,_}, _}}) ->
    finish_network();
net_kernel_start({error, Reason}) ->
    io_lib:format("Unable to start network: ~p~n",[Reason]),
    {error, Reason}.


%% Do simple validation to check for syntax errors in essential files
validate_configs() ->
    ConfigFiles =
        filelib:wildcard("/opt/delivery/embedded/service/delivery/lib/delivery-*/priv/authz_rules") ++
        ["/var/opt/delivery/delivery/etc/erlang.cfg",
         "/var/opt/delivery/delivery/etc/sys.config"],
    lists:map(fun(X) ->
                      case file:consult(X) of
                          {ok, _} ->
                              ok;
                          {error, _Error} ->
                              io:format("Syntax error in " ++ X)
                      end
              end, ConfigFiles),

    %% We use wildcard to get the correct directory, since delivery is versioned
    JsonFiles =
        filelib:wildcard("/opt/delivery/embedded/service/delivery/lib/delivery-*/priv/schemas/*.json"),
    lists:map(fun(X) ->
                      {ok, JsonBin} = rpc_call(?CD, file, read_file, [X]),
                      case rpc_call(?CD, jiffy, decode, [JsonBin]) of
                          {error, _Error} ->
                              io:format("Syntax error in " ++ X);
                          _ ->
                              ok
                      end
              end, JsonFiles).

maybe_merge_key_val(Key, MergeList, PropList) ->
    case proplists:get_value(Key, PropList) of
        undefined ->
            MergeList;
        SshKey ->
            lists:append(MergeList, [{Key, SshKey}])
    end.


%% These are private
update_project_hooks_for_ent({_, _, EntName}) ->
  case rpc_call(?CD, deliv_organization, fetch_for_ent, [EntName]) of
      {error, Error}  ->
          io:format("Error getting organizations: ~p~n", [Error]);
      Organizations ->
          lists:foreach(fun(Org) ->
                            update_project_hooks_for_org(EntName, Org)
                        end, Organizations)
  end.

update_project_hooks_for_org(EntName, Org) ->
  case rpc_call(?CD, ej, get, [{<<"name">>}, Org]) of
      {error, Error}  ->
          io:format("Error getting organization name: ~p~n", [Error]);
      OrgName ->
        case rpc_call(?CD, deliv_project, fetch_names, [EntName, OrgName]) of
            {error, Error}  ->
                io:format("Error listing projects: ~p~n", [Error]);
            Projects ->
                lists:foreach(fun(Project) ->
                                  update_project_hooks_for_proj(EntName,
                                                                 OrgName,
                                                                 Project)
                              end, Projects)

        end
  end.

update_project_hooks_for_proj(EntName, OrgName, ProjName) ->
  case rpc_call(?CD, deliv_project, ensure_repo_hooks,
                [EntName, OrgName, ProjName]) of
    {error, Error}  ->
        io:format("Error updating hook for project: ~p with error: ~p~n",
                  [ProjName, Error]);
    ok ->
        io:format("Updated hook for project: ~s/~s/~s~n",
                  [EntName, OrgName, ProjName]),
        ok
  end.

migrate_change_description()->
    io:format("STARTING CHANGE DESCRIPTION MIGRATION -- LIVE RUN\n\n"),
    %% pass on full return to be helpful to migrater
    io:format("~p", [rpc_call(?CD, deliv_migration, add_change_description,[])]).

migrate_change_description_dry_run()->
    io:format("STARTING CHANGE DESCRIPTION MIGRATION -- DRY RUN\n\n"),
    %% pass on full return to be helpful to migrater
    io:format("~p", [rpc_call(?CD, deliv_migration,
                              add_change_description_dry_run,[])]).

migrate_change_patchset_diffs() ->
    io:format("STARTING PATCHSET DIFF MIGRATION -- LIVE RUN\n\n"),
    %% pass on full return to be helpful to migrater
    io:format("~p", [rpc_call(?CD, deliv_migration, update_patchset_diffs,[])]).

migrate_change_patchset_diffs_dry_run() ->
    io:format("STARTING PATCHSET DIFF MIGRATION -- DRY RUN\n\n"),
    %% pass on full return to be helpful to migrater
    io:format("~p", [rpc_call(?CD, deliv_migration,
                              update_patchset_diffs_dry_run,[])]).

-spec user_create_mod(binary()) -> atom().
user_create_mod(<<"external">>) ->
    deliv_user;
user_create_mod(<<"internal">>) ->
    deliv_intern_user;
user_create_mod(_) ->
    deliv_intern_user.

%% @doc Default is <<"internal">>
-spec get_user_type(list()) -> binary().
get_user_type(PropList) ->
   case lists:keyfind(user_type, 1, PropList) of
        false -> <<"internal">>;
        {user_type, Val} -> Val
   end.

-spec user_data(binary(), string(), string()) -> list().
user_data(<<"internal">>, UserName, Password) ->
    HashedPass = rpc_call(?CD, user_password, hash, [Password]),
    [{name, UserName},
     {hashed_pass, HashedPass},
     {hash_type, <<"bcrypt">>},
     {user_type, <<"internal">>}];
user_data(<<"external">>, UserName, _) ->
    [{name, UserName},
     {user_type, <<"external">>}].


%% @doc Ensure the given user is an enterprise-level admin.
set_admin_privs(EnterpriseName, UserName) ->
    ok = rpc_call(?CD, deliv_authz, assign_roles, [EnterpriseName, UserName, [<<"admin">>]]).

set_user_privs(EnterpriseName, UserName, Roles) ->
    ok = rpc_call(?CD, deliv_authz, assign_roles, [EnterpriseName, UserName, Roles]).

%% Wrap rpc calls to check for errors
rpc_call(Node, Module, Function, Args) ->
    case rpc:call(Node, Module, Function, Args, ?TIMEOUT) of
        {badrpc, Reason} ->
            perror(io_lib:format("RPC call ~p:~p:~p:~p failed with error ~p~n",
                                 [Node, Module, Function, Args, Reason]));
        Res ->
            Res
    end.

perror(Msg) ->
    io:format(Msg),
    erlang:halt(1).

log_user_creation(EntName, Args) ->
    WebLogin = rpc_call(?CD, deliv_web_utils, make_web_url_for_base,
                        [EntName]),
    log_user_creation(get_user_type(Args), EntName,
                      get_option(username, Args),
                      get_option(password, Args),
                      WebLogin).

log_user_creation(<<"internal">> = UserType, EntName, User, Password, WebLogin) ->
    io:format("Created user~n"
              "enterprise: ~s~n"
              "username: ~s~n"
              "password: ~s~n"
              "Web login: ~s~n"
              "User type: ~s~n",
              [EntName, User, Password, WebLogin, UserType]);

log_user_creation(<<"external">> = UserType, EntName, User, _Password, WebLogin) ->
    io:format("Created user~n"
              "enterprise: ~s~n"
              "username: ~s~n"
              "Web login: ~s~n"
              "User type: ~s~n",
              [EntName, User, WebLogin, UserType]).

get_cookie() ->
    case os:getenv("ERL_COOKIE") of
        false ->
            'delivery';
        Cookie ->
            list_to_atom(Cookie)
    end.
