%% @doc SSH server for delivery git
-module(deliv_ssh_git).

-include("deliv_types.hrl").

%% TODO Remains to be done
%% - basic test suite
%% - actually make authZ
%% - global timeout on any SSH connection?

-behaviour(ssh_channel).

%% actual API
-export([
         start/0
        ]).

%% Behavior callbacks
-export([
         code_change/3,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_msg/2,
         handle_ssh_msg/2,
         terminate/2
        ]).

%% see http://www.erlang.org/doc/man/ssh_connection.html
%% ssh_data_type_code() = 1 ("stderr") | 0 ("normal")
-define(STDOUT, 0).
-define(STDERR, 1).

%% Used in ssh_daemon_opts/0 to drop known-bad algorithms
%% with help from https://github.com/arthepsy/ssh-audit
-define(BAD_KEX, ['ecdh-sha2-nistp256', % using weak elliptic curves
                  'ecdh-sha2-nistp384', % using weak elliptic curves
                  'ecdh-sha2-nistp521', % using weak elliptic curves
                  'diffie-hellman-group-exchange-sha1', % removed since OpenSSH 6.7, unsafe algorithm
                  'diffie-hellman-group1-sha1' % removed since OpenSSH 6.7, unsafe algorithm
                 ]).
-define(BAD_ENC, ['aes128-cbc', % removed since OpenSSH 6.7, unsafe algorithm
                  '3des-cbc' % removed since OpenSSH 6.7, unsafe algorithm
                 ]).
-define(BAD_KEY, ['ssh-dss' % removed since OpenSSH 6.7, weak algorithm
                 ]).

-record(state, {connection_ref,
                channel_id,
                port,
                env,
                client_eof}).

-spec start() -> {ok, pid()} | {error, _Why}.
start() ->
    ok = delivery_app:start_app_with_deps(ssh, permanent),
    Opts = ssh_daemon_opts(),
    {ok, HostAddress} = inet:parse_address(delivery_app:get_env(deliv_ssh_git_host_address, "0.0.0.0")),
    Port = delivery_app:get_env(deliv_ssh_git_port),
    ssh:daemon(HostAddress, Port, Opts).

%% @private
%% @doc Returns the right options for the SSH daemon
ssh_daemon_opts() ->
    [{system_dir, delivery_app:get_env(deliv_ssh_git_server_keys_path)},
     {ssh_cli, {?MODULE, []}},
     %% TODO: that seems broken : even with that, it still prompts for
     %% a password when it fails to authenticate with public key
     {auth_methods, "publickey"},
     %% max time in milliseconds for the authentication negotiation
     {negotiation_timeout, 5000},
     {key_cb, deliv_ssh_key_cb},
     {preferred_algorithms, preferred_algs()}
    ].

-spec preferred_algs() -> list({atom(), any()}).
preferred_algs() ->
    lists:map(fun remove_bad_algs/1, ssh:default_algorithms()).

%% We only remove algorithms that we know are insecure, instead of specifying a
%% list of algorithms we want to use -- so when the next OTP release includes
%% new algorithms, we will use them.
-spec remove_bad_algs({atom(), any()}) -> {atom(), any()}.
remove_bad_algs({public_key, Algs}) ->
    {public_key, lists:subtract(Algs, ?BAD_KEY)};
remove_bad_algs({cipher, Directions}) ->
    {cipher, lists:map(fun remove_bad_algs/1, Directions)};
remove_bad_algs({server2client, Algs}) ->
    {server2client, lists:subtract(Algs, ?BAD_ENC)};
remove_bad_algs({kex, Algs}) ->
    {kex, lists:subtract(Algs, ?BAD_KEX)};
remove_bad_algs(Any) -> Any.

init([]) ->
    {ok, #state{port = none,
                client_eof = false,
                env = []}}.

terminate(Reason, State) ->
    chef_log:debug("terminate R:~p, S:~p\n", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(Msg, From, State) ->
    chef_log:error("Unexpected handle_call in ~p: ~p", [?MODULE, {Msg, From, State}]),
    {reply, error, State}.

handle_cast(Msg, State) ->
    chef_log:error("Unexpected handle_cast in ~p: ~p", [?MODULE, {Msg, State}]),
    {noreply, State}.

handle_ssh_msg({ssh_cm, ConnectionRef, {env, ChannelId, WantReply, VarName, Value}},
               #state{port = none, connection_ref = ConnectionRef, channel_id = ChannelId, env = Env} = State) ->
    ssh_connection:reply_request(ConnectionRef, WantReply, success, ChannelId),
    %% we save that env var to pass it to git-shell later
    NewState = State#state{env = [{chef_utils:to_str(VarName), chef_utils:to_str(Value)} | Env]},
    {ok, NewState};
handle_ssh_msg({ssh_cm, ConnectionRef, {exec, ChannelId, WantReply, Cmd}},
               #state{port = none, connection_ref = ConnectionRef, channel_id = ChannelId, env = Env0} = State) ->
    ssh_connection:reply_request(ConnectionRef, WantReply, success, ChannelId),
    NewState =
        case authorized_command(Cmd, State) of
            {forbid, Why} ->
                chef_log:info("Refused to execute ~p: ~p", [Cmd, Why]),
                Feedback = "DELIVERY: ERROR: " ++ chef_utils:to_str(Why) ++ "\n",
                close_with_feedback(State, Feedback),
                State;
            {allow, Action, UserName, EntName, OrgName, Proj} ->
                ProjName = deliv_project:getval(name, Proj),
                chef_log:debug("Action ~p on repo ~p/~p/~p by ~p",
                                [Action, EntName, OrgName, ProjName, UserName]),

                StrPort = chef_utils:to_str(delivery_app:get_env(listen_port)),
                %% these env vars are expeceted by our update git hook
                %% of course, their names must remain consistent with what's
                %% in that script
                Env = [{"DELIV_USER", chef_utils:to_str(UserName)},
                       {"DELIV_ENTERPRISE", chef_utils:to_str(EntName)},
                       {"DELIV_ORGANIZATION", chef_utils:to_str(OrgName)},
                       {"DELIV_PROJECT", chef_utils:to_str(ProjName)},
                       %% we pass the port number as this changes for tests
                       {"DELIV_HTTP_PORT", StrPort}
                       | Env0],

                RealCmd = chef_utils:to_str(build_real_command(Action, Proj)),
                chef_log:info("Executing ~p with env ~p", [RealCmd, Env]),
                PortOptions = [exit_status, binary, {env, Env}],
                Port = erlang:open_port({spawn, RealCmd}, PortOptions),
                State#state{port = Port}
        end,
    {ok, NewState};
handle_ssh_msg({ssh_cm, ConnectionRef, {data, ChannelId, ?STDOUT, Data}},
               #state{port = Port, connection_ref = ConnectionRef, channel_id = ChannelId} = State) ->
    chef_log:debug("Forwarding ~B bytes from client to port\n", [size(Data)]),
    erlang:port_command(Port, Data),
    {ok, State};
handle_ssh_msg({ssh_cm, ConnectionRef, {eof, ChannelId}},
               #state{client_eof = false, connection_ref = ConnectionRef, channel_id = ChannelId} = State) ->
    chef_log:debug("Received eof from client"),
    NewState = State#state{client_eof = true},
    maybe_close(NewState);
%% we let the user know if they login successfully (that's helpful to debug
%% SSH connection problems, as a user)
handle_ssh_msg({ssh_cm, ConnectionRef, {pty, ChannelId, WantReply, _PtyData}},
               #state{connection_ref = ConnectionRef, channel_id = ChannelId} = State) ->
    refuse_shell_connection(State, WantReply);
handle_ssh_msg({ssh_cm, ConnectionRef, {shell, ChannelId, WantReply}},
               #state{connection_ref = ConnectionRef, channel_id = ChannelId} = State) ->
    refuse_shell_connection(State, WantReply);
handle_ssh_msg(Msg, State) ->
    chef_log:error("Unexpected handle_ssh_msg in ~p: ~p", [?MODULE, {Msg, State}]),
    {ok, State}.

%% @private
%% @doc Checks if that `Cmd' is allowed, i.e.
%%  - it is a white-listed git command
%%  - the user has the right authorizations to perform that action
-spec authorized_command(string(), #state{})->
                                {allow, Action, UserName, EntName, OrgName, Proj}
                                | {forbid, str_or_binary()} when
      Action :: read | write,
      UserName :: binary(),
      EntName :: binary(),
      OrgName :: binary(),
      Proj :: d_project().
authorized_command(Cmd, State) ->
    parse_cmd_then_authz(parse_cmd(Cmd), State).

parse_cmd_then_authz({forbid, _Why} = Forbidden, _State) ->
    %% this is not a recognized command, independently of who's trying
    %% to run it
    Forbidden;
parse_cmd_then_authz({allow, Action, RepoName}, State) ->
    %% then we need to check the user has the right to do this
    SshUserName = get_ssh_username(State),
    %% the following line is 'guaranteed' to work as we already passed authN
    %% (except if that user happens to be modified at that exact precise
    %% moment somewhere else)
    {UserName, EntName} = deliv_ssh_key_cb:extract_user_ent(SshUserName),
    case check_action_authorized(Action, UserName, EntName, RepoName) of
        {allow, OrgName, Proj} ->
            {allow, Action, UserName, EntName, OrgName, Proj};
        forbid ->
            {forbid, "Unauthorized action"}
    end.

check_action_authorized(Action, UserName, EntName, RepoName) ->
    case deliv_project:decompose_repo_name(RepoName) of
        %% note that this pattern matching also makes
        %% sure that this project belongs to the user's enterprise
        {EntName, OrgName, ProjName} ->
            check_proj_valid_then_authz(
              deliv_project:fetch(EntName, OrgName, ProjName),
              Action, UserName, EntName, OrgName
             );
        Other ->
            chef_log:debug("decompose_repo_name failed ~p : ~p",
                            [RepoName, Other]),
            forbid
    end.

check_proj_valid_then_authz({ok, Proj}, Action, UserName, EntName, OrgName) ->
    ProjName = deliv_project:getval(name, Proj),
    case deliv_git:authorized_git_action(EntName, OrgName, ProjName, UserName, Action) of
        allow -> {allow, OrgName, Proj};
        forbid -> forbid
    end;
check_proj_valid_then_authz({error, not_found}, _, _, _, _) ->
    forbid.

%% @private
%% @doc Checks that the command is a white-listed git command
-spec parse_cmd(string()) ->
                       {allow, read | write, RepoName :: string()} | {forbid, string()}.
parse_cmd(Cmd) when erlang:is_list(Cmd) ->
    case re:run(Cmd, valid_cmd_regex(), [{capture, all, list}]) of
        {match, [Cmd, "receive", RepoName]} -> {allow, write, RepoName};
        {match, [Cmd, "upload", RepoName]}-> {allow, read, RepoName};
        _ -> {forbid, "Forbidden command"}
    end.

%% @private
%% @doc Returns a compiled regex to match valid commands
valid_cmd_regex() ->
    {ok, Regex} = re:compile("^git-(receive|upload)-pack ['\"]/?(.+)['\"]$"),
    Regex.

%% @private
%% @doc Buils the real command that gets executed on the system
-spec build_real_command(read | write, d_project()) -> binary().
build_real_command(Action, Proj) ->
    ActionBin = case Action of
                    read -> <<"upload">>;
                    write -> <<"receive">>
                end,
    RepoPath = deliv_project:repo_path(Proj),
    <<"git-shell -c \"git-", ActionBin/binary, "-pack '",
      RepoPath/binary, "'\"">>.

refuse_shell_connection(#state{connection_ref = ConnectionRef,
                               channel_id = ChannelId} = State, WantReply) ->
    ssh_connection:reply_request(ConnectionRef, WantReply, success, ChannelId),
    UserName = get_ssh_username(State),
    Message = <<"Hi ", UserName/binary,"! You've successfully authenticated, "
                "but Chef Delivery does not provide shell access.\n">>,
    close_with_feedback(State, Message).

handle_msg({ssh_channel_up, ChannelId, ConnectionRef}, State) ->
    {ok, State#state{channel_id = ChannelId, connection_ref = ConnectionRef}};
handle_msg({Port, {data, Data}},
           #state{port = Port, connection_ref = ConnectionRef, channel_id = ChannelId} = State) ->
    chef_log:debug("Sending ~B bytes from port to SSH client\n", [size(Data)]),
    ssh_connection:send(ConnectionRef, ChannelId, chef_utils:to_str(Data)),
    {ok, State};
%% Philosophy on handling the command's execution result:
%%  - if we get a 0 exit_status, we must make sure there's no race condition
%%    for some of the data so we actually wait for the `'EXIT'' message below
%%  - if we get an error exit_status, we don't really care about some data
%%    possibly being lost - we just close the connection
handle_msg({Port, {exit_status, ExitStatus}}, #state{port = Port} = State) ->
    case ExitStatus of
        0 ->
            {ok, State};
        ErrorExitStatus when ErrorExitStatus =/= 0 ->
            NewState = State#state{port = closed},
            chef_log:info("Command failed!"),
            close(NewState, ErrorExitStatus)
    end;
handle_msg({'EXIT', _Port, normal}, #state{port = Port} = State) ->
    case Port of
        closed ->
            %% means there was a non-0 exit status, the connection has already
            %% been closed above, and so there's nothing left to do
            {ok, State};
        _P ->
            %% means that was a successful opeartion
            NewState = State#state{port = closed},
            maybe_close(NewState)
    end;
handle_msg(Msg, State) ->
    chef_log:error("Unexpected handle_msg in ~p: ~p", [?MODULE, {Msg, State}]),
    {ok, State}.

%% @private
%% @doc Close sthe SSH side if we've received eof from the client and our
%% port command has completed.
maybe_close(#state{port = closed, client_eof = true} = State) -> close(State, 0);
maybe_close(#state{} = State)                                 -> {ok, State}.

%% @private
%% @doc Closes the connection, with given exit status
close(#state{connection_ref = ConnectionRef, channel_id = ChannelId} = State,
      ExitStatus) ->
    ssh_connection:send_eof(ConnectionRef, ChannelId),
    ssh_connection:exit_status(ConnectionRef, ChannelId, ExitStatus),
    ssh_connection:close(ConnectionRef, ChannelId),
    {ok, State}.

%% @private
%% @doc Sends the given feedback to the client, then closes the connection
%% with exit status 1
-spec close_with_feedback(#state{}, str_or_binary()) -> {ok, #state{}}.
close_with_feedback(#state{channel_id = ChannelId,
        connection_ref = ConnectionRef} = State, Feedback) ->
    ssh_connection:send(ConnectionRef, ChannelId,
                        ?STDERR, chef_utils:to_str(Feedback)),
    close(State, 1).

%% @private
%% @doc Returns the SSH user name used to login
%% If you think, like me, that it makes no sense to map connection refs
%% to user names, since after all you can reuse the same SSH connection
%% for different sessions, possibly with different login names - well then
%% if you think that, you're wrong. Tested it, if you use the same connection
%% with different login names, they somehow get distinct `ConnectionRef's
%% in the OTP app
%% TODO: does the protocol enforce any size limits on the username? If not,
%% wonder if we should
-spec get_ssh_username(#state{}) -> binary().
get_ssh_username(#state{connection_ref = ConnectionRef}) ->
    [{user, UserName}] = ssh:connection_info(ConnectionRef, [user]),
    chef_utils:to_bin(UserName).
