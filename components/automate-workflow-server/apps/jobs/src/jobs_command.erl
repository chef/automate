-module(jobs_command).

-include("jobs_types.hrl").

-behaviour(gen_server).

% Public API
-export([
         start/2,
         cancel/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% Note: inside of gather_data, this is the timeout that the caller
%% would wait for output from the executed command before giving up on it. So,
%% while this is technically the timeout _between_ two pieces of output, it
%% becomes the overall timeout since delivery-cmd doesn't output anything.
%% TODO: do we want to expose this to the user (omnibus + delivery.rb)?
-define(RUN_TIMEOUT, application:get_env(jobs, ssh_run_timeout, 18000000)).

start(Runner, Command) ->
    gen_server:start_link(?MODULE, [Runner, Command], []).

cancel(Pid) ->
    Pid ! cancel_job.

init([#runner{pid = RunnerPid} = Runner, Command]) ->
    SshCommand = jobs_ssh_command:generate_ssh_command(Runner, Command),
    Port = chef_utils:open_port({spawn, SshCommand}, [exit_status, binary, stderr_to_stdout]),
    gen_server:cast(self(), gather_data),
    {ok, {RunnerPid, Port}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(gather_data, State = {_RunnerPid, Port}) ->
    {stop, {shutdown, gather_data(Port, ?RUN_TIMEOUT, <<>>)}, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVersion, State, _Extras) ->
    {ok, State}.

%% private
-spec gather_data(port(), integer(), binary()) -> {integer(), binary()} | {error, canceled | timeout}.
gather_data(Port, Timeout, Acc) ->
    receive
        {Port, {exit_status, 0}} ->
            {0, Acc};
        {Port, {exit_status, Status}} ->
            {Status, Acc};
        {Port, {data, Part}} ->
            gather_data(Port, Timeout, <<Acc/binary, Part/binary>>);
        cancel_job ->
            PortInfo = chef_utils:port_info(Port),
            chef_utils:run_cmd(["kill", integer_to_list(proplists:get_value(os_pid, PortInfo))]),
            chef_utils:port_close(Port),
            {error, canceled}
    after Timeout ->
            {error, timeout}
    end.
