-module(jobs_command_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_runner_test_() ->
    [
     hoax:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "handle_cast_")
    ].

%% Note: we are not currently testing if init/1 calls
%% gen_server:cast(self(), gather_data).
init_generates_ssh_command_and_opens_port() ->
    Command = <<"ls -al">>,
    Opts = [exit_status, binary, stderr_to_stdout],
    RunnerPid = list_to_pid("<0.24.0>"),
    Runner = #runner{pid = RunnerPid},
    hoax:expect(receive
                    jobs_ssh_command:generate_ssh_command(Runner, Command) -> ssh_command_str;
                    chef_utils:open_port({spawn, ssh_command_str}, Opts) -> port
                end),

    Actual = jobs_command:init([Runner, Command]),
    ?assertEqual({ok, {RunnerPid, port}}, Actual),
    ?verifyAll.

%% See https://github.com/chef/delivery/blob/616a94a4/server/apps/delivery/test/unit/deliv_event_tests.erl#L65-L71

spawn_sender(Parent, Msgs) ->
    LivenessPid = self(),
    Pid = spawn(fun() ->
                        LivenessPid ! {self(), alive},
                        [ Parent ! M || M <- Msgs]
                end),
    receive
        {Pid, alive} -> Pid
    end.

handle_cast_gather_data_queries_the_port_for_data_and_returns_results_to_runner_when_the_command_succeeds() ->
    Pid = list_to_pid("<0.10.0>"),
    Port = port,
    State = {Pid, Port},
    Output = <<"output\n">>,
    spawn_sender(self(),
                 [{Port, {data, Output}},
                  {Port, {exit_status, 0}}]),

    Actual = jobs_command:handle_cast(gather_data, State),
    ?assertEqual({stop, {shutdown, {0, Output}}, State}, Actual),
    ?verifyAll.

handle_cast_gather_data_when_the_command_times_out_returns_timeout_to_runner() ->
    Pid = list_to_pid("<0.10.0>"),
    State = {Pid, port},
    %% Note: To test this, we simply do not set up a process that would send
    %% output. To have it timeout fast, we set the timeout to almost nothing.
    hoax:expect(receive
                    application:get_env(jobs, ssh_run_timeout, 18000000) -> 1
                end),

    Actual = jobs_command:handle_cast(gather_data, State),
    ?assertEqual({stop, {shutdown, {error, timeout}}, State}, Actual),
    ?verifyAll.

handle_cast_gather_data_when_the_command_fails_with_non_zero_exit_code_returns_error_to_runner() ->
    Pid = list_to_pid("<0.10.0>"),
    Port = port,
    State = {Pid, Port},
    Output = <<"command failed">>,
    spawn_sender(self(),
                 [{Port, {data, Output}},
                  {Port, {exit_status, 1}}]),

    Actual = jobs_command:handle_cast(gather_data, State),
    ?assertEqual({stop, {shutdown, {1, Output}}, State}, Actual),
    ?verifyAll.

handle_cast_gather_data_when_the_command_execution_is_canceled_returns_error_canceled() ->
    Pid = list_to_pid("<0.10.0>"),
    State = {Pid, port},
    PortInfo = [{os_pid, 1234}],
    %% Note: To test this, we simply do not set up a process that would send
    %% output. Compared to above, we also don't want it to timeout, so we can
    %% trigger the cancelation.
    spawn_sender(self(), [cancel_job]),
    hoax:expect(receive
                    chef_utils:port_info(port) -> PortInfo;
                    chef_utils:run_cmd(["kill", "1234"]) -> ignored;
                    chef_utils:port_close(port) -> ignored
                end),

    Actual = jobs_command:handle_cast(gather_data, State),
    ?assertEqual({stop, {shutdown, {error, canceled}}, State}, Actual),
    ?verifyAll.
