-module(jobs_runners_state_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_runners_state_test_() ->
    [
     hoax:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "handle_info_"),
     hoax:fixture(?MODULE, "handle_call_")
    ].

init_should_set_initial_state() ->
    hoax:expect(receive
                    jobs_runner:subscribe_runner_events() -> ignored
                end),

    InitialState = jobs_runners_state:init([]),

    ?assertEqual({ok, maps:new()}, InitialState),
    ?verifyAll.

handle_info_runner_state_changed_should_add_runner_to_state() ->
    State = maps:new(),
    Runner = #runner{
                enterprise_name = entname,
                hostname = hostname
               },
    ExpectedState = maps:put({entname, hostname}, Runner, State),

    hoax:expect(receive
                    deliv_event:publish(runners_state_updated, [Runner]) -> true
                end),

    Reply = jobs_runners_state:handle_info({pid, runner_state_changed, Runner}, State),

    ?assertEqual({noreply, ExpectedState}, Reply),
    ?verifyAll.

handle_info_runner_state_changed_should_update_existing_runner_to_state() ->
    State = maps:new(),
    Runner = #runner{
                enterprise_name = entname,
                hostname = hostname,
                health_status = pending
               },
    InitialState = maps:put({entname, hostname}, Runner, State),
    NewRunner = Runner#runner{
                  health_status = ok
                 },
    ExpectedState = maps:put({entname, hostname}, NewRunner, State),
    hoax:expect(receive
                    deliv_event:publish(runners_state_updated, [NewRunner]) -> true
                end),

    Reply = jobs_runners_state:handle_info({pid, runner_state_changed, NewRunner}, InitialState),

    ?assertEqual({noreply, ExpectedState}, Reply),
    ?verifyAll.

handle_info_runner_stopped_should_add_runner_to_state() ->
    State = maps:new(),
    Runner = #runner{
                enterprise_name = entname,
                hostname = hostname
               },
    InitialState = maps:put({entname, hostname}, Runner, State),
    ExpectedState = maps:remove({entname, hostname}, InitialState),
    hoax:expect(receive
                    deliv_event:publish(runners_state_updated, []) -> true
                end),

    Reply = jobs_runners_state:handle_info({pid, runner_stopped, Runner}, InitialState),

    ?assertEqual({noreply, ExpectedState}, Reply),
    ?verifyAll.

handle_call_list_returns_an_empty_list_when_no_runners() ->
    State = maps:new(),

    Reply = jobs_runners_state:handle_call(list, from, State),

    ?assertEqual({reply, [], State}, Reply),
    ?verifyAll.

handle_call_list_returns_a_list_of_runners() ->
    Runner1 = #runner{
                 enterprise_name = entname1,
                 hostname = hostname1
                },
    Runner2 = #runner{
                 enterprise_name = entname2,
                 hostname = hostname2
                },
    State = maps:put({entname1, hostname1}, Runner1, maps:new()),
    InitialState = maps:put({entname2, hostname2}, Runner2, State),

    Reply = jobs_runners_state:handle_call(list, from, InitialState),

    ?assertEqual({reply, [Runner2, Runner1], InitialState}, Reply),
    ?verifyAll.

handle_call_fetch_returns_an_empty_list_if_runner_does_not_exist() ->
    Runner1 = #runner{
                 enterprise_name = entname1,
                 hostname = hostname1
                },
    Runner2 = #runner{
                 enterprise_name = entname2,
                 hostname = hostname2
                },
    State = maps:put({entname1, hostname1}, Runner1, maps:new()),
    InitialState = maps:put({entname2, hostname2}, Runner2, State),

    Reply = jobs_runners_state:handle_call({fetch, not_found, not_found}, from, InitialState),

    ?assertEqual({reply, [], InitialState}, Reply),
    ?verifyAll.

handle_call_fetch_returns_a_list_of_a_single_runner() ->
    Runner1 = #runner{
                 enterprise_name = entname1,
                 hostname = hostname1
                },
    Runner2 = #runner{
                 enterprise_name = entname2,
                 hostname = hostname2
                },
    State = maps:put({entname1, hostname1}, Runner1, maps:new()),
    InitialState = maps:put({entname2, hostname2}, Runner2, State),

    Reply = jobs_runners_state:handle_call({fetch, entname1, hostname1}, from, InitialState),

    ?assertEqual({reply, [Runner1], InitialState}, Reply),
    ?verifyAll.

