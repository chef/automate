%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-

-module(deliv_stage_event_listeners_sup_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_ () ->
    hoax:fixture(?MODULE, setup, teardown).

%% The listener worker processes require GProc to subscribe to their
%% events of interest.
setup() ->
    error_logger:tty(false),
    Names = [verify, build, acceptance, union, rehearsal, delivered],
    application:set_env(delivery, stages_data, [{Stage, ignored} ||
                                                   Stage <- Names]),
    application:start(gproc).

teardown(_) ->
    application:stop(gproc),
    error_logger:tty(true).

start_link_should_start_children_listeners() ->
    {ok, Pid} = deliv_stage_event_listeners_sup:start_link(),
    unlink(Pid),

    ?assert(is_process_alive(whereis(deliv_changeset_lifecycle_listener))),
    ?assert(is_process_alive(whereis(deliv_dashboard_stage_lifecycle_listener))),

    erlang:exit(Pid, shutdown).

init_should_create_one_for_one_supervisor() ->
    {ok, {SupFlags, _ChildSpecs}} = deliv_stage_event_listeners_sup:init([]),
    ?assertMatch({one_for_one, _, _}, SupFlags).
