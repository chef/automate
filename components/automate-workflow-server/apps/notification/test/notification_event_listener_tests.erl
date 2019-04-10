-module(notification_event_listener_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

fixture_test_() ->
    hoax:fixture(?MODULE, setup, teardown).

setup() ->
    error_logger:tty(false),
    application:start(gproc).

teardown(_) ->
    application:stop(gproc),
    error_logger:tty(true).

start_link_starts_gen_server() ->
    {ok, Pid} = notification_event_listener:start_link(notifier_module),
    unlink(Pid),
    ?assertEqual(true, erlang:is_process_alive(Pid)),
    erlang:exit(Pid, shutdown).

init_subscribes_and_returns_ok_module_name() ->
    hoax:mock(notification_notifier,
              ?expect(event_subscriptions,
                      ?withArgs([]),
                      ?andReturn([subscriptions]))),

    hoax:mock(deliv_event,
              ?expect(subscribe,
              ?withArgs([[subscriptions]]))),

    Result = notification_event_listener:init({notifier_module}),
    ?assertEqual({ok, notifier_module}, Result),
    ?verifyAll.

handle_info_passes_event_type_and_body_to_notifier() ->
    hoax:mock(notification_notifier,
              ?expect(notify,
                      ?withArgs([notifier_module, type, body]))),
    Result = notification_event_listener:handle_info({pid, type, body}, notifier_module),
    ?assertEqual({noreply, notifier_module}, Result),
    ?verifyAll.
