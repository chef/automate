-module(insights_listener_tests).

-include_lib("hoax/include/hoax.hrl").

-include("insights.hrl").

-compile([export_all]).

-define(TEST_IMPL_MODULE, insights_listener_test_impl).

fixture_test_() ->
    hoax:fixture(?MODULE).

start_link_starts_gen_server() ->
    application:start(gproc),
    {ok, Pid} = insights_listener:start_link(?TEST_IMPL_MODULE),
    unlink(Pid),
    timer:sleep(10),
    ?assertEqual(true, erlang:is_process_alive(Pid)),
    erlang:exit(Pid, shutdown),
    application:stop(gproc).

init_writes_module_to_state() ->
    hoax:mock(?TEST_IMPL_MODULE,
              ?expect(subscribe_to_events,
                      ?withArgs([]),
                      ?andReturn(ok))),

    Result = insights_listener:init({?TEST_IMPL_MODULE}),
    Expected = {ok, ?TEST_IMPL_MODULE},
    ?assertEqual(Expected, Result),
    ?verifyAll.

new_event_arity3_returns_insights_event() ->
    Type = event_type,
    Action = event_action,
    Ejson = {[{key, value}]},
    Hostname = "delivery.chef.co",

    hoax:mock(application,
             ?expect(get_env,
                    ?withArgs([delivery, hostname]),
                    ?andReturn({ok, Hostname}))),

    Actual = insights_listener:new_event(Type, Action, Ejson),
    Expected = #insights_event{
        source = delivery,
        source_fqdn = Hostname,
        type = Type,
        action = Action,
        ejson = Ejson
    },
    ?assertEqual(Expected, Actual).

handle_call_ignores_unsupported_events() ->
    Subj = invalid_subj,
    Body = invalid_body,
    hoax:mock(?TEST_IMPL_MODULE, [
                          ?expect(handle_event,
                                  ?withArgs([Subj, Body]),
                                  ?andReturn({error, unhandled_event}))
                        ]),
    hoax:mock(insights_ingester,
              ?expect(publish,
                      ?withArgs([?any]),
                      ?times(0))),

    Result = insights_listener:handle_info({self, Subj, Body}, ?TEST_IMPL_MODULE),
    Expected = {noreply, ?TEST_IMPL_MODULE},
    ?assertEqual(Expected, Result),
    ?verifyAll.

handle_info_calls_impl_and_publishes_json_results() ->
    Subj = event_subj,
    Body = event_body,
    Record = #insights_event{
        source = delivery,
        type = change,
        action = created,
        ejson = {[{<<"key">>, <<"value">>}]}
    },
    HydratedEjson = {[
        {<<"key">>, <<"value">>},
        {<<"event_type">>, <<"change">>},
        {<<"event_action">>, <<"created">>},
        {<<"source">>, <<"delivery">>}
    ]},
    HydratedJson = chef_json:encode(HydratedEjson),

    hoax:mock(?TEST_IMPL_MODULE, [
                          ?expect(handle_event,
                                  ?withArgs([Subj, Body]),
                                  ?andReturn(Record))
                        ]),
    hoax:mock(chef_json,
                ?expect(encode,
                ?withArgs([HydratedEjson]),
                ?andReturn(HydratedJson))),
    hoax:mock(insights_ingester,
              ?expect(publish,
                      ?withArgs([HydratedJson]))),

    Result = insights_listener:handle_info({self, Subj, Body}, ?TEST_IMPL_MODULE),
    Expected = {noreply, ?TEST_IMPL_MODULE},
    ?assertEqual(Expected, Result),
    ?verifyAll.
