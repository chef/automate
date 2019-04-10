%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et

-module(deliv_dashboard_stage_lifecycle_listener_tests).

-include_lib("hoax/include/hoax.hrl").

-include("deliv_types.hrl").
-include("deliv_events.hrl").
-include("deliv_coordinates.hrl").

-compile(export_all).

%% SUT stands for Subject Under Test
-define(SUT, deliv_dashboard_stage_lifecycle_listener).

init_fixture_test_() ->
    hoax:fixture(?MODULE, "init").

handle_info_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "handle_info", setup_with_stage_names, teardown).

setup() ->
    error_logger:tty(false),
    application:start(gproc),
    setup_stage_names(),
    {ok, Pid} = ?SUT:start_link(),
    unlink(Pid),
    ok.

setup_with_stage_names() ->
    Names = setup_stage_names(),
    setup(),
    {Names, self()}.

setup_stage_names() ->
    Names = [verify, build, acceptance, union, rehearsal, delivered],
    application:set_env(delivery, stages_data, [{Stage, ignored} ||
                                                   Stage <- Names]),
    Names.


teardown(_) ->
    receive
        {'ETS-TRANSFER', hoax, _, ignored} ->
            %% confirmed we still own the hoax process
            ok;
        _ ->
            ?debugFmt("~p lost management of the hoax process", [?SUT])
    after
        500 -> ok
    end,

    case whereis(deliv_dashboard_stage_lifecycle_listener) of
        undefined ->
            ok;
        Pid ->
            erlang:exit(Pid, shutdown)
    end,
    application:stop(gproc),
    error_logger:tty(true),
    ok.

init_should_listen_only_stage_start_finished_events() ->
    hoax:mock(deliv_stage,
              ?expect(subscribe_stage_events,
                      ?withArgs([]))),
    hoax:mock(deliv_change,
              ?expect(subscribe_change_events,
                      ?withArgs([]))),

    ?assertEqual({ok, dict:new()},
                 ?SUT:init([])),

    ?verifyAll.

handle_info_should_act_on_stage_start_events({StageNames, _}) ->
    Payloads = [stage_event_for(StageName, started) || StageName <- StageNames],
    EntNames = [deliv_scopes:'#get'(ent_name, Payload#stage_event.scope) || Payload <- Payloads],
    [
     hoax:mock(deliv_stage_run,
               ?expect(current_pipeline_status_for_enterprise,
                       ?withArgs([deliv_scopes:'#get'(ent_name, Payload#stage_event.scope)]),
                       ?andReturn({ok, pipeline_ejson})))
     || Payload <- Payloads],

    [?assertEqual({ok, pipeline_ejson}, ?SUT:subscribe(EntName)) || EntName <- EntNames],
    [deliv_event:publish({{stage,started}, Payload#stage_event.stage_name}, Payload) || Payload <- Payloads],

    assert_events_emitted(EntNames),
    ?assertEqual({ok, pipeline_ejson}, ?SUT:ejson(hd(EntNames))),

    ?verifyAll.

handle_info_should_store_on_stage_start_events({[StageName | _], Pid}) ->
    assertListeningToStageEvent(Pid, StageName, started).

handle_info_should_store_on_stage_start_finished({[StageName | _], Pid}) ->
    assertListeningToStageEvent(Pid, StageName, finished).

handle_info_should_store_on_stage_running({[StageName | _], Pid}) ->
    assertListeningToStageEvent(Pid, StageName, running).

handle_info_should_store_on_change_updated({_, Pid}) ->
    assertListeningToChangeEvent(Pid, change_updated).

handle_info_should_store_on_change_deleted({_, Pid}) ->
    %% Arrange: Subscribe and mock dabase query
    EntName = chef_utils:random_string(6),
    Deleter = chef_utils:random_string(6),
    ChangeId = chef_utils:random_string(6),
    hoax:mock(deliv_stage_run,
              ?expect(current_pipeline_status_for_enterprise,
                      ?withArgs([EntName]),
                      ?andReturn({ok, pipeline_ejson}))),
    hoax:mock(deliv_stage_run,
              ?expect(current_pipeline_status_for_enterprise,
                      ?withArgs([EntName]),
                      ?andReturn({ok, pipeline_ejson}))),
    ?SUT:subscribe(EntName),
    restart_hoax(Pid),
    hoax:mock(deliv_user,
             ?expect(scoping_names,
                     ?withArgs([Deleter]),
                     ?andReturn({EntName}))),
    hoax:mock(deliv_stage_run,
              ?expect(current_pipeline_status_for_enterprise,
                      ?withArgs([EntName]),
                      ?andReturn({ok, updated_pipeline_ejson}))),

    %% Act: Publish event for consumption, wait for events to be emitted,
    %% unsubscribe (because gproc won't let you subscribe twice),
    %% then subscribe again.
    deliv_event:publish(change_deleted, {{change_id, ChangeId}, {deleted_by, Deleter}}),
    assert_events_emitted([EntName]),
    ?SUT:unsubscribe(EntName),
    SubscriptionResult = ?SUT:subscribe(EntName),

    %% Assert: the subscription returns updated json
    ?assertEqual({ok, updated_pipeline_ejson}, SubscriptionResult).

handle_info_should_store_on_change_created({_, Pid}) ->
    assertListeningToChangeEvent(Pid, change_created).

handle_info_should_store_on_change_approved({_, Pid}) ->
    assertListeningToChangeEvent(Pid, change_approved).

handle_info_should_store_on_change_superseded({_, Pid}) ->
    assertListeningToChangeEvent(Pid, change_superseded).

handle_info_should_store_on_change_delivered({_, Pid}) ->
    assertListeningToChangeEvent(Pid, change_delivered).

%%
%% Assertion Helpers
%%

assertListeningToStageEvent(Pid, StageName, StageAction) ->
    %% Arrange: Subscribe and mock dabase query
    Payload = stage_event_for(StageName, StageAction),
    EntName = deliv_scopes:'#get'(ent_name, Payload#stage_event.scope),
    hoax:mock(deliv_stage_run,
              ?expect(current_pipeline_status_for_enterprise,
                      ?withArgs([deliv_scopes:'#get'(ent_name, Payload#stage_event.scope)]),
                      ?andReturn({ok, pipeline_ejson}))),
    ?SUT:subscribe(EntName),
    restart_hoax(Pid),
    hoax:mock(deliv_stage_run,
              ?expect(current_pipeline_status_for_enterprise,
                      ?withArgs([deliv_scopes:'#get'(ent_name, Payload#stage_event.scope)]),
                      ?andReturn({ok, updated_pipeline_ejson}))),

    %% Act: Publish event for consumption, wait for events to be emitted,
    %% unsubscribe (because gproc won't let you subscribe twice),
    %% then subscribe again.
    deliv_event:publish({{stage,started}, Payload#stage_event.stage_name}, Payload),
    assert_events_emitted([EntName]),
    ?SUT:unsubscribe(EntName),
    SubscriptionResult = ?SUT:subscribe(EntName),

    %% Assert: the subscription returns updated json
    ?assertEqual({ok, updated_pipeline_ejson},SubscriptionResult).

assertListeningToChangeEvent(Pid, ChangeEvent) ->
    %% Arrange: Subscribe and mock dabase query
    Change = deliv_change:'#new'(),
    Scope = generate_scope(),
    #proj_coordinates{ent_name = EntName} = Coords = deliv_scopes:scope_to_coordinates(Scope),
    hoax:mock(deliv_stage_run,
              ?expect(current_pipeline_status_for_enterprise,
                      ?withArgs([EntName]),
                      ?andReturn({ok, pipeline_ejson}))),
    hoax:mock(deliv_stage_run,
              ?expect(current_pipeline_status_for_enterprise,
                      ?withArgs([EntName]),
                      ?andReturn({ok, pipeline_ejson}))),
    ?SUT:subscribe(EntName),
    restart_hoax(Pid),
    hoax:mock(deliv_scopes,
             ?expect(from_change,
                     ?withArgs([Change]),
                     ?andReturn(Scope))),
    hoax:mock(deliv_scopes,
             ?expect(scope_to_coordinates,
                     ?withArgs([Scope]),
                     ?andReturn(Coords))),
    hoax:mock(deliv_stage_run,
              ?expect(current_pipeline_status_for_enterprise,
                      ?withArgs([EntName]),
                      ?andReturn({ok, updated_pipeline_ejson}))),

    %% Act: Publish event for consumption, wait for events to be emitted,
    %% unsubscribe (because gproc won't let you subscribe twice),
    %% then subscribe again.
    deliv_event:publish(ChangeEvent, Change),
    assert_events_emitted([EntName]),
    ?SUT:unsubscribe(EntName),
    SubscriptionResult = ?SUT:subscribe(EntName),
    ?assertEqual({ok, updated_pipeline_ejson}, SubscriptionResult).

assert_events_emitted([]) ->
    ok;
assert_events_emitted([EntName | EntNames]) ->
    receive
        {_, {dashboard_updated, EventEntName}, _} ->
            ?assertEqual(EntName, EventEntName);
        Message ->
            ?debugVal(Message),
            ?assertEqual({events_received, EntName}, {events_not_received, EntName, length(EntNames)})
    after
        500 ->
            ?assertEqual({events_received, EntName}, {events_not_received, EntName, length(EntNames)})
    end,
    assert_events_emitted(EntNames).

%%
%% Helper functions
%%

stage_event_for(StageName, Action) ->
    stage_event_for(StageName, Action, deliv_change:'#new'()).

stage_event_for(StageName, Action, Change) ->
    #stage_event{
       action = Action,
       stage_name = StageName,
       scope = generate_scope(),
       stage_run = ignored_for_now,
       change = Change
      }.

generate_scope() ->
    EntName = crypto:rand_bytes(6),
    deliv_scopes:'#new_common'([
                                {scoping_names, [EntName, <<"org">>, <<"proj">>, <<"pipe">>]},
                                {org_name, <<"org">>},
                                {proj_name, <<"proj">>},
                                {pipe_name, <<"pipe">>},
                                {ent_name, EntName}
                               ]).


%% Hoax doesn't currently support multiple returns. This allows us to
%% reset hoax's state. The Pid is the self() of setup which needs to
%% retake ownership when the test process completes so it can clean up
%% hoax correctly.
restart_hoax(Pid) ->
    hoax:stop(),
    hoax:start(),
    ets:setopts(hoax, [{heir, Pid, ignored}]).
