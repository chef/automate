-module(audit_subscriptions_tests).

-compile([export_all]).

-include_lib("hoax/include/hoax.hrl").

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_events.hrl").
-include("audit_events.hrl").

init_fixture_test_() ->
    hoax:fixture(?MODULE, "init").

audit_subscriptions_fixture_test_() ->
    hoax:fixture(?MODULE, "handle_info", setup, teardown).

setup() ->
    error_logger:tty(false),
    application:start(gproc),
    application:set_env(delivery, stages_data, [
                                                {verify, do_not_care},
                                                {build, do_not_care},
                                                {acceptance, do_not_care},
                                                {union, do_not_care},
                                                {rehearsal, do_not_care},
                                                {delivered, do_not_care}
                                               ]),
    application:set_env(audit, max_events_in_memory, 5),
    hoax:mock(audit_stage_event_db,
             ?expect(fetch_audit_events,
                    ?withArgs([5]),
                    ?andReturn([]))),
    {ok, Pid} = audit_subscriptions:start_link(),
    unlink(Pid),
    Pid.

teardown(Pid) ->
    erlang:exit(Pid, shutdown),
    application:stop(gproc),
    error_logger:tty(true),
    ok.

init_should_listen_for_stage_start_events() ->
    MaxEvents = 5,
    application:set_env(audit, max_events_in_memory, MaxEvents),
    hoax:mock(audit_stage_event_db,
             ?expect(fetch_audit_events,
                    ?withArgs([MaxEvents]),
                    ?andReturn([#audit_stage_event{}]))),

    hoax:mock(deliv_stage,
              ?expect(subscribe_stage_events,
                      ?withArgs([]))),

    ?assertEqual({ok, {state, MaxEvents, [#audit_stage_event{}]}}, audit_subscriptions:init([])),

    ?verifyAll.

handle_info_should_log_stage_started_events() ->
    {Change, Scope} = stub_change_and_scope(),
    Payload = create_stage_event(started, verify, Change, Scope),
    ExpectedAuditLog = mock_audit_stage_event(
                         Payload#stage_event.change,
                         Payload#stage_event.action,
                         Payload#stage_event.stage_name,
                         Payload#stage_event.create_time),
    hoax:mock(audit_stage_event_db,
              ?expect(insert,
                      ?withArgs([ExpectedAuditLog]))),

    hoax:mock(audit_logger,
              ?expect(log,
                      ?withArgs([ExpectedAuditLog]),
                      ?andReturn({ok, ignored}))),

    deliv_event:publish({{stage, started}, verify}, Payload),

    timer:sleep(1),

    ?verifyAll.

handle_info_should_log_stage_finished_events() ->
    {Change, Scope} = stub_change_and_scope(),
    Payload = create_stage_event(finished, verify, Change, Scope),
    ExpectedAuditLog = mock_audit_stage_event(
                         Payload#stage_event.change,
                         Payload#stage_event.action,
                         Payload#stage_event.stage_name,
                         Payload#stage_event.create_time),

    hoax:mock(audit_stage_event_db,
              ?expect(insert,
                      ?withArgs([ExpectedAuditLog]))),

    hoax:mock(audit_logger,
              ?expect(log,
                      ?withArgs([ExpectedAuditLog]),
                      ?andReturn({ok, ignored}))),

    deliv_event:publish({{stage, finished}, verify}, Payload),

    timer:sleep(1),

    ?verifyAll.

handle_info_should_store_stage_started_events_in_current_state() ->
    {Change, Scope} = stub_change_and_scope(),

    Payload1 = create_stage_event(started, verify, Change, Scope),
    ExpectedAuditLog = [mock_audit_stage_event(
                          Payload#stage_event.change,
                          Payload#stage_event.action,
                          Payload#stage_event.stage_name,
                          Payload#stage_event.create_time)
                        || Payload <-
                               [Payload1]],
    hoax:mock(audit_stage_event_db,
              ?expect(insert,
                      ?withArgs(ExpectedAuditLog))),


    hoax:mock(audit_logger,
              ?expect(log,
                      ?withArgs([hd(ExpectedAuditLog)]),
                      ?andReturn({ok, ignored}))),

    deliv_event:publish({{stage, started}, verify}, Payload1),

    timer:sleep(1),

    ?assertEqual(ExpectedAuditLog, audit_subscriptions:audit_log()).

handle_info_should_store_stage_finished_events_in_current_state() ->
    {Change, Scope} = stub_change_and_scope(),

    Payload1 = create_stage_event(finished, verify, Change, Scope),
    ExpectedAuditLog = [mock_audit_stage_event(
                          Payload#stage_event.change,
                          Payload#stage_event.action,
                          Payload#stage_event.stage_name,
                          Payload#stage_event.create_time)
                        || Payload <-
                               [Payload1]],
    hoax:mock(audit_stage_event_db,
              ?expect(insert,
                      ?withArgs(ExpectedAuditLog))),

    hoax:mock(audit_logger,
              ?expect(log,
                      ?withArgs([hd(ExpectedAuditLog)]),
                      ?andReturn({ok, ignored}))),

    deliv_event:publish({{stage, finished}, verify}, Payload1),

    timer:sleep(1),

    ?assertEqual(ExpectedAuditLog, audit_subscriptions:audit_log()).

handle_info_should_store_and_persist_only_5_stage_started_events_in_current_state() ->
    {Change, Scope} = stub_change_and_scope(),

    Payload1 = create_stage_event(started, verify, Change, Scope),
    Payload2 = create_stage_event(started, build, Change, Scope),
    Payload3 = create_stage_event(started, acceptance, Change, Scope),
    Payload4 = create_stage_event(started, union, Change, Scope),
    Payload5 = create_stage_event(started, rehearsal, Change, Scope),
    Payload6 = create_stage_event(started, delivered, Change, Scope),
    Payload7 = create_stage_event(finished, delivered, Change, Scope),
    hoax:mock(audit_stage_event_db,
              ?expect(insert,
                      ?withArgs([?any]))),

    [deliv_event:publish(
       {{stage, started},
        Payload#stage_event.stage_name},
       Payload
      )
     || Payload <- [Payload1,
                    Payload2,
                    Payload3,
                    Payload4,
                    Payload5,
                    Payload6,
                    Payload7]],

    ExpectedAuditLog = [mock_audit_stage_event(
                          Payload#stage_event.change,
                          Payload#stage_event.action,
                          Payload#stage_event.stage_name,
                          Payload#stage_event.create_time)
                        || Payload <-
                               [Payload7, Payload6, Payload5, Payload4, Payload3]],
    timer:sleep(1),
    Log = audit_subscriptions:audit_log(),
    ?assertEqual(ExpectedAuditLog, Log),
    ?verifyAll.

stub_change(SubmittedAt, SubmittedBy, Title, ApprovedBy, DeliveredBy) ->
    Change = deliv_change:'#new'(),
    deliv_change:setvals([
                          {id, <<"1">>},
                          {submitted_at, SubmittedAt},
                          {submitted_by, SubmittedBy},
                          {title, Title},
                          {approved_by, ApprovedBy},
                          {delivered_by, DeliveredBy}
                         ], Change).

mock_scoping_names() ->
    [<<"Enterprise">>,  <<"Organization">>, <<"Project Name">>, <<"Pipeline Name">>].

stub_change_and_scope() ->
    SubmittedAt = os:timestamp(),
    SubmittedBy = <<"user name">>,
    Title = <<"Change Title">>,
    ApprovedBy = <<"approver name">>,
    DeliveredBy = <<"deliverer name">>,
    Change = stub_change(SubmittedAt, SubmittedBy, Title, ApprovedBy, DeliveredBy),
    [Ent, Org, Proj, Pipe] = mock_scoping_names(),
    Scope = deliv_scopes:'#new_common'([
                                        {ent_name, Ent},
                                        {org_name, Org},
                                        {proj_name, Proj},
                                        {pipe_name, Pipe}
                                       ]),
    {Change, Scope}.

mock_audit_stage_event(Change, Action, Name, CreateTime) ->
    [Ent, Org, Proj, Pipe] = mock_scoping_names(),
    #audit_stage_event{
       action = Action,
       create_time = CreateTime,
       stage_name = Name,
       status = <<"failed">>,
       ent = Ent,
       org = Org,
       proj = Proj,
       pipe = Pipe,
       change_id = deliv_change:getval(id, Change),
       change_title = deliv_change:getval(title, Change),
       submitted_at = deliv_change:getval(submitted_at, Change),
       submitted_by = deliv_change:getval(submitted_by, Change),
       approved_by = deliv_change:getval(approved_by, Change),
       delivered_by = deliv_change:getval(delivered_by, Change)
      }.

create_stage_event(Action, Name, Change, Scope) ->
    #stage_event{action = Action,
                 create_time = os:timestamp(),
                 stage_name = Name,
                 status = <<"failed">>,
                 change = Change,
                 scope = Scope}.
