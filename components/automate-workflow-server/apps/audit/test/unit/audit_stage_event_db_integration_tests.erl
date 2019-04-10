-module(audit_stage_event_db_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_events.hrl").

-compile(export_all).

insert_fixture_test_() ->
  hoax:parameterized_fixture(?MODULE, "insert_", setup, teardown).

fetch_audit_events_fixture_test_() ->
  hoax:parameterized_fixture(?MODULE, "fetch_audit_events_", setup, teardown).

listener_fixture_test_() ->
  hoax:parameterized_fixture(?MODULE, "listener_", setup, teardown).

setup() ->
    error_logger:tty(false),
    setup_audit_subscriptions(),
    eu_database:setup(),
    eu_data:with_enterprise(<<"deliv_change_test_enterprise">>,
       eu_data:with_organization(<<"deliv_change_test_organization">>,
        eu_data:with_project(<<"deliv_change_test_project">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_change_eunit_user">>),
                Patchset = eu_data:create_patchset(Enterprise, User, Organization,
                                        Project, Pipeline, "not_master"),
                Change = eu_data:change_from_patchset(Patchset),
                Scope = deliv_scopes:from_change(Change),
                SubmittedAt = deliv_change:getval(submitted_at, Change),
                UpdatedChange = deliv_change:setvals([{submitted_at,
                                                       chef_utils:trunc_timestamp(SubmittedAt)}], Change),
                {Enterprise, UpdatedChange, Scope}
            end)))).

setup_audit_subscriptions() ->
    application:set_env(delivery, stages_data, [
                                                {verify, do_not_care},
                                                {build, do_not_care},
                                                {acceptance, do_not_care},
                                                {union, do_not_care},
                                                {rehearsal, do_not_care},
                                                {delivered, do_not_care}
                                               ]),
    application:set_env(audit, max_events_in_memory, 5),
    application:start(gproc).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).


insert_should_serialize_event_to_database({_Enterprise,Change, Scope}) ->
  AuditEvent = create_audit_event(Change, Scope, 0),
  audit_stage_event_db:insert(AuditEvent),
  [DeserializedEvent] =
  audit_stage_event_db:fetch_audit_events(5),
  List1 = tuple_to_list(AuditEvent),
  List2 = tuple_to_list(DeserializedEvent),
  [?assertEqual(Val1, Val2) || {Val1, Val2} <- lists:zip(List1, List2)],
  ?assertEqual(AuditEvent, DeserializedEvent).

fetch_audit_events_should_return_in_desc_order_obeying_limit({_Enterprise,Change, Scope}) ->
  AuditEvents = [create_audit_event(Change, Scope, Val) || Val <- lists:seq(1,3)],
  [audit_stage_event_db:insert(AuditEvent) || AuditEvent <- AuditEvents],
  [First, Second, Third] = AuditEvents,
  ?assertEqual([Third],
               audit_stage_event_db:fetch_audit_events(1)),
  ?assertEqual([Third, Second],
               audit_stage_event_db:fetch_audit_events(2)),
  ?assertEqual([Third, Second, First],
               audit_stage_event_db:fetch_audit_events(3)).

fetch_audit_events_should_return_in_desc_order_by_serial_obeying_limit({_Enterprise,Change, Scope}) ->
  AuditEvents = [create_audit_event(Change, Scope, 0) || _Val <- lists:seq(1,3)],
  [audit_stage_event_db:insert(AuditEvent) || AuditEvent <- AuditEvents],
  [First, Second, Third] = AuditEvents,
  ?assertEqual([Third],
               audit_stage_event_db:fetch_audit_events(1)),
  ?assertEqual([Third, Second],
               audit_stage_event_db:fetch_audit_events(2)),
  ?assertEqual([Third, Second, First],
               audit_stage_event_db:fetch_audit_events(3)).


listener_should_query_database_for_initial_state({_Enterprise,Change, Scope}) ->
  AuditEvent = create_audit_event(Change, Scope, 0),
  audit_stage_event_db:insert(AuditEvent),
  audit_subscriptions:start_link(),
  Events = audit_subscriptions:audit_log(),
  ?assertEqual(Events, audit_stage_event_db:fetch_audit_events(5)).

create_audit_event(Change, Scope, TimeIncrement) ->
  Seconds = calendar:datetime_to_gregorian_seconds(
              chef_utils:trunc_timestamp(
                calendar:now_to_universal_time(os:timestamp())
               )
             ),
  UpdatedTime = calendar:gregorian_seconds_to_datetime(Seconds + TimeIncrement),
  audit_subscriptions:build_audit_event(
                 verify,
                 #stage_event{
                    action = started,
                    change = Change,
                    status = <<"passed">>,
                    scope  = Scope,
                    create_time = UpdatedTime
                   }).
