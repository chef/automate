-module(insights_stage_event_listener_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_events.hrl").
-include("insights.hrl").

-compile([export_all]).

fixture_test_() ->
  hoax:fixture(?MODULE).

start_link_starts_listener() ->
  hoax:mock(insights_listener,
            ?expect(start_link,
                    ?withArgs([insights_stage_event_listener]))),
  insights_stage_event_listener:start_link(),
  ?verifyAll.

subscribe_to_change_events() ->
  hoax:mock(deliv_stage,
            ?expect(subscribe_stage_events,
                    ?withArgs([]),
                    ?andReturn(ok))),

  ok = insights_stage_event_listener:subscribe_to_events(),
  ?verifyAll.

handle_event_for_change_events_returns_ejson() ->
    ChangeId = <<"46216d32-213e-4d89-995d-ddd57bd5cf63">>,
    ChangeTitle = <<"add debug logging for blocked sets">>,
    ChangeSubmittedBy = <<"mcquinc">>,
    ChangeApprovedBy = <<"vjeffrey">>,
    ChangeDeliveredBy = undefined,

    StageEjson = {[
      {<<"change_id">>, ChangeId},
      {<<"change_title">>, ChangeTitle},
      {<<"status">>, <<"running">>},
      {<<"enterprise_name">>, <<"Chef">>},
      {<<"organization_name">>, <<"Chef_Delivery">>},
      {<<"pipeline_name">>, <<"master">>},
      {<<"project_name">>, <<"delivery">>},
      {<<"stage_name">>, <<"acceptance">>},
      {<<"action">>, <<"running">>},
      {<<"create_time">>, <<"2016-01-14 15:05:35">>},
      {<<"submitted_at">>, <<"2016-01-14 00:48:00">>},
      {<<"submitted_by">>, ChangeSubmittedBy},
      {<<"approved_by">>, ChangeApprovedBy},
      {<<"delivered_by">>, ChangeDeliveredBy}
    ]},

    Change = deliv_change:'#new'(),
    hoax:mock(deliv_change, [
      ?expect(getval, ?withArgs([submitted_at, Change]), ?andReturn({{2016, 1, 14}, {0, 48, 0}})),
      ?expect(getval, ?withArgs([id, Change]), ?andReturn(ChangeId)),
      ?expect(getval, ?withArgs([title, Change]), ?andReturn(ChangeTitle)),
      ?expect(getval, ?withArgs([submitted_by, Change]), ?andReturn(ChangeSubmittedBy)),
      ?expect(getval, ?withArgs([approved_by, Change]), ?andReturn(ChangeApprovedBy)),
      ?expect(getval, ?withArgs([delivered_by, Change]), ?andReturn(ChangeDeliveredBy))
    ]),

    EventPayload = #stage_event{
                      action = running,
                      create_time = {{2016, 01, 14}, {15, 5, 35}},
                      stage_name = acceptance,
                      status = <<"running">>,
                      scope = deliv_scopes:'#new_common'([
                                          {ent_name, <<"Chef">>},
                                          {org_name, <<"Chef_Delivery">>},
                                          {proj_name, <<"delivery">>},
                                          {pipe_name, <<"master">>}
                                         ]),
                      stage_run = deliv_stage_run:fromlist([]),
                      change = Change
                      },

    Event = #insights_event{},
    hoax:mock(insights_listener,
              ?expect(new_event,
                      ?withArgs([stage, started, StageEjson]),
                      ?andReturn(Event))),

    Result = insights_stage_event_listener:handle_event({{stage, started}, acceptance}, EventPayload),

    ?assertEqual(Event, Result),
    ?verifyAll.
