-module(audit_web_utils_tests).

-include_lib("hoax/include/hoax.hrl").

-include("audit_events.hrl").

-compile([export_all]).

to_json_fixture_test_() ->
    hoax:fixture(?MODULE).

to_ejson_returns_audit_state_started_record_as_ejson() ->
    StartedAt = calendar:now_to_universal_time(os:timestamp()),
    CreateTime = calendar:now_to_universal_time(os:timestamp()),
    FormattedStartedAt = chef_utils:format_timestamp(StartedAt),
    FormattedCreateTime = chef_utils:format_timestamp(CreateTime),
    CreatedEvent = #audit_stage_event{
                      change_id = <<"aaabbbccc">>,
                      change_title = <<"Test Change">>,
                      action = started,
                      status = <<"running">>,
                      create_time = CreateTime,
                      ent = <<"Test Ent">>,
                      org = <<"Test Org">>,
                      pipe = <<"Test Pipe">>,
                      proj = <<"Test Proj">>,
                      stage_name = delivered,
                      submitted_at = StartedAt,
                      submitted_by = <<"Test User">>,
                      approved_by = <<"Test Approver">>,
                      delivered_by = <<"Test Deliverer">>},
    [{ExpectedContent} | _] = ExpectedEJson =
        [{
           [{<<"change_id">>,<<"aaabbbccc">>},
            {<<"change_title">>,<<"Test Change">>},
            {<<"status">>, <<"running">>},
            {<<"ent">>, <<"Test Ent">>},
            {<<"org">>, <<"Test Org">>},
            {<<"pipe">>, <<"Test Pipe">>},
            {<<"proj">>, <<"Test Proj">>},
            {<<"stage_name">>, delivered},
            {<<"action">>, started},
            {<<"create_time">>, FormattedCreateTime},
            {<<"submitted_at">>, FormattedStartedAt},
            {<<"submitted_by">>, <<"Test User">>},
            {<<"approved_by">>, <<"Test Approver">>},
            {<<"delivered_by">>, <<"Test Deliverer">>}
           ]
         }],
    [{ReturnedContent} | _] = ReturnedEJson = audit_web_utils:to_ejson([CreatedEvent]),
    [?assertEqual(Expected, Val) || {Expected, Val} <-
                                        lists:zip(ExpectedContent,
                                                  ReturnedContent)],
    ?assertEqual(ExpectedEJson, ReturnedEJson).

to_json_json() ->
    StartedAt = calendar:now_to_universal_time(os:timestamp()),
    CreateTime = calendar:now_to_universal_time(os:timestamp()),
    FormattedStartedAt = chef_utils:format_timestamp(StartedAt),
    FormattedCreateTime = chef_utils:format_timestamp(CreateTime),
    CreatedEvent = #audit_stage_event{
                      change_id = <<"aaabbbccc">>,
                      change_title = <<"Test Change">>,
                      status = <<"running">>,
                      ent = <<"Test Ent">>,
                      org = <<"Test Org">>,
                      pipe = <<"Test Pipe">>,
                      proj = <<"Test Proj">>,
                      stage_name = delivered,
                      action = started,
                      create_time = CreateTime,
                      submitted_at = StartedAt,
                      submitted_by = <<"Test User">>,
                      approved_by = <<"Test Approver">>,
                      delivered_by = <<"Test Deliverer">>},
    ExpectedEJson =
        [{
           [{<<"change_id">>,<<"aaabbbccc">>},
            {<<"change_title">>,<<"Test Change">>},
            {<<"status">>, <<"running">>},
            {<<"ent">>, <<"Test Ent">>},
            {<<"org">>, <<"Test Org">>},
            {<<"pipe">>, <<"Test Pipe">>},
            {<<"proj">>, <<"Test Proj">>},
            {<<"stage_name">>, delivered},
            {<<"action">>, started},
            {<<"create_time">>, FormattedCreateTime},
            {<<"submitted_at">>, FormattedStartedAt},
            {<<"submitted_by">>, <<"Test User">>},
            {<<"approved_by">>, <<"Test Approver">>},
            {<<"delivered_by">>, <<"Test Deliverer">>}
           ]
         }],
    hoax:mock(chef_json,
              ?expect(encode,
                      ?withArgs([ExpectedEJson]),
                      ?andReturn(json))),

    ?assertEqual(json, audit_web_utils:to_json([CreatedEvent])),

    ?verifyAll.
