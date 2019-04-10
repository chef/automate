-module(jobs_job_json_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_job_json_test_() ->
    [
     hoax:fixture(?MODULE, to_json_)
    ].

to_json_when_given_job_record_properly_converts_submitted_at_and_returns_ejson() ->
    JobId = <<"id1">>,
    ChangeId = change_id,
    ChangeTitle = change,
    OrgName = org,
    Stage = stage,
    Phase = phase,
    Status = running,
    Project = project,
    SubmittedAt = {{2016, 9, 13}, {1, 52, 4}},
    Now = {{2016, 9, 13}, {2, 0, 0}},
    Job = #job{id = JobId,
               status = Status,
               started_running_at = undefined,
               command = <<"delivery-cmd">>,
               deliv_ssh_job_pid = list_to_pid("<0.1.2>"),
               job_criteria = #deliv_ssh_job_criteria{},
               deliv_change_info = #deliv_change_info{
                                      id = ChangeId,
                                      title = ChangeTitle,
                                      org = OrgName,
                                      stage = Stage,
                                      phase = Phase,
                                      project = Project,
                                      submitted_at = SubmittedAt}},
    hoax:expect(receive
                    calendar:universal_time() -> Now;
                    calendar:datetime_to_gregorian_seconds(Now) -> 63640951200;
                    calendar:datetime_to_gregorian_seconds(SubmittedAt) -> 63640950724;
                    calendar:seconds_to_daystime(476) -> {0, {0, 7, 56}}
                end),
    Expected = {[{<<"id">>, <<"id1">>},
                   {<<"project">>, project},
                   {<<"change">>, {[
                                    {<<"id">>, ChangeId},
                                    {<<"title">>, ChangeTitle}
                                   ]}},
                   {<<"org">>, org},
                   {<<"stage">>, stage},
                   {<<"phase">>, phase},
                   {<<"status">>, running},
                   {<<"submittedAt">>, <<"2016-09-13 01:52:04">>},
                   {<<"timeInState">>, <<"00:07:56">>}]},

    ?assertEqual(Expected, jobs_job_json:to_json(Job)),
    ?verifyAll.

to_json_when_given_running_job_record_properly_converts_started_running_at_and_returns_ejson() ->
    JobId = <<"id1">>,
    ChangeId = change_id,
    ChangeTitle = change,
    OrgName = org,
    Stage = stage,
    Phase = phase,
    Status = running,
    Project = project,
    SubmittedAt = {{2016, 9, 13}, {1, 40, 4}},
    StartedRunningAt = {{2016, 9, 13}, {1, 52, 4}},
    Now = {{2016, 9, 13}, {2, 0, 0}},
    Job = #job{id = JobId,
               status = Status,
               started_running_at = StartedRunningAt,
               command = <<"delivery-cmd">>,
               deliv_ssh_job_pid = list_to_pid("<0.1.2>"),
               job_criteria = #deliv_ssh_job_criteria{},
               deliv_change_info = #deliv_change_info{
                                      id = ChangeId,
                                      title = ChangeTitle,
                                      org = OrgName,
                                      stage = Stage,
                                      phase = Phase,
                                      project = Project,
                                      submitted_at = SubmittedAt}},
    hoax:expect(receive
                    calendar:universal_time() -> Now;
                    calendar:datetime_to_gregorian_seconds(Now) -> 63640951200;
                    calendar:datetime_to_gregorian_seconds(StartedRunningAt) -> 63640950724;
                    calendar:seconds_to_daystime(476) -> {0, {0, 7, 56}}
                end),
    Expected = {[{<<"id">>, <<"id1">>},
                   {<<"project">>, project},
                   {<<"change">>, {[
                                    {<<"id">>, ChangeId},
                                    {<<"title">>, ChangeTitle}
                                   ]}},
                   {<<"org">>, org},
                   {<<"stage">>, stage},
                   {<<"phase">>, phase},
                   {<<"status">>, running},
                   {<<"submittedAt">>, <<"2016-09-13 01:40:04">>},
                   {<<"timeInState">>, <<"00:07:56">>}]},

    ?assertEqual(Expected, jobs_job_json:to_json(Job)),
    ?verifyAll.

to_json_when_given_job_record_with_submitted_at_time_as_given_from_the_database_truncates_it_and_returns_ejson() ->
    JobId = <<"id1">>,
    ChangeId = change_id,
    ChangeTitle = change,
    OrgName = org,
    Stage = stage,
    Phase = phase,
    Project = project,
    Status = running,
    SubmittedAt = {{2016, 9, 13}, {1, 52, 4.31}},
    SubmittedAtTruncated = {{2016, 9, 13}, {1, 52, 4}},
    Now = {{2016, 9, 13}, {2, 0, 0}},
    Job = #job{id = JobId,
               status = Status,
               started_running_at = undefined,
               command = <<"delivery-cmd">>,
               deliv_ssh_job_pid = list_to_pid("<0.1.2>"),
               job_criteria = #deliv_ssh_job_criteria{},
               deliv_change_info = #deliv_change_info{
                                      id = ChangeId,
                                      title = ChangeTitle,
                                      org = OrgName,
                                      stage = Stage,
                                      phase = Phase,
                                      project = Project,
                                      submitted_at = SubmittedAt}},
    hoax:expect(receive
                    calendar:universal_time() -> Now;
                    calendar:datetime_to_gregorian_seconds(Now) -> 63640951200;
                    calendar:datetime_to_gregorian_seconds(SubmittedAtTruncated) -> 63640950724;
                    calendar:seconds_to_daystime(476) -> {0, {0, 7, 56}}
                end),
    Expected = {[{<<"id">>, <<"id1">>},
                   {<<"project">>, project},
                   {<<"change">>, {[
                                    {<<"id">>, ChangeId},
                                    {<<"title">>, ChangeTitle}
                                   ]}},
                   {<<"org">>, org},
                   {<<"stage">>, stage},
                   {<<"phase">>, phase},
                   {<<"status">>, running},
                   {<<"submittedAt">>, <<"2016-09-13 01:52:04">>},
                   {<<"timeInState">>, <<"00:07:56">>}]},

    ?assertEqual(Expected, jobs_job_json:to_json(Job)),
    ?verifyAll.

to_json_when_given_job_record_submitted_at_time_is_more_than_a_day_displays_time_in_state_with_days() ->
    JobId = <<"id1">>,
    ChangeId = change_id,
    ChangeTitle = change,
    OrgName = org,
    Stage = stage,
    Phase = phase,
    Status = running,
    Project = project,
    SubmittedAt = {{2016, 9, 13}, {1, 52, 4}},
    Now = {{2016, 9, 15}, {2, 0, 0}},
    Job = #job{id = JobId,
               status = Status,
               started_running_at = undefined,
               command = <<"delivery-cmd">>,
               deliv_ssh_job_pid = list_to_pid("<0.1.2>"),
               job_criteria = #deliv_ssh_job_criteria{},
               deliv_change_info = #deliv_change_info{
                                      id = ChangeId,
                                      title = ChangeTitle,
                                      org = OrgName,
                                      stage = Stage,
                                      phase = Phase,
                                      project = Project,
                                      submitted_at = SubmittedAt}},
    hoax:expect(receive
                    calendar:universal_time() -> Now;
                    calendar:datetime_to_gregorian_seconds(Now) -> 63641124000;
                    calendar:datetime_to_gregorian_seconds(SubmittedAt) -> 63640950724;
                    calendar:seconds_to_daystime(173276) -> {2, {0, 7, 56}}
                end),
    Expected = {[{<<"id">>, <<"id1">>},
                   {<<"project">>, project},
                   {<<"change">>, {[
                                    {<<"id">>, ChangeId},
                                    {<<"title">>, ChangeTitle}
                                   ]}},
                   {<<"org">>, org},
                   {<<"stage">>, stage},
                   {<<"phase">>, phase},
                   {<<"status">>, running},
                   {<<"submittedAt">>, <<"2016-09-13 01:52:04">>},
                   {<<"timeInState">>, <<"2d 00:07:56">>}]},

    ?assertEqual(Expected, jobs_job_json:to_json(Job)),
    ?verifyAll.

to_json_when_lists_returns_list_of_jobs_json() ->
    JobId1 = <<"id1">>,
    JobId2 = <<"id2">>,
    ChangeId = change_id,
    ChangeTitle = change,
    OrgName = org,
    Stage = stage,
    Phase = phase,
    Project = project,
    Status = running,
    SubmittedAt = {{2016, 9, 13}, {1, 52, 4}},
    Now = {{2016, 9, 13}, {2, 0, 0}},
    Job1 = #job{id = JobId1,
                status = Status,
                started_running_at = undefined,
                command = <<"delivery-cmd">>,
                deliv_ssh_job_pid = list_to_pid("<0.1.3>"),
                job_criteria = #deliv_ssh_job_criteria{},
                deliv_change_info = #deliv_change_info{
                                       id = ChangeId,
                                       title = ChangeTitle,
                                       org = OrgName,
                                       stage = Stage,
                                       phase = Phase,
                                       project = Project,
                                       submitted_at = SubmittedAt}},
    Job2 = Job1#job{id = JobId2,
                    started_running_at = undefined,
                    deliv_ssh_job_pid = list_to_pid("<0.1.2>")},
    hoax:expect(receive
                    calendar:universal_time() -> Now;
                    calendar:datetime_to_gregorian_seconds(Now) -> 63640951200;
                    calendar:datetime_to_gregorian_seconds(SubmittedAt) -> 63640950724;
                    calendar:seconds_to_daystime(476) -> {0, {0, 7, 56}}
                end),
    Expected1 = {[{<<"id">>, <<"id1">>},
                  {<<"project">>, project},
                  {<<"change">>, {[
                                   {<<"id">>, ChangeId},
                                   {<<"title">>, ChangeTitle}
                                  ]}},
                  {<<"org">>, org},
                  {<<"stage">>, stage},
                  {<<"phase">>, phase},
                  {<<"status">>, running},
                  {<<"submittedAt">>, <<"2016-09-13 01:52:04">>},
                  {<<"timeInState">>, <<"00:07:56">>}]},
    Expected2 = ej:set([<<"id">>], Expected1, <<"id2">>),

    ?assertEqual([Expected1, Expected2], jobs_job_json:to_json([Job1, Job2])),
    ?verifyAll.

to_json_when_lists_returns_empty_list_properly() ->
    ?assertEqual([], jobs_job_json:to_json([])).
