-module(jobs_criteria_queue_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_criteria_queue_test_() ->
    [
     eunit_sugar:fixture(?MODULE, "in_"),
     eunit_sugar:fixture(?MODULE, "find_by_id_"),
     eunit_sugar:fixture(?MODULE, "remove_job_by_id_"),
     hoax:fixture(?MODULE, "find_job_for_runner_")
    ].

in_when_queue_is_empty_queues_a_job() ->
    Job = #job{id = <<"id1">>},
    ExpectedQueue = [Job],

    Actual = jobs_criteria_queue:queue_job(Job, []),
    ?assertEqual(ExpectedQueue, Actual).

in_when_queue_has_a_job_already_adds_job_at_the_end_of_the_queue() ->
    Job1 = #job{id = <<"id1">>},
    Job2 = #job{id = <<"id2">>},
    ExpectedQueue = [Job1, Job2],

    Actual = jobs_criteria_queue:queue_job(Job2, [Job1]),
    ?assertEqual(ExpectedQueue, Actual).

find_by_id_when_job_is_not_present_returns_not_found() ->
    Id = <<"id1">>,
    Job = #job{id = Id},

    Actual = jobs_criteria_queue:find_by_id(<<"no-such-id">>, [Job]),
    ?assertEqual(not_found, Actual).

find_by_id_when_job_is_present_returns_job() ->
    Id = <<"id1">>,
    Job = #job{id = Id},

    Actual = jobs_criteria_queue:find_by_id(Id, [Job]),
    ?assertEqual(Job, Actual).

find_job_for_runner_when_queue_is_nonempty_and_criteria_are_not_matching_returns_not_found_and_unchanged_queue() ->
    Job = #job{job_criteria = job_criteria},
    Queue = [Job],
    hoax:expect(receive
                    jobs_criteria:match(job_criteria, runner_properties) -> false
                end),

    Actual = jobs_criteria_queue:find_job_for_runner(pid, runner_properties, Queue),
    ?assertEqual({not_found, Queue}, Actual),
    ?verifyAll.

find_job_for_runner_when_queue_is_empty_returns_no_job_found_for_runner_and_unchanged_queue() ->
    Queue = [],
    Actual = jobs_criteria_queue:find_job_for_runner(pid, runner_properties, Queue),
    ?assertEqual({not_found, Queue}, Actual),
    ?verifyAll.

find_job_for_runner_when_queue_has_more_than_one_element_and_criteria_are_matching_returns_job_and_new_queue() ->
    Now = {{2016, 9, 13}, {2, 0, 0}},
    Job1 = #job{job_criteria = job_criteria_1},
    Job2 = #job{job_criteria = job_criteria_2},
    MatchingJob = #job{job_criteria = matching},
    ExpectedJob = MatchingJob#job{status = running, runner_pid = pid, started_running_at = Now},
    InitialQueue = [Job1, MatchingJob, Job2],
    ExpectedQueue = [Job1, ExpectedJob, Job2],
    hoax:expect(receive
                    calendar:universal_time() -> Now;
                    jobs_criteria:match(job_criteria_1, runner_properties) -> false;
                    jobs_criteria:match(matching, runner_properties) -> true
                end),

    Actual = jobs_criteria_queue:find_job_for_runner(pid, runner_properties, InitialQueue),
    ?assertEqual({ExpectedJob, ExpectedQueue}, Actual),
    ?verifyAll.

remove_job_by_id_when_job_is_present_returns_job_and_jobs_queue_minus_that_job() ->
    Id = <<"id1">>,
    Job = #job{id = Id},
    Queue = [Job],
    ExpectedQueue = [],

    Actual = jobs_criteria_queue:remove_job_by_id(Id, Queue),
    ?assertEqual({Job, ExpectedQueue}, Actual).

remove_job_by_id_when_job_is_present_in_front_of_queue_returns_job_and_jobs_queue_minus_that_job() ->
    Id1 = <<"id1">>,
    Job1 = #job{id = Id1},
    Id2 = <<"id2">>,
    Job2 = #job{id = Id2},
    Id3 = <<"id3">>,
    Job3 = #job{id = Id3},
    Queue = [Job1, Job2, Job3],
    ExpectedQueue = [Job2, Job3],

    Actual = jobs_criteria_queue:remove_job_by_id(Id1, Queue),
    ?assertEqual({Job1, ExpectedQueue}, Actual).

remove_job_by_id_when_job_is_present_in_middle_of_queue_returns_ok_and_jobs_queue_minus_that_job() ->
    Id1 = <<"id1">>,
    Job1 = #job{id = Id1},
    Id2 = <<"id2">>,
    Job2 = #job{id = Id2},
    Id3 = <<"id3">>,
    Job3 = #job{id = Id3},
    Queue = [Job1, Job2, Job3],
    ExpectedQueue = [Job1, Job3],

    Actual = jobs_criteria_queue:remove_job_by_id(Id2, Queue),
    ?assertEqual({Job2, ExpectedQueue}, Actual).

remove_job_by_id_when_job_is_present_at_end_of_queue_returns_ok_and_jobs_queue_minus_that_job() ->
    Id1 = <<"id1">>,
    Job1 = #job{id = Id1},
    Id2 = <<"id2">>,
    Job2 = #job{id = Id2},
    Id3 = <<"id3">>,
    Job3 = #job{id = Id3},
    Queue = [Job1, Job2, Job3],
    ExpectedQueue = [Job1, Job2],

    Actual = jobs_criteria_queue:remove_job_by_id(Id3, Queue),
    ?assertEqual({Job3, ExpectedQueue}, Actual).

remove_job_by_id_when_job_is_not_present_returns_not_found_and_old_jobs_queue() ->
    Id = <<"id1">>,
    Job = #job{id = Id},
    ExpectedQueue = Queue = [Job],

    Actual = jobs_criteria_queue:remove_job_by_id(<<"no-such-id">>, Queue),
    ?assertEqual({not_found, ExpectedQueue}, Actual).

find_job_for_runner_when_only_1_pending_job_returns_job_and_new_queue() ->
    Now = {{2016, 9, 13}, {2, 0, 0}},
    Job1 = #job{job_criteria = job_criteria_1, status = running, runner_pid = other_pid},
    MatchingJob = #job{job_criteria = matching, status = pending},
    ExpectedJob = MatchingJob#job{status = running, runner_pid = pid, started_running_at = Now},
    InitialQueue = [Job1, MatchingJob],
    ExpectedQueue = [Job1, ExpectedJob],
    hoax:expect(receive
                    calendar:universal_time() -> Now;
                    jobs_criteria:match(matching, runner_properties) -> true
                end),

    Actual = jobs_criteria_queue:find_job_for_runner(pid, runner_properties, InitialQueue),
    ?assertEqual({ExpectedJob, ExpectedQueue}, Actual),
    ?verifyAll.
