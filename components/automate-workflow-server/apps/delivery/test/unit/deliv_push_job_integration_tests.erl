-module(deliv_push_job_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../../src/deliverance_types.hrl").

-compile(export_all).

push_job_test_() ->
    hoax:fixture(?MODULE, "push_job").

push_job_failes_when_no_node_available() ->
    error_logger:tty(false),
    {ok, Pid} = deliv_push_job:start_link("phase pid", <<"deliverance">>, <<"command">>),
    unlink(Pid),
    application:set_env(delivery, push_jobs_overall_timeout, 10),

    % State = #state{node_search_query="node search query"},
    hoax:mock(deliv_chef_api,
              [?expect(search,
                      ?withArgs(["node", ?any,<<"{\"name\":[\"name\"]}">>]),
                      ?andReturn({ok, #search{total = 0,
                                   start = 1,
                                   rows = []
                                  }})),
               ?expect(push_node_states,
                      ?withArgs([]),
                      ?andReturn({ok, [{[{<<"node_name">>, <<"alice">>},
                              {<<"availability">>,
                               <<"available">>}]}]}))
              ]),
    hoax:mock(deliv_phase,
                ?expect(no_worker,
                  ?withArgs(["phase pid"]))),
    deliv_push_job:start(Pid),

    erlang:exit(Pid, shutdown),
    error_logger:tty(true),
    ?verifyAll.



push_job_retries_unavailable_when_less_than_max_retries() ->
    error_logger:tty(false),
    {ok, Pid} = deliv_push_job:start_link("phase pid", <<"deliverance">>, <<"command">>),
    unlink(Pid),
    application:set_env(delivery, push_jobs_overall_timeout, 10),
    application:set_env(delivery, push_jobs_retry_interval, 1),
    application:set_env(delivery, push_jobs_max_retries, 3),

    JobId = "job_id",
    Nodes = [{<<"unavailable">>, ["nodename"]}],
    Status = #push_job_status{status = <<"quorum_failed">>, id = 1, nodes = Nodes},
    hoax:mock(deliv_chef_api,
               [?expect(search,
                       ?withArgs(["node", ?any,<<"{\"name\":[\"name\"]}">>]),
                       ?andReturn({ok, #search{total = 1,
                                    start = 1,
                                    rows = [{[{<<"data">>,
                                               {[{<<"name">>,
                                                 <<"alice">>}]}}]}]
                                   }})),
                ?expect(push_node_states,
                       ?withArgs([]),
                       ?andReturn({ok, [{[{<<"node_name">>, <<"alice">>},
                               {<<"availability">>,
                                <<"available">>}]}]}))
               ]),

    hoax:mock(deliv_chef_api,
              [?expect(push_job_start,
                       ?withArgs([<<"command">>, [<<"alice">>], 1]),
                       ?andReturn({ok, JobId}), 3),
               ?expect(push_job_status,
                       ?withArgs(["job_id"]),
                       ?andReturn({ok, Status}))]),
    hoax:mock(deliv_phase,
              ?expect(update,
                      ?withArgs(["phase pid", Status]))),

    deliv_push_job:start(Pid),

    timer:sleep(100),
    erlang:exit(Pid, shutdown),
    error_logger:tty(true),
    ?verifyAll.
