-module(jobs_rest_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_rest_test_() ->
    [
     hoax:fixture(?MODULE, "resource_exists_")
    ].

resource_exists_when_there_is_no_hostname_in_bindings_returns_false() ->
    hoax:expect(receive
                    cowboy_req:binding(hostname, req) -> {undefined, req1}
                end),

    Actual = jobs_rest:resource_exists(req, #handler{ent_name = <<"ent_name">>}),

    ?assertEqual({false, req1, #runner{}}, Actual),
    ?verifyAll.

resource_exists_when_there_is_a_hostname_in_bindings_and_fetch_returns_nothing_returns_false() ->
    EntName = <<"ent_name">>,
    hoax:expect(receive
                    cowboy_req:binding(hostname, req) -> {runner_hostname, req1};
                    jobs_runners_state:fetch(EntName, runner_hostname) -> []
                end),

    Actual = jobs_rest:resource_exists(req, #handler{ent_name = EntName}),

    ?assertEqual({false, req1, #runner{}}, Actual),
    ?verifyAll.

resource_exists_when_there_is_an_hostname_in_bindings_and_fetch_returns_runner_returns_true_and_updates_state_record() ->
    EntName = <<"ent_name">>,
    Runner = #runner{ enterprise_name = EntName, hostname = runner_hostname },
    hoax:expect(receive
                    cowboy_req:binding(hostname, req) -> {runner_hostname, req1};
                    jobs_runners_state:fetch(EntName, runner_hostname) -> [Runner]
                end),

    Actual = jobs_rest:resource_exists(req, #handler{ent_name = EntName}),

    ?assertEqual({true, req1, Runner}, Actual),
    ?verifyAll.
