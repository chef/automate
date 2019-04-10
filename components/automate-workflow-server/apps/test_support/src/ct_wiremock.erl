%% @doc Start, stop and communicate with a Wiremock process.
-module(ct_wiremock).

%% API
-export([start/1, stop/1, url/0, url/1]).

-define(WIREMOCK_JAR, "/usr/share/wiremock/lib/wiremock-standalone.jar").
-define(WIREMOCK_PORT, "8888").

%% @doc Start a wiremock process using the wiremock directory in the CT folder
%% as the root directory.
-spec start(module()) -> OsPid::integer().
start(Module) ->
    Cmd = "java -jar ~s --port ~s --verbose --root-dir ~s > ~s 2>&1 & echo $!",
    case chef_utils:run_cmd([io_lib:format(Cmd, [?WIREMOCK_JAR,
                                                  ?WIREMOCK_PORT,
                                                  app_test_helpers:project_path(Module, "test/ct/wiremock"),
                                                  app_test_helpers:project_path(Module, "test/ct/logs/wiremock.log")])]) of
        {0, StdErrAndOut} ->
            OsPid = chef_utils:to_int(binary:replace(StdErrAndOut, <<"\n">>, <<>>)),
            wait(5, OsPid),
            OsPid;
        {1, StdErrAndOut} ->
            erlang:error({wiremock_failed_to_start, StdErrAndOut});
        {error, timeout} ->
            erlang:error({wiremock_failed_to_start, timeout})
    end.

%% @doc Stop the specified wiremock process.
-spec stop(non_neg_integer()) -> _.
stop(OsPid) ->
    os:cmd(io_lib:format("kill ~b", [OsPid])).

%% @doc Return the base URL of wiremock only.
-spec url() -> iodata().
url() ->
    io_lib:format("http://localhost:~s", [?WIREMOCK_PORT]).

%% @doc Evaluate the route and return the full URL wiremocked URL
-spec url(iodata()) -> string().
url(Route) ->
    chef_utils:iodata_to_str(["http://localhost:", ?WIREMOCK_PORT, Route]).

%% @private
%% @doc The start/0 fun starts the process and returns the PID but wiremock still
%% takes a few moments to start. This fun waits for the process to be accessible
%% before returning to prevent a race condition in your CT tests.
-spec wait(non_neg_integer(), non_neg_integer()) -> ok.
wait(Retries, OsPid) when Retries > 0 ->
    case deliv_http:req(get, url("/__admin")) of
        {ok, _, _, _} -> ok;
        {error, Why} ->
            ct:pal("Wiremock failed to start: ~p", [Why]),
            timer:sleep(1000),
            wait(Retries - 1, OsPid)
    end;
wait(0, OsPid) ->
    erlang:error({wiremock_failed_to_start, OsPid}).
