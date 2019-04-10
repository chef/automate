-module(jobs_routes_tests).

-include("jobs_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

routes_should_return_jobs_route_list_test() ->
    ok = application:set_env(delivery, read_ttl_secs, 3600),
    ok = application:set_env(delivery, write_ttl_secs, 3600),

    State = deliv_routes:set_ttls(#handler{}),
    RunnersRoute = {
      prefix() ++ ":ent_name/runners", jobs_hand_runners,
      State#handler{authz = {enterprise,[{deliv_web_utils:bin_method(get), ?ALL_ROLES},
                                         {deliv_web_utils:bin_method(post), [<<"admin">>]}]}}},

    RunnersStreamingRoute = {
      prefix() ++ ":ent_name/runners/streaming", jobs_hand_runners_sse,
      State#handler{authz = {enterprise,[{deliv_web_utils:bin_method(get), ?ALL_ROLES}]}}},

    NamedRunnersRoute = {
      prefix() ++ ":ent_name/runners/:hostname", jobs_hand_runners_named,
      State#handler{authz = {enterprise,[{deliv_web_utils:bin_method(get), ?ALL_ROLES},
                                         {deliv_web_utils:bin_method(delete), [<<"admin">>]}]}}},

    NamedRunnersHealthRoute = {
      prefix() ++ ":ent_name/runners/:hostname/health", jobs_hand_runners_named_health,
      State#handler{authz = {enterprise,[{deliv_web_utils:bin_method(post), ?ALL_ROLES}]}}},

    JobsRoute = {
      prefix() ++ ":ent_name/jobs", jobs_handler,
      State#handler{authz = {enterprise,[{deliv_web_utils:bin_method(get), ?ALL_ROLES}]}}},

    JobsStreamingRoute = {
      prefix() ++ ":ent_name/jobs/streaming", jobs_handler_sse,
      State#handler{authz = {enterprise,[
                                         {deliv_web_utils:bin_method(get), ?ALL_ROLES}
                                        ]}}},

    NamedJobsRoute = {
      prefix() ++ ":ent_name/jobs/:job_id", jobs_handler_named,
      State#handler{authz = {enterprise,[
                                         {deliv_web_utils:bin_method(get), ?ALL_ROLES},
                                         {deliv_web_utils:bin_method(delete), lists:delete(<<"observer">>, ?ALL_ROLES)}
                                        ]}}},

    ExpectedRoutes = [
                      RunnersRoute,
                      RunnersStreamingRoute,
                      NamedRunnersRoute,
                      NamedRunnersHealthRoute,
                      JobsRoute,
                      JobsStreamingRoute,
                      NamedJobsRoute
                     ],
    ?assertEqual(ExpectedRoutes, jobs_routes:routes()).

prefix() ->
    "/api/" ++ ?DELIV_API_VERSION ++ "/e/".
