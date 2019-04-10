-module(scm_routes_tests).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

routes_should_return_bitbucket_route_list_test() ->
    ok = application:set_env(delivery, read_ttl_secs, 3600),
    ok = application:set_env(delivery, write_ttl_secs, 3600),

    State = deliv_routes:set_ttls(#handler{}),

    ProjectHandlerRoute = {
        deliv_web_utils:route_prefix() ++ ":ent_name/orgs/[:org_name]/bitbucket-projects",
        scm_hand_projects,
        State#handler{authz = {organization, [
                                {deliv_web_utils:bin_method(post),[<<"admin">>]}]}}},

    GHProjectHandlerRoute = {
        deliv_web_utils:route_prefix() ++ ":ent_name/orgs/[:org_name]/github-projects",
        scm_hand_projects,
        State#handler{authz = {organization, [
                                {deliv_web_utils:bin_method(post),[<<"admin">>]}]}}},

     ServerHandlerRoute = {
        deliv_web_utils:route_prefix() ++ ":ent_name/scm/:scm_type/servers",
        scm_hand_servers,
        State#handler{authz = {enterprise, [
                                 {deliv_web_utils:bin_method(post), [<<"admin">>]},
                                 {deliv_web_utils:bin_method(get), [<<"admin">>]}
                               ]}}
    },

    NamedServerHandlerRoute = {
        deliv_web_utils:route_prefix() ++ ":ent_name/scm/:scm_type/servers/:server_url",
        scm_hand_servers_named,
        State#handler{authz = {enterprise, [
                                {deliv_web_utils:bin_method(put), [<<"admin">>]},
                                {deliv_web_utils:bin_method(delete), [<<"admin">>]}
                              ]}}
    },

    ExpectedRoutes = [ProjectHandlerRoute,
                      GHProjectHandlerRoute,
                      ServerHandlerRoute,
                      NamedServerHandlerRoute],

    ?assertEqual(ExpectedRoutes, scm_routes:routes()).
