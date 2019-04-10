-module(delivery_sup_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

gather_routes_fixture_test_() ->
    hoax:fixture(?MODULE).

gather_routes_should_aggregate_routes_from_registered_routers() ->
    hoax:expect(receive
                  jobs_routes:routes() -> [route13, route14];
                  vis_routes:routes() -> [route15, route16];
                  auth_routes:routes() -> [route11, route12];
                  notification_routes:routes() -> [route9, route10];
                  scm_routes:routes() -> [route7, route8];
                  deliv_routes:routes() -> [route5, route6];
                  deliv_github_routes:routes() -> [route3, route4];
                  audit_routes:routes() -> [route1, route2]
              end),

    Result = delivery_sup:gather_routes(),

    ?assertEqual([route1, route2, route7, route8, route3, route4, route9,
                  route10, route11, route12, route13, route14, route15,
                  route16, route5, route6],
                 Result).
