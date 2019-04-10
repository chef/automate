-module(auth_routes_tests).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

routes_should_return_auth_route_list_test() ->
    ok = application:set_env(delivery, read_ttl_secs, 3600),
    ok = application:set_env(delivery, write_ttl_secs, 3600),
    State = deliv_routes:set_ttls(#handler{}),

    OidcConfigRoute = {
      prefix() ++ "/.well-known/openid-configuration",
      auth_hand_oidc_configuration, []},

    OidcJwksRoute = { prefix() ++ "/oidc/jwks", auth_hand_oidc_jwks, []},

    OidcTokenRoute = { prefix() ++ "/oidc/token", auth_hand_oidc_token, []},

    OidcAuthRoute = { prefix() ++ "/oidc/auth", auth_hand_oidc_auth, []},

    SamlAuthRoute = {
      prefix() ++ "/e/:ent_name/saml/auth/[:user_name]", auth_hand_saml_auth,
      State},

    SamlConsumeRoute = {
      prefix() ++ "/e/:ent_name/saml/consume", auth_hand_saml_consume,
      State},

    SamlMetadataRoute = {
      prefix() ++ "/e/:ent_name/saml/metadata", auth_hand_saml_metadata,
      State},

    SamlConfigRoute = {
      prefix() ++ "/e/:ent_name/saml/config", auth_hand_saml_config,
      State#handler{authz = {enterprise, [
                                          {deliv_web_utils:bin_method(put), [<<"admin">>]},
                                          {deliv_web_utils:bin_method(delete), [<<"admin">>]},
                                          {deliv_web_utils:bin_method(get), [<<"admin">>]}
                                         ]}}},

     SamlEnabledRoute = {
       prefix() ++ "/e/:ent_name/saml/enabled", auth_hand_saml_enabled,
       State},

      ExpectedRoutes = [
                        OidcConfigRoute,
                        OidcJwksRoute,
                        OidcAuthRoute,
                        OidcTokenRoute,
                        SamlAuthRoute,
                        SamlConsumeRoute,
                        SamlMetadataRoute,
                        SamlConfigRoute,
                        SamlEnabledRoute
                       ],

      ?assertEqual(ExpectedRoutes, auth_routes:routes()).

prefix() ->
    "/api/" ++ ?DELIV_API_VERSION.
