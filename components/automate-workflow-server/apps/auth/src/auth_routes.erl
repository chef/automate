-module(auth_routes).

-include_lib("delivery/include/deliv_types.hrl").

-export([
         routes/0
        ]).


routes() ->
    AuthzRules = deliv_authz:authz_rules(),
    State = deliv_routes:set_ttls(#handler{}),
    [
     {prefix() ++ "/.well-known/openid-configuration",
      auth_hand_oidc_configuration, []},

     {prefix() ++ "/oidc/jwks", auth_hand_oidc_jwks, []},

     {prefix() ++ "/oidc/auth", auth_hand_oidc_auth, []},

     {prefix() ++ "/oidc/token", auth_hand_oidc_token, []},

     {deliv_web_utils:route_prefix() ++ ":ent_name/saml/auth/[:user_name]",
         auth_hand_saml_auth, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name/saml/consume",
         auth_hand_saml_consume, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name/saml/metadata",
         auth_hand_saml_metadata, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name/saml/config" ,
         auth_hand_saml_config, set_authz(auth_hand_saml_config,
                                           State,
                                           AuthzRules)},

     {deliv_web_utils:route_prefix()  ++ ":ent_name/saml/enabled", auth_hand_saml_enabled,
      State}
    ].

prefix() ->
    "/api/" ++ ?DELIV_API_VERSION.

set_authz(Module, State, Rules) ->
    Authz = deliv_authz:authz_rules_for(Module, Rules),
    State#handler{authz=Authz}.
