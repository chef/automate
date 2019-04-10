-module(auth_hand_oidc_configuration).
-behaviour(deliv_rest).

-export([
         allowed_methods/2,
         content_types_provided/2,
         init/3,
         to_json/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

%% https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderMetadata
to_json(Req, State) ->
    Json = {[
             {<<"issuer">>, auth_oidc_utils:issuer()},
             {<<"authorization_endpoint">>, auth_oidc_utils:authorization_endpoint()},
             {<<"token_endpoint">>, auth_oidc_utils:token_endpoint()},
             {<<"jwks_uri">>, auth_oidc_utils:jwks_uri()},
             {<<"response_types_supported">>, [<<"code">>]},
             {<<"grant_types_supported">>, [<<"authorization_code">>]},
             {<<"subject_types_supported">>, [<<"public">>]},
             {<<"id_token_signing_alg_values_supported">>, [<<"RS256">>]},
             {<<"scopes_supported">>, [<<"openid">>]},
             {<<"token_endpoint_auth_methods_supported">>, [<<"client_secret_post">>]},
             {<<"claims_supported">>, [<<"aud">>,
                                       <<"exp">>,
                                       <<"iat">>,
                                       <<"iss">>,
                                       <<"name">>,
                                       <<"email">>,
                                       <<"sub">>]}
            ]},
    deliv_web_utils:content(Json, Req, State).
