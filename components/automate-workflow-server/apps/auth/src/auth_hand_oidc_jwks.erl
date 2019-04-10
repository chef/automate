-module(auth_hand_oidc_jwks).
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

%% Note: for key rotation, both the `kid` key in the keys objects and the
%% cache-control header need adjustments
%% JWK: https://tools.ietf.org/html/rfc7517
to_json(Req, State) ->
    %% max-age: clients need to refresh after this many seconds (since we
    %% currently don't rotate keys, 24hrs should be ok.
    Req2 = cowboy_req:set_resp_header(<<"Cache-Control">>, <<"public, max-age=86400">>, Req),
    deliv_web_utils:content(keys_to_jwks(), Req2, State).

keys_to_jwks() ->
    case auth_oidc_utils:private_signing_key() of
        {ok, PrivKey} ->
            {_, KeyMap} = jose_jwk:to_public_map(PrivKey),
            JWK = KeyMap#{<<"kid">> => <<"1">>,
                          <<"alg">> => <<"RS256">>,
                          <<"use">> => <<"sig">>},
            {[{<<"keys">>, [{maps:to_list(JWK)}]}]};
        {error, no_key} ->
            {[{<<"keys">>, []}]}
    end.
