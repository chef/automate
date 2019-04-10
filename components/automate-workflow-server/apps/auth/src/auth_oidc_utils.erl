-module(auth_oidc_utils).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("jose/include/jose_jwk.hrl").
-include_lib("jose/include/jose_jwt.hrl").

-export([
         access_token_response/1,
         access_token_response/3,
         authorization_endpoint/0,
         auth_request_vals/1,
         extract_client_state/1,
         generate_jwt/3,
         generate_jwt/5,
         issuer/0,
         jwks_uri/0,
         private_signing_key/0,
         make_token/2,
         redirect_code_response/5,
         redirect_error_response/5,
         redirect_error_response/6,
         redirect_code_request/5,
         token_endpoint/0,
         verify_client_redirect_uri/2
        ]).

-type jwt_error() :: {error, no_key}.
-type jwt() :: binary().
-type jwt_resp() :: {ok, jwt()} | jwt_error().

-spec access_token_response(binary(), d_user(), binary()) -> {ok, json()} | jwt_error().
access_token_response(EntName, User, ClientId) ->
    access_token_response(generate_jwt(EntName, User, ClientId)).

-spec access_token_response(jwt_resp()) -> {ok, json()} | jwt_error().
access_token_response({ok, Token}) ->
    {ok, {[
           {<<"access_token">>, Token}, %% openid_connect rubygem needs this
           {<<"id_token">>, Token},     %% go-oidc needs this
           {<<"token_type">>, <<"Bearer">>},
           {<<"expires_in">>, oauth2_config:expiry_time()}
          ]}};
access_token_response(Err) -> Err.

-spec auth_request_vals(cowboy_req()) -> {ok, cowboy_req(), list({binary(), binary()})} | {error, term()}.
auth_request_vals(Req) ->
    Vals = [<<"response_type">>, <<"client_id">>, <<"redirect_uri">>, <<"scope">>, <<"state">>],
    deliv_web_utils:process_qs_vals(Req, fun(_, V) -> V end, Vals).

%% for testing: cannot mock os:timestamp(), so this function can be fed timestamps
-spec generate_jwt(binary(),
                   d_user(),
                   binary(),
                   non_neg_integer() | undefined,
                   non_neg_integer() | undefined) -> {ok, jwt()} | jwt_error().
generate_jwt(EntName, User, ClientId, ExpiresAt, IssuedAt) ->
    Claims = #{
      sub => deliv_user:getval(name, User),
      enterprise => EntName,
      email => deliv_user:getval(email, User),
      first => deliv_user:getval(first_name, User),
      last => deliv_user:getval(last_name, User),
      iss => undefined,
      aud => ClientId,
      iat => IssuedAt,
      exp => ExpiresAt},
    make_token(private_signing_key(), Claims).

%% this one is used in access_token_response: uses proper timestamps (in
%% make_token/1)
-spec generate_jwt(binary(), d_user(), binary()) -> {ok, jwt()} | jwt_error().
generate_jwt(EntName, User, ClientId) ->
    generate_jwt(EntName, User, ClientId, undefined, undefined).

-spec make_token({ok, #jose_jwk{}} | jwt_error(),
                 #{atom() => atom() | binary() | non_neg_integer()}) -> {ok, jwt()} | jwt_error().
make_token({error, no_key}, _) ->
    {error, no_key};
make_token(Key, #{iat := undefined} = Token) ->
    make_token(Key, Token#{iat := expiry_from_now(0)});
make_token(Key, #{iss := undefined} = Token) ->
    make_token(Key, Token#{iss := issuer()});
make_token(Key, #{exp := undefined} = Token) ->
    make_token(Key, Token#{exp := expiry_from_now(oauth2_config:expiry_time())});
make_token({ok, PrivateKey}, Claims) ->
    JWS = #{<<"alg">> => <<"RS256">>},
    Signed = jose_jwt:sign(PrivateKey, JWS, #jose_jwt{fields = Claims}),
    {_, Token} = jose_jws:compact(Signed), %% TODO: what is first return?
    {ok, Token}.

-spec redirect_error_response(binary(), atom() | binary(), binary(), cowboy_req(), req_handler()) -> {ok, cowboy_req(), req_handler()}.
redirect_error_response(RedirectUri, Error, ClientState, Req, State)
  when is_atom(Error)->
    redirect_error_response(RedirectUri, chef_utils:to_bin(Error), ClientState, Req, State);
redirect_error_response(RedirectUri, Error, ClientState, Req, State) ->
    Location = <<RedirectUri/binary,
                 "?error=", Error/binary,
                 "&state=", (deliv_web_utils:encode_url(ClientState))/binary>>,
    deliv_web_utils:redirect_302(Location, Req, State).

%% Redirects to callback with `?error=` parameter, but first validates client
%% redirect URI
-spec redirect_error_response(binary(), binary(), atom() | binary(), binary(), cowboy_req(), req_handler()) -> {ok, cowboy_req(), req_handler()}.
redirect_error_response(ClientId, RedirectUri, Error, ClientState, Req, State) ->
    case verify_client_redirect_uri(ClientId, RedirectUri) of
        ok -> redirect_error_response(RedirectUri, Error, ClientState, Req, State);
        error ->
            {halt, Req2, State2} = deliv_web_utils:error_response(403, forbidden, <<"Redirect URI mismatch">>, Req, State),
            {ok, Req2, State2}
    end.

%% Note that a code response only happens following a successful client
%% validation, so special redirect URI checking is not neccessary here.
-spec redirect_code_response(binary(), binary(), binary(), cowboy_req(), req_handler()) -> {ok, cowboy_req(), req_handler()}.
redirect_code_response(RedirectUri, Code, ClientState, Req, State) ->
    Location = <<RedirectUri/binary,
                 "?code=", Code/binary,
                 "&state=", (deliv_web_utils:encode_url(ClientState))/binary>>,
    deliv_web_utils:redirect_302(Location, Req, State).

-spec expiry_from_now(non_neg_integer()) -> non_neg_integer().
expiry_from_now(Ttl) when is_number(Ttl) andalso Ttl >= 0 ->
    {A, B, _} = os:timestamp(),
    (A * 1000000) + B + Ttl.

-spec private_signing_key() -> {ok, #jose_jwk{}} | {error, no_key}.
private_signing_key() ->
    case application:get_env(auth, oidc_signing_key_file, undefined) of
        KeyPath when is_binary(KeyPath) orelse is_list(KeyPath) ->
            wrap_result(jose_jwk:from_pem_file(KeyPath));
        _ -> {error, no_key}
    end.

-spec wrap_result(term()) -> {error, no_key} | {ok, #jose_jwk{}}.
wrap_result(#jose_jwk{} = Key) -> {ok, Key};
wrap_result(_) -> {error, no_key}.

-spec redirect_code_request(proplists:proplist(), binary(), d_user(), cowboy_req(), req_handler()) ->
    {ok, cowboy_req(), req_handler()}.
redirect_code_request(Relay, EntName, User, Req, State) ->
    ClientState = extract_client_state(Relay),
    ClientId    = proplists:get_value(oidc_client_id, Relay),
    RedirectUri = proplists:get_value(oidc_redirect, Relay),
    Scopes      = proplists:get_value(oidc_scopes, Relay),
    case oauth2:authorize_code_request({EntName, User},
                                       ClientId,
                                       RedirectUri,
                                       Scopes,
                                       State) of
        {ok, {AppCtx, Auth}} ->
            {ok, {_AppCtx2, Resp}} = oauth2:issue_code(Auth, AppCtx),
            {ok, Code} = oauth2_response:access_code(Resp),
            redirect_code_response(RedirectUri, Code, ClientState, Req, State);
        {error, Reason} ->
            redirect_error_response(ClientId, RedirectUri, Reason, ClientState, Req, State)
    end.

-spec extract_client_state(proplists:proplist()) -> binary().
extract_client_state(Relay) ->
    case proplists:get_value(client_state, Relay) of
        Bin when is_binary(Bin) -> Bin;
        _ -> <<>>
    end.

-spec verify_client_redirect_uri(binary(), binary()) -> ok | error.
verify_client_redirect_uri(ClientId, Uri) ->
    case auth_oidc_backend:get_redirection_uri(ClientId, app_ctx) of
        {ok, Uri} -> ok;
        _ -> error
    end.
%% @doc The issuer URL must match in auth_hand_oidc_configuration and `iss`
%% claim in make_token/7
-spec issuer() -> binary().
issuer() -> deliv_web_utils:api_base().

-spec authorization_endpoint() -> binary().
authorization_endpoint() ->
    <<(issuer())/binary, "/oidc/auth">>.

-spec token_endpoint() -> binary().
token_endpoint() ->
    <<(issuer())/binary, "/oidc/token">>.

-spec jwks_uri() -> binary().
jwks_uri() ->
    <<(issuer())/binary, "/oidc/jwks">>.
