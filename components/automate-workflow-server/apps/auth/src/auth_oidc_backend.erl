%% based on github.com/kivra/oauth2_example (MIT-licensed)
-module(auth_oidc_backend).

-include_lib("delivery/include/deliv_types.hrl").

-behavior(oauth2_backend).

%%% API
-export([
         start/0,
         stop/0,
         add_client/4,
         delete_client/1,
         get_user/1,
         get_enterprise/1,
         delete_access_code/1 %% only exported to MFA it in apply_after
        ]).

%%% Behavior API
-export([authenticate_user/2,
         authenticate_client/2,
         associate_access_code/3,
         associate_access_token/3,
         associate_refresh_token/3,
         resolve_access_code/2,
         resolve_access_token/2,
         resolve_refresh_token/2,
         revoke_access_code/2,
         revoke_access_token/2,
         revoke_refresh_token/2,
         get_client_identity/2,
         get_redirection_uri/2,
         verify_redirection_uri/3,
         verify_client_scope/3,
         verify_resowner_scope/3,
         verify_scope/3
        ]).

-define(ACCESS_CODE_TABLE, access_codes).
-define(CLIENT_TABLE, clients).

-define(TABLES, [?ACCESS_CODE_TABLE,
                 ?CLIENT_TABLE]).

-record(client, {
          client_id     :: binary(),
          client_secret :: binary() | undefined,
          redirect_uri  :: binary(),
          scope         :: [binary()]
         }).

%%% API

start() ->
    lists:foreach(fun(Table) ->
                      ets:new(Table, [named_table, public])
                  end,
                  ?TABLES),
    add_clients_from_env().

stop() ->
    lists:foreach(fun ets:delete/1, ?TABLES).

-spec add_client(Id, Secret, RedirectURI, Scope) -> ok when
      Id          :: binary(),
      Secret      :: binary() | undefined,
      RedirectURI :: binary(),
      Scope       :: [binary()].
add_client(Id, Secret, RedirectURI, Scope) ->
    put(?CLIENT_TABLE, Id, #client{client_id = Id,
                                   client_secret = Secret,
                                   redirect_uri = RedirectURI,
                                   scope = Scope
                                  }),
    ok.

delete_client(Id) ->
    delete(?CLIENT_TABLE, Id).

-spec get_user(oauth2:auth()) -> d_user().
get_user({a, {client,_,_,_,_}, {_, User}, [_|_], _}) -> User.

-spec get_enterprise(oauth2:auth()) -> binary().
get_enterprise({a, {client,_,_,_,_}, {EntName, _}, [_|_], _}) -> EntName.


%%% OAuth2 backend functions

%% Before SAML, the user record is undefined; after, we pass a ent name and
%% deliv_user
authenticate_user(undefined, AppCtx) ->
    {ok, {AppCtx, undefined}};
authenticate_user({EntName, User}, AppCtx) ->
    case deliv_user:getval(user_type, User) of
        <<"saml">> -> {ok, {AppCtx, {EntName, User}}};
        _ ->
            chef_log:error("Unable to SAML authenticate user: ~p", [User]),
            {error, notfound}
    end.

authenticate_client({ClientId, ClientSecret}, AppCtx) ->
    case get(?CLIENT_TABLE, ClientId) of
        {ok, #client{client_secret = ClientSecret} = Identity} ->
            {ok, {AppCtx, Identity}};
        {ok, #client{client_secret = _WrongSecret}} ->
            {error, badsecret};
        _ ->
            {error, notfound}
    end.

associate_access_code(AccessCode, GrantCtx, AppCtx) ->
    put(?ACCESS_CODE_TABLE, AccessCode, GrantCtx),
    Expiry = timer:seconds(oauth2_config:expiry_time(code_grant)),
    timer:apply_after(Expiry, ?MODULE, delete_access_code, [AccessCode]),
    {ok, AppCtx}.

associate_refresh_token(_RefreshToken, _, _) ->
    {error, notfound}.

associate_access_token(_AccessToken, _, _) ->
    {error, notfound}.

resolve_access_code(AccessCode, AppCtx) ->
    case get(?ACCESS_CODE_TABLE, AccessCode) of
        {ok, Grant} ->
            {ok, {AppCtx, Grant}};
        Error = {error, notfound} ->
            Error
    end.

resolve_refresh_token(_RefreshToken, _) ->
    {error, notfound}.

resolve_access_token(_AccessToken, _) ->
    {error, notfound}.

revoke_access_code(AccessCode, AppCtx) ->
    delete(?ACCESS_CODE_TABLE, AccessCode),
    {ok, AppCtx}.

revoke_access_token(_AccessToken, _) ->
    {error, notfound}.

revoke_refresh_token(_RefreshToken, _) ->
    {error, notfound}.

get_redirection_uri(ClientId, _) ->
    case get(?CLIENT_TABLE, ClientId) of
        {ok, #client{redirect_uri = RedirectUri}} ->
            {ok, RedirectUri};
        Error = {error, notfound} ->
            Error
    end.

get_client_identity(ClientId, AppCtx) ->
    case get(?CLIENT_TABLE, ClientId) of
        {ok, Identity} ->
            {ok, {AppCtx, Identity}};
        {error, notfound} ->
            {error, notfound}
    end.

verify_redirection_uri(#client{redirect_uri = _RegisteredUri}, undefined, AppCtx) ->
    {ok, AppCtx};
verify_redirection_uri(#client{redirect_uri = _RegisteredUri}, <<>>, AppCtx) ->
    {ok, AppCtx};
verify_redirection_uri(#client{redirect_uri = <<>>}, _Uri, _AppCtx) ->
    {error, baduri};
verify_redirection_uri(#client{redirect_uri = RegisteredUri}, RegisteredUri, AppCtx) ->
    {ok, AppCtx};
verify_redirection_uri(#client{redirect_uri = _RegisteredUri}, _DifferentUri, _AppCtx) ->
    {error, baduri}.

verify_client_scope(_Client, Scope, AppCtx) ->
    verify_scope(oidc_scopes(), Scope, AppCtx).

verify_resowner_scope(_User, Scope, AppCtx) ->
    verify_scope(oidc_scopes(), Scope, AppCtx).

verify_scope(RegisteredScope, undefined, AppCtx) ->
    {ok, {AppCtx, RegisteredScope}};
verify_scope(_RegisteredScope, [], AppCtx) ->
    {ok, {AppCtx, []}};
verify_scope([], _Scope, _AppCtx) ->
    {error, invalid_scope};
verify_scope(RegisteredScope, Scope, AppCtx) ->
    case oauth2_priv_set:is_subset(oauth2_priv_set:new(Scope),
                                   oauth2_priv_set:new(RegisteredScope)) of
        true ->
            {ok, {AppCtx, Scope}};
        false ->
            {error, badscope}
    end.

%%% Internal functions

%% Scopes that may appear: allow more than just openid to not scare off (oidc)
%% clients -- currently not really making use of these
oidc_scopes() -> [<<"openid">>,
                  <<"profile">>,
                  <<"email">>,
                  <<"address">>,
                  <<"phone">>,
                  <<"offline_access">>].

get(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            {error, notfound};
        [{_Key, Value}] ->
            {ok, Value}
    end.

put(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}),
    ok.

delete(Table, Key) ->
    ets:delete(Table, Key).

delete_access_code(Code) ->
    delete(?ACCESS_CODE_TABLE, Code).

add_clients_from_env() ->
    add_clients(application:get_env(auth, oidc, []), false).

add_clients([Client|T], _) ->
    ClientId = chef_utils:to_bin(proplists:get_value(client_id, Client)),
    RedirectUri = chef_utils:to_bin(proplists:get_value(client_redirect_uri, Client)),
    chef_log:info("adding oidc client (id: ~s, redirect_uri: ~s)", [ClientId, RedirectUri]),
    add_client(ClientId,
               chef_utils:to_bin(proplists:get_value(client_secret, Client)),
               RedirectUri,
               oidc_scopes()),
    add_clients(T, true);
add_clients([], false) ->
    chef_log:warning("no oidc clients configured");
add_clients([], true) -> ok.
