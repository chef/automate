-module(cta_http).

-export([base_ent_route/0,
         with_base_ent_route/1,
         token/2,
         admin_token/0,
         auth_data/2,
         admin_auth_data/0]).

%% @doc Base route for all endpoints
-spec base_ent_route() -> string().
base_ent_route() ->
    http_test_helpers:base_ent_route(cta_app:ent_name()).

%% @doc Prepends the base route, and stringifies
-spec with_base_ent_route(iodata()) -> string().
with_base_ent_route(Path) ->
    chef_utils:iodata_to_str([base_ent_route(), Path]).

%% @doc Gets a token for `UserName' with `Password'
-spec token(binary(), binary()) -> binary().
token(UserName, Password) ->
    Route = base_ent_route()
              ++ "/users/"
              ++ chef_utils:to_str(UserName)
              ++ "/get-token",
    Body = {[{<<"username">>, UserName},
             {<<"password">>, Password}]},
    {200, _, RawJson} = http_test_helpers:req(post, Route, Body),
    Json = chef_json:decode(RawJson),
    ej:get([<<"token">>], Json).

%% @doc Gets a token for the 'admin' user
-spec admin_token() -> binary().
admin_token() ->
    token(<<"admin">>, <<"admin">>).

%% @doc Returns auth data as expected by `http_test_helpers:auth_req/*' funs
-spec auth_data(binary(), binary()) -> {binary(), binary()}.
auth_data(UserName, Password) ->
    {UserName, token(UserName, Password)}.

%% @doc Same as `auth_data/2', but for the admin user
-spec admin_auth_data() -> {binary(), binary()}.
admin_auth_data() ->
    {<<"admin">>, admin_token()}.
