%% @doc Pretty much the same as the `db_test_helpers' module,
%% except all with API calls
-module(cta_db).

-export([new_intern_user/0,
         new_intern_user/1,
         new_org/0]).

%% @doc Proxy for `new_intern_user/1', handy if you don't care about the password
-spec new_intern_user() -> {binary(), binary()}.
new_intern_user() ->
    new_intern_user(<<"password">>).

%% @doc Creates a new intern user, and returns their usename and an API token
-spec new_intern_user(binary()) -> {binary(), binary()}.
new_intern_user(Password) ->
    %% not fool-proof, but good enough for now
    UserName = chef_utils:random_string(20),
    AdminAuthData = cta_http:admin_auth_data(),
    CreateRoute = cta_http:with_base_ent_route("/internal-users"),
    CreateBody = {[{<<"name">>, UserName}]},
    {201, _, _} = http_test_helpers:auth_req(AdminAuthData, post, CreateRoute, CreateBody),
    %% then reset the password
    ResetRoute = CreateRoute ++ "/"
                   ++ chef_utils:to_str(UserName)
                   ++ "/change-password",
    ResetBody = {[{<<"password">>, Password}]},
    {204, _, _} = http_test_helpers:auth_req(AdminAuthData, post, ResetRoute, ResetBody),
    %% and finally get a token
    Token = cta_http:token(UserName, Password),
    {UserName, Token}.

%% @doc Creates a new organization, and returns its name
-spec new_org() -> binary().
new_org() ->
    OrgName = chef_utils:random_string(20),
    AdminAuthData = cta_http:admin_auth_data(),
    Route = cta_http:with_base_ent_route("/orgs"),
    Body = {[{<<"name">>, OrgName}]},
    {201, _, _} = http_test_helpers:auth_req(AdminAuthData, post, Route, Body),
    OrgName.
