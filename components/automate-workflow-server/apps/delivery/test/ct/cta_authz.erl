%% @doc Provides funs to assign roles through the HTTP API to use in CT
%% acceptance tests
-module(cta_authz).

-export([set_roles/3,
         grant_roles/3,
         revoke_roles/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("delivery/include/deliv_types.hrl").

%% @doc Expects `Config' to define the relevant keys for that `Scope' (see
%% `ct_authz:scope_to_keys/1' for more info)
%% Returns the json containing the new roles for the user
-spec set_roles(proplist(atom(), any()), deliv_scope(), [deliv_role()] | deliv_role() | none) -> json().
set_roles(Config, Scope, Roles) ->
    http_call(<<"set">>, Config, Scope, Roles).

%% @doc Same as `set_roles/3', instead just using the 'grant' action
-spec grant_roles(proplist(atom(), any()), deliv_scope(), [deliv_role()] | deliv_role() | none) -> json().
grant_roles(Config, Scope, Roles) ->
    http_call(<<"grant">>, Config, Scope, Roles).

%% @doc Same as `set_roles/3', instead just using the 'revoke' action
-spec revoke_roles(proplist(atom(), any()), deliv_scope(), [deliv_role()] | deliv_role() | none) -> json().
revoke_roles(Config, Scope, Roles) ->
    http_call(<<"revoke">>, Config, Scope, Roles).

%% @private
-spec http_call(binary(), binary(), deliv_scope(), [deliv_role()] | deliv_role() | none) -> json().
http_call(Action, Config, Scope, Roles) ->
    Route = build_route(Config, Scope),
    Body = {[{Action, ct_authz:translate_roles(Roles)}]},
    AdminAuthData = cta_http:admin_auth_data(),
    Response = http_test_helpers:auth_req(AdminAuthData, post,
                                          Route, Body),
    case Response of
        {200, _, RespBody} ->
            chef_json:decode(RespBody);
        Else ->
            ct:pal("Unexpected reply when trying to ~s roles ~p to ~s: ~p",
                   [Action, Roles, Route, Else]),
            ?assert(false)
    end.

%% @private
build_route(Config, Scope) ->
    EntName = cta_app:ent_name(),
    Config2 = [{ent_name, EntName} | Config],
    [EntName, UserName | OtherParams] = ct_authz:params_for_scope(Scope, Config2),

    RouteTemplate = route_template_for(Scope),
    cta_http:with_base_ent_route([io_lib:format(RouteTemplate, OtherParams),
                                  "/authz/users/",
                                  UserName]).

%% @private
route_template_for(enterprise) -> "";
route_template_for(organization) -> "/orgs/~s";
route_template_for(project) -> "/orgs/~s/projects/~s";
route_template_for(pipeline) -> "/orgs/~s/projects/~s/pipelines/~s".
