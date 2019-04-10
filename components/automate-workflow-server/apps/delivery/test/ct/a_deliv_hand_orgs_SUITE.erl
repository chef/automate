%% @doc Acceptance-level CT tests on the orgs endpoint
-module(a_deliv_hand_orgs_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("delivery/include/deliv_types.hrl").

groups() -> [{as_admin,
              [],
              [list_orgs_empty,
               create_org,
               list_orgs,
               create_conflict]},
             {authz,
              [],
              [authz_get,
               authz_post]}].
all() -> [{group, as_admin},
          {group, authz}].

init_per_suite(Config) ->
    cta_app:start(),

    {UserName, _Token} = AuthData = cta_db:new_intern_user(),
    Route = cta_http:with_base_ent_route("/orgs"),
    OrgName = chef_utils:random_string(20),

    [{user_name, UserName},
     {auth_data, AuthData},
     {route, Route},
     {org_name, OrgName}
     | Config].

init_per_group(as_admin, Config) ->
    cta_authz:set_roles(Config, enterprise, [<<"admin">>]),
    Config;
init_per_group(authz, Config) ->
    cta_authz:set_roles(Config, enterprise, []),
    Config.

end_per_group(_, Config) ->
    Config.

list_orgs_empty(Config) ->
    assert_get_success(Config, false, true).

create_org(Config) ->
    EntName = cta_app:ent_name(),
    OrgName = ?config(org_name, Config),

    Response = http_create(Config),

    FullLink = <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary>>,
    ProjectLink = <<FullLink/binary, "/projects">>,
    ExpectedJson = {[{ <<"_links">>,
        {[{<<"full">>, {[{<<"href">>, FullLink}]}}, {<<"create-project">>, {[{<<"href">>, ProjectLink}]}}]}
    }]},
    http_test_helpers:test_json_response({201, {exact, ExpectedJson}}, Response).

list_orgs(Config) ->
    assert_get_success(Config, true, true).

create_conflict(Config) ->
    Response = http_create(Config),
    ConflictJson = {[{<<"error">>, <<"conflict">>}]},
    http_test_helpers:test_json_response({409, {exact, ConflictJson}}, Response).

authz_get(Config) ->
    %% any role should be able to list orgs (and no role too)
    TestListOrgsAs = fun (Role) ->
        cta_authz:set_roles(Config, enterprise, Role),
        assert_get_success(Config, true, Role =:= <<"admin">>)
    end,
    [TestListOrgsAs(Role) || Role <- [none | ?ALL_DELIV_ROLES]].

authz_post(Config) ->
    %% no one but admins can create orgs
    AllOtherRoles = [<<"committer">>, <<"reviewer">>, <<"shipper">>, <<"observer">>],
    cta_authz:set_roles(Config, enterprise, AllOtherRoles),
    ForbiddenResponse = http_create(Config),
    http_test_helpers:test_empty_response(403, ForbiddenResponse).

%%% Helper funs

assert_get_success(Config, WasOrgCreated, AsAdmin) ->
    Response = http_list(Config),

    ExpectedJson = build_expected_list_json(Config, WasOrgCreated, AsAdmin),
    http_test_helpers:test_json_response({200, {exact, ExpectedJson}}, Response).

build_expected_list_json(Config, WasOrgCreated, AsAdmin) ->
    OrgsJson = case WasOrgCreated of
        true ->
            OrgName = ?config(org_name, Config),
            [{[{<<"name">>, OrgName},
               {<<"project_count">>, 0}]}];
        false ->
            []
    end,

    EntName = cta_app:ent_name(),
    BaseLink = <<"/api/v0/e/", EntName/binary, "/orgs">>,

    CreateLink = {<<"create_org">>, {[ {<<"href">>, BaseLink} ]}},
    ShowLink = {<<"show_org">>, {[
                   {<<"href">>, <<BaseLink/binary, "/{org_name}">>},
                   {<<"templated">>, true}
                ]}},

    LinksJson = case {WasOrgCreated, AsAdmin} of
        {true, true} -> [CreateLink, ShowLink];
        {false, true} -> [CreateLink];
        {true, false} -> [ShowLink];
        {false, false} -> []
    end,

    {[
        {<<"orgs">>, OrgsJson},
        {<<"_links">>, {LinksJson}}
    ]}.

http_create(Config) ->
    OrgName = ?config(org_name, Config),
    Body = {[{<<"name">>, OrgName}]},
    make_http_call(Config, post, Body).

http_list(Config) ->
    make_http_call(Config, get, <<>>).

make_http_call(Config, Method, Body) ->
    AuthData = ?config(auth_data, Config),
    Route = ?config(route, Config),

    http_test_helpers:auth_req(AuthData, Method, Route, Body).
