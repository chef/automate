-module(ct_authz).

%% @doc Provides a few misc helper methods to write tests

-include_lib("common_test/include/ct.hrl").

-include_lib("delivery/include/deliv_types.hrl").

-export([
         scope_to_keys/1,
         params_for_scope/2,
         assign_roles/3,
         translate_roles/1
         ]).


%% @doc Map a scope to the corresponding CT `Config' object key.
scope_to_key(enterprise)   -> ent_name;
scope_to_key(organization) -> org_name;
scope_to_key(project)      -> proj_name;
scope_to_key(pipeline)     -> pipe_name.

%% @doc Given a scope, return a list of all CT `Config' object keys
%% needed to manipulate roles at that scope.
-spec scope_to_keys(deliv_scope()) -> [atom()].
scope_to_keys(enterprise) -> [ent_name, user_name];
scope_to_keys(Scope)      -> scope_to_keys(previous_scope(Scope)) ++ [scope_to_key(Scope)].

%% @doc Given a scope, return the scope immediately above it in the
%% hierarchy.
previous_scope(enterprise)   -> 'NONE';
previous_scope(organization) -> enterprise;
previous_scope(project)      -> organization;
previous_scope(pipeline)     -> project.

%% @doc Returns the params from the CT `Config' for the given `Scope'
-spec params_for_scope(deliv_scope(), proplist(atom(), any())) -> [binary()].
params_for_scope(Scope, Config) ->
     [ct_utils:get_config(Key, Config) || Key <- scope_to_keys(Scope)].

%% @doc Set the given `Roles' at `Scope' to the test user specified
%% in `Config'.
%% You can pass it `none`, which is equivalent to an empty list; that's
%% mainly syntactic sugar to be able to treat no role the same as an
%% actual role if you have a test fun that sets a role and then runs some test
-spec assign_roles(deliv_scope(), [deliv_role()] | deliv_role() | none, proplist(atom(), any()))
        -> json() | {error, _Why}.
assign_roles(Scope, Roles, Config) ->
    Params = params_for_scope(Scope, Config),
    Results = deliv_user:edit_roles(set, Scope, Params ++ [translate_roles(Roles)]),
    ct:pal("Assigned roles at scope ~p: ~p", [Scope, Results]),
    Results.

translate_roles(none) -> [];
translate_roles(Role) when erlang:is_binary(Role) -> [Role];
translate_roles(Roles) when erlang:is_list(Roles) -> Roles.
