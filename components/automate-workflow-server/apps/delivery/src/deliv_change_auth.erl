%% Functions to determine if a user is authorized to perform an action on
%% a change.

-module(deliv_change_auth).

-include("deliv_types.hrl").

-export([
         authorized_change_action/6
        ]).

%% @doc determines if the given user can take the given action on a change.
-spec authorized_change_action(binary(), binary(), binary(), binary(), binary(),
                               change_action()) -> allow | forbid.
authorized_change_action(EntName, OrgName, ProjName, Pipeline, UserName, Action) ->
    {Scope, AuthdRoles} = scope_and_roles_for_action(Action),

    Params = case Scope of
                 pipeline -> [EntName, UserName, OrgName, ProjName, Pipeline];
                 enterprise -> [EntName, UserName]
             end,

    case deliv_user:effective_roles(Scope, Params) of
        {ok, Roles} ->
            deliv_authz:roles_match(Roles, AuthdRoles);
        {error, _Why} ->
            forbid
    end.

%% @private
%% @doc returns the scope and roles requiured for the given action on
%% this change. This is ugly because we are in a change centric world
%% right now the actions on union, rehearsal, delivered are from the
%% context of change.
-spec scope_and_roles_for_action(change_action()) -> {deliv_scope(),
                                                      [deliv_role()]}.
scope_and_roles_for_action(trigger_verify) ->
    {pipeline, [<<"admin">>, <<"reviewer">>, <<"committer">>]};
scope_and_roles_for_action(delete) ->
    {pipeline, [<<"admin">>, <<"reviewer">>, <<"committer">>]};
scope_and_roles_for_action(approve) ->
    {pipeline, [<<"admin">>, <<"reviewer">>]};
scope_and_roles_for_action(trigger_build) ->
    {pipeline, [<<"admin">>, <<"reviewer">>]};
scope_and_roles_for_action(trigger_acceptance) ->
    {pipeline, [<<"admin">>, <<"reviewer">>]};
scope_and_roles_for_action(deliver) ->
    {pipeline, [<<"admin">>, <<"shipper">>]};
scope_and_roles_for_action(trigger_union) ->
    {enterprise, [<<"admin">>, <<"shipper">>]};
scope_and_roles_for_action(trigger_rehearsal) ->
    {enterprise, [<<"admin">>, <<"shipper">>]};
scope_and_roles_for_action(trigger_delivered) ->
    {enterprise, [<<"admin">>, <<"shipper">>]}.
