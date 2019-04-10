%% @doc There are several common values that are passed frequently between modules
%% and funs. This method contains a record that encapsulates those values. Instead
%% of having arities greater than five or six you can simply pass this record.
-module(deliv_scopes).

-include("deliv_types.hrl").
-include("deliv_coordinates.hrl").

-compile({parse_transform, exprecs}).

-export([
         from_scoping_names/3,
         from_change/1,
         from_change_id/1,
         from_patchset/1,
         scope_to_coordinates/1,
         from_coords/1
        ]).

%% The "common" record.
-record(common, {scm_module     :: atom(),
                 change_id      :: db_guid(),
                 scoping_names  :: [binary() | undefined],
                 ent_name       :: binary(),
                 org_name       :: binary(),
                 proj_name      :: binary(),
                 proj_id        :: db_id(),
                 pipe_name      :: binary()
                }).

%% Configure exprecs to export helper funs.
%% @see parse_trans documentation.
-exprecs_prefix(["#", operation]).
-exprecs_fname([prefix, "_", record]).

%% Export the records
-export_records([common]).

%% @doc Create a record using just the base triple (Ent, Org, Proj names).
-spec from_scoping_names(binary(), binary(), binary()) -> d_common_scope().
from_scoping_names(Ent, Org, Proj) ->
    {ok, Project} = deliv_project:fetch(Ent, Org, Proj),
    '#new_common'([
                   {scm_module, chef_utils:to_atom(deliv_project:getval(scm_module, Project))},
                   {scoping_names, [Ent, Org, Proj, undefined]},
                   {ent_name, Ent},
                   {org_name, Org},
                   {proj_name, Proj},
                   {proj_id, deliv_project:getval(id, Project)},
                   {pipe_name, undefined},
                   {change_id, undefined}
                  ]).

%% @doc Create a record using a Change ID.
-spec from_change_id(binary()) -> d_common_scope().
from_change_id(ChangeId) ->
    ScopingNames = [EntName, OrgName, ProjName, PipeName] = deliv_change:scoping_names(ChangeId),
    {ok, Project} = deliv_project:fetch(EntName, OrgName, ProjName),
    '#new_common'([
                   {scm_module, chef_utils:to_atom(deliv_project:getval(scm_module, Project))},
                   {scoping_names, ScopingNames},
                   {ent_name, EntName},
                   {org_name, OrgName},
                   {proj_name, ProjName},
                   {proj_id, deliv_project:getval(id, Project)},
                   {pipe_name, PipeName},
                   {change_id, ChangeId}
                  ]).

%% @doc Create a record using a Change record.
-spec from_change(d_change()) -> d_common_scope().
from_change(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    from_change_id(ChangeId).

%% @doc Create a record using a Patchset record.
-spec from_patchset(d_patchset()) -> d_patchset().
from_patchset(Patchset) ->
    ChangeId = deliv_patchset:getval(change_id, Patchset),
    from_change_id(ChangeId).


scope_to_coordinates(CommonScope) ->
    [EntName, OrgName, ProjName, _] = deliv_scopes:'#get'(scoping_names, CommonScope),
    #proj_coordinates{ent_name = EntName,
                      org_name = OrgName,
                      proj_name = ProjName}.

-spec from_coords(#proj_coordinates{}) -> d_common_scope().
from_coords(#proj_coordinates{ent_name=EntName, org_name=OrgName, proj_name=ProjName}) ->
    from_scoping_names(EntName, OrgName, ProjName).