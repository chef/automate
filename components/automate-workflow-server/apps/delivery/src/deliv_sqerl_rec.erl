%% @doc All our sqerl records should implement this behaviour, on top of sqerl_rec
%% Also includes some helper funs to generate some common queries
-module(deliv_sqerl_rec).

-include("deliv_types.hrl").

-export([
    gen_scoped_sqerl_statements/1,
    gen_fetch_names_by_scoping_names/1,
    gen_fetch_scoping_params_from_id/1,
    fetch_scoping_params_from_id/2
]).

%% @doc Our objects belong to a tree, where each object
%% is scoped to exactly one parent; the root of the tree
%% are enterprises
%% This callback should return a tuple containing the name
%% of its scoping parent's module and the name of the foreign
%% key's column
%% For enterprises only, this callback should return `none'
%% This callback is used to generate scoped sqerl statements
%% (see `gen_scoped_sqerl_statements/1' below)
%% and also to get the enterprise_id for objects deeper than 1 level
%% into the scoping tree
-callback scoping_parent() -> {atom(), atom()} | none.

%% @doc This should return the name of the column that uniquely defines
%% this object, given a scoping context (i.e. there should be a (possibly
%% partial, see `scoping_index_constraint/0' below) index on the couple
%% `(<scoping_column>, <this_column>)' in the DB
-callback scoping_column_name() -> atom().

%% @doc If the scoping index is partial, this should return the SQL expression
%% that defines that partial index; should just return `none'
%% if the index is not partial
%% Of course, this SQL expression should be totally unambiguous, so
%% please always prefix comlumn names with the full names of the tables they belong to!
-callback scoping_index_constraint() -> string() | none.

%% @doc Generates the common queries to insert, delete and fecth by ent name
-spec gen_scoped_sqerl_statements(atom()) -> proplist(atom(), [string()]).
gen_scoped_sqerl_statements(Module) ->
    [
        {insert_by_scoping_params, gen_insert_by_scoping_params(Module)},
        {delete_by_scoping_params, gen_del_by_scoping_params(Module)},
        {fetch_by_scoping_params, gen_fetch_by_scoping_params(Module)}
    ].

%% @private
%% @doc Generates the query to insert by scoping names
%% Uses `scoping_parent/0' for that
%% The only assumption made here is that _all_ such objects
%% define the 'id' field
%% Of course all the scoped statements generated below
%% expect their query parameters to be in a very specific order,
%% but that should be taken care of by
%% `deliv_db:*_unique_by_scoping_names/3' functions
%%
%% for example for organizations
%% "INSERT INTO organizations (enterprise_id, name) "
%% "SELECT enterprises.id, $2 FROM enterprises"
%% "WHERE enterprises.name = $1 RETURNING id, enterprise_id, name"
%%
%% or for pipelines
%% "INSERT INTO pipelines (project_id, name) "
%% "SELECT projects.id, $4 FROM projects "
%% "JOIN organizations "
%% "ON projects.organization_id = organizations.id "
%% "JOIN enterprises "
%% "ON organizations.enterprise_id = enterprises.id "
%% "WHERE enterprises.name = $1 "
%% "AND organizations.name = $2 "
%% "AND projects.name = $3 "
%% "RETURNING id, project_id, name"
-spec gen_insert_by_scoping_params(atom()) -> [string()].
gen_insert_by_scoping_params(Module) ->
    Genealogy = scoping_genealogy(Module),
    [{ImmediateParentModule, ImmediateParentIdField} | _Ancestors] = Genealogy,
    ImmediateParentName = ImmediateParentModule:'#table_name'(),
    InsertFields = [ atom_to_list(F) || F <- Module:'#insert_fields'() ],
    InsertFieldsSql = string:join(InsertFields, ", "),
    {RevParams, _} = lists:foldl(
        fun(ParentIdField, {Acc, CurrentIdx}) when ParentIdField =:= ImmediateParentIdField ->
            {[ImmediateParentName ++ ".id" | Acc], CurrentIdx};
           (_OtherField, {Acc, CurrentIdx}) ->
            {["$" ++ chef_utils:to_str(CurrentIdx) | Acc], CurrentIdx + 1}
        end,
        {[], length(Genealogy) + 1},
        Module:'#insert_fields'()
    ),
    Params = string:join(lists:reverse(RevParams), ", "),
    ReturnFields = [atom_to_list(F) || F <- Module:fields()],
    ReturnFieldsSql = string:join(ReturnFields, ", "),
    ["INSERT INTO ", Module:'#table_name'(),
     " (", InsertFieldsSql, ") ",
     "SELECT ", Params,
     " FROM ", ImmediateParentName, " ",
     gen_scoping_joins_and_where_clauses(Genealogy),
     " RETURNING ", ReturnFieldsSql].



%% @private
%% @doc Generates the query to delete by scoping names
%% Makes the same assumptions as `gen_insert_by_scoping_params/1' above
%%
%% Examples: for organizations:
%% "DELETE FROM organizations"
%% "USING enterprises"
%% "WHERE enterprises.name = $1 "
%% "  AND organizations.name = $2 "
%% "  AND organizations.enterprise_id = enterprises.id"
%%
%% or for pipelines:
%% "DELETE FROM pipelines "
%% "USING projects "
%% "JOIN organizations "
%% "ON projects.organization_id = organizations.id "
%% "JOIN enterprises "
%% "ON organizations.enterprise_id = enterprises.id "
%% "WHERE enterprises.name = $1 "
%% "AND organizations.name = $2 "
%% "AND projects.name = $3 "
%% "AND pipelines.name = $4 "
%% "AND pipelines.project_id = projects.id"
-spec gen_del_by_scoping_params(atom()) -> [string()].
gen_del_by_scoping_params(Module) ->
    Genealogy = scoping_genealogy(Module),
    [{ParentModule, ParentIdField} | _Ancestors] = Genealogy,
    ParentName = ParentModule:'#table_name'(),
    ParentIdFieldStr = chef_utils:to_str(ParentIdField),
    ScopingColumn = chef_utils:to_str(Module:scoping_column_name()),
    TableName = Module:'#table_name'(),
    Constraint = case Module:scoping_index_constraint() of
        none -> "";
        C -> " AND " ++ C
    end,
    ["DELETE FROM ", TableName,
     " USING ", ParentName, " ",
     gen_scoping_joins_and_where_clauses(Genealogy),
     " AND ", TableName, ".", ScopingColumn ," = $",
     chef_utils:to_str(length(Genealogy) + 1),
     Constraint,
     " AND ", TableName, ".", ParentIdFieldStr, " = ", ParentName, ".id"].

%% @private
%% @doc Generates the query to fetch by scoping names
%% Makes the same assumptions as `gen_insert_by_scoping_params/1' above
%%
%% Examples: for organizations:
%% "SELECT organizations.id, organizations.enterprise_id, organizations.name "
%% "FROM organizations "
%% "JOIN enterprises "
%% "ON organizations.enterprise_id = enterprises.id "
%% "WHERE enterprises.name = $1 AND organizations.name = $2"
%%
%% And for pipelines:
%% "SELECT pipelines.id, pipelines.project_id, pipelines.name "
%% "FROM pipelines "
%% "JOIN projects "
%% "ON pipelines.project_id = projects.id "
%% "JOIN organizations "
%% "ON projects.organization_id = organizations.id "
%% "JOIN enterprises "
%% "ON organizations.enterprise_id = enterprises.id "
%% "WHERE enterprises.name = $1 "
%% "AND organizations.name = $2 "
%% "AND projects.name = $3 "
%% "AND pipelines.name = $4"
-spec gen_fetch_by_scoping_params(atom()) -> [string()].
gen_fetch_by_scoping_params(Module) ->
    {BaseFetch, Depth} = gen_base_scoped_fetch(Module, Module:fields()),
    ScopingColumn = chef_utils:to_str(Module:scoping_column_name()),
    BaseFetch ++ [" AND ", Module:'#table_name'(),
                  ".", ScopingColumn, " = $", chef_utils:to_str(Depth)].

%% @private
%% @doc Generates a base statement for fetch queries
%% Returns the base query and the depth of the object in the scoping tree
%% (useful too add other query parameters if needed)
-spec gen_base_scoped_fetch(atom(), [str_or_binary() | atom()])
        -> {[string()], non_neg_integer()}.
gen_base_scoped_fetch(Module, FieldsToFetch) ->
    Genealogy = scoping_genealogy(Module),
    TableName = Module:'#table_name'(),
    FieldsSql = string:join(lists:map(
        fun(FieldName) -> TableName ++ "." ++ chef_utils:to_str(FieldName) end,
        FieldsToFetch
    ), ", "),
    {["SELECT ", FieldsSql, " FROM ", TableName, " ",
      gen_single_join_statetement(Module),
      gen_scoping_joins_and_where_clauses(Genealogy)],
     length(Genealogy) + 1}.

%% @private
%% @doc Generates a join statement, with the constraint on the current module
-spec gen_single_join_statetement(atom()) -> string().
gen_single_join_statetement(Module) ->
    Base = gen_single_join_statetement_without_constraint(Module),
    Constraint = case Module:scoping_index_constraint() of
        none -> "";
        C -> ["AND ", C, " "]
    end,
    lists:flatten(Base, Constraint).

%% @private
%% @doc Generates a join statement, ommitting the constraint on the current module
-spec gen_single_join_statetement_without_constraint(atom()) -> string().
gen_single_join_statetement_without_constraint(Module) ->
    gen_single_join_statetement_without_constraint(Module, Module:scoping_parent()).
gen_single_join_statetement_without_constraint(_Module, none) -> "";
gen_single_join_statetement_without_constraint(Module, {ParentModule, ParentIdField}) ->
    ParentIdFieldStr = chef_utils:to_str(ParentIdField),
    ParentName = ParentModule:'#table_name'(),
    TableName = Module:'#table_name'(),
    ["JOIN ", ParentName,
     " ON ", TableName, ".", ParentIdFieldStr, " = ", ParentName, ".id "].

%% @private
%% @doc Generates the common bit to all 3 scoped queries that joins on
%% scoping tables,
%%
%% e.g.for organizations:
%% "WHERE enterprises.name = $1"
%%
%% or for pipelines:
%% "JOIN organizations "
%% "ON projects.organization_id = organizations.id "
%% "JOIN enterprises "
%% "ON organizations.enterprise_id = enterprises.id "
%% "WHERE enterprises.name = $1 "
%% "AND organizations.name = $2 "
%% "AND projects.name = $3"
%%
%% It's a somewhat ugly piece of code, and not super-efficient
%% either, but that's okay since it just runs once when the app starts
%% to generate the statements
%% and it's somewhat easy to read if you follow on the examples above
-spec gen_scoping_joins_and_where_clauses([{atom(), atom()}]) -> [string()].
gen_scoping_joins_and_where_clauses(Genealogy) ->
    Joins = gen_scoping_joins(Genealogy),
    Clauses = gen_scoping_clauses(Genealogy),
    lists:flatten(Joins, Clauses).

%% @private
%% @doc This generates the 'join' part for
%% `gen_scopgen_scoping_joins_and_where_clause'
%% for example generate for pipelines:
%% "JOIN organizations "
%% "ON projects.organization_id = organizations.id "
%% "JOIN enterprises "
%% "ON organizations.enterprise_id = enterprises.id "
-spec gen_scoping_joins([{atom(), atom()}]) -> [string()].
gen_scoping_joins(Genealogy) ->
    lists:reverse(lists:foldl(
        fun({Module, _}, Acc) ->
            [Acc | gen_single_join_statetement(Module)]
        end,
        [],
        Genealogy
    )).

%% @private
%% @doc This generates the 'where ... and ...' part for
%% `gen_scoping_joins_and_where_clauses/1' above
%% Will for example generate for pipelines:
%% "WHERE enterprises.name = $1 "
%% "AND organizations.name = $2 "
%% "AND projects.name = $3"
-spec gen_scoping_clauses([{atom(), atom()}]) -> [string()].
gen_scoping_clauses(Genealogy) ->
    {WhereClauses, _} = lists:foldl(
        fun({Parent, _ParentIdField}, {Acc, CurrentIdx}) ->
            ScopingColumn = chef_utils:to_str(Parent:scoping_column_name()),
            {[Parent:'#table_name'() ++ "." ++ ScopingColumn ++ " = $"
                ++ chef_utils:to_str(CurrentIdx) | Acc],
             CurrentIdx + 1}
        end,
        {[], 1},
        lists:reverse(Genealogy)
    ),
    ["WHERE " | string:join(lists:reverse(WhereClauses), " AND ")].

%% @private
%% @doc Returns the parents of a module in the scoping tree
%% e.g. for organizations
%% [{deliv_enterprise, enterprise_id}]
%% or for pipelines
%% [{deliv_project, project_id},
%%  {deliv_organization, organization_id},
%%  {deliv_enterprise, enterprise_id}]
-spec scoping_genealogy(atom()) -> [{atom(), atom()}].
scoping_genealogy(Module) ->
    scoping_genealogy_rec(Module, []).

-spec scoping_genealogy_rec(atom(), [{atom(), atom()}])
        -> [{atom(), atom()}].
scoping_genealogy_rec(Module, Parents) ->
    case Module:scoping_parent() of
        none ->
            lists:reverse(Parents);
        {ParentModule, ParentIdField} ->
            scoping_genealogy_rec(ParentModule,
                [{ParentModule, ParentIdField} | Parents])
    end.

%% @doc This generates a query that fetches all the objects' names based
%% on scoping names
%%
%% Examples: for organizations
%% "SELECT organizations.name FROM organizations "
%% "JOIN enterprises "
%% "ON organizations.enterprise_id = enterprises.id "
%% "WHERE enterprises.name = $1 "
%% "ORDER BY organizations.name"
%%
%% and for projects:
%% "SELECT projects.name FROM projects "
%% "JOIN organizations "
%% "ON projects.organization_id = organizations.id "
%% "JOIN enterprises "
%% "ON organizations.enterprise_id = enterprises.id "
%% "WHERE enterprises.name = $1 "
%% "AND organizations.name = $2"
%% "ORDER BY projects.name"
-spec gen_fetch_names_by_scoping_names(atom()) -> [string()].
gen_fetch_names_by_scoping_names(Module) ->
    {BaseFetch, _Depth} = gen_base_scoped_fetch(Module, ["name"]),
    BaseFetch ++ [" ORDER BY ", Module:'#table_name'(), ".name"].

%% @doc Generates a query to fetch an object's scoping params from its id
%% We can't use `gen_base_scoped_fetch/2' here because we don't want to use
%% the `scoping_index_constraint' on the current table here!
%%
%% Example for changes:
%% "SELECT enterprises.name, organizations.name, projects.name, pipelines.name "
%% "FROM changes "
%% "JOIN pipelines "
%% "ON changes.pipeline_id = pipelines.id "
%% "AND changes.merge_sha IS NULL "
%% "JOIN projects "
%% "ON pipelines.project_id = projects.id "
%% "JOIN organizations "
%% "ON projects.organization_id = organizations.id "
%% "JOIN enterprises "
%% "ON organizations.enterprise_id = enterprises.id "
%% "WHERE changes.id = $1"
-spec gen_fetch_scoping_params_from_id(atom()) -> {atom(), [string()]}.
gen_fetch_scoping_params_from_id(Module) ->
    Genealogy = scoping_genealogy(Module),
    FieldsSql = string:join(lists:map(
        fun({Mod, _}) ->
            ScopingColName = Mod:scoping_column_name(),
            Mod:'#table_name'() ++ "." ++ chef_utils:to_str(ScopingColName)
        end,
        lists:reverse(Genealogy)
    ), ", "),
    TableName = Module:'#table_name'(),
    Q = ["SELECT ", FieldsSql, " FROM ", TableName, " ",
         gen_single_join_statetement_without_constraint(Module),
         gen_scoping_joins(Genealogy),
         "WHERE ", TableName, ".id = $1"],
    {fetch_scoping_params_from_id, Q}.

%% @doc This grabs the scoping params from the DB for any scoped object
%% assumes that a query `fetch_scoping_params_from_id' has been defined
%% for that module by `gen_fetch_scoping_params_from_id' above
-spec fetch_scoping_params_from_id(atom(), any())
        -> [any()] | {error, _Why}.
fetch_scoping_params_from_id(Module, Id) ->
    case deliv_db:select(Module, fetch_scoping_params_from_id, [Id]) of
        {error, _Why} = Error ->
            Error;
        {ok, []} ->
            [];
        {ok, [Row]} ->
            {_Keys, Values} = lists:unzip(Row),
            Values;
        {ok, Rows} ->
            {error, {too_many_rows, Rows}}
    end.
