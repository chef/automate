-module(deliv_organization).
-behaviour(deliv_sqerl_rec).

-include("deliv_types.hrl").

%% DB operations
-export([
        delete/2,
        insert/2,
        fetch_by_id/1,
        fetch/2,
        update/1
       ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

%% deliv_sqerl_rec callbacks
-export([
        scoping_parent/0,
        scoping_column_name/0,
        scoping_index_constraint/0
       ]).

%% others
-export([
         fetch_for_ent/1,
         rename/2,
         get_changes_for_review/2
        ]).

-compile({parse_transform, sqerl_gobot}).

-record(deliv_organization, {
          id                :: db_id(),
          enterprise_id     :: db_id(),
          name              :: binary()
         }).

-spec update(d_organization()) -> {error, conflict | any()} | {ok, d_organization()}.
update(Org) ->
    deliv_db:update(Org).

-spec insert(binary(), binary()) -> db_op_result(d_organization()).
insert(EntName, OrgName) ->
    deliv_db:insert(?MODULE, [EntName], OrgName).

-spec delete(binary(), binary()) -> ok | {error, any()}.
delete(EntName, OrgName) ->
    deliv_db:delete(?MODULE, [EntName], OrgName).

-spec fetch_by_id(binary()) -> db_op_result(d_organization()).
fetch_by_id(OrganizationId) ->
    deliv_db:fetch_by_id(?MODULE, OrganizationId).

%% @doc Fetches by enterprise name & name
-spec fetch(binary(), binary()) -> {error, not_found} | {ok, d_organization()}.
fetch(EntName, OrgName) ->
    deliv_db:fetch(?MODULE, [EntName], OrgName).

%% @doc Returns all organizations for `EntName'
-spec fetch_for_ent(binary()) -> [json()] | {error, _Why}.
fetch_for_ent(EntName) ->
    deliv_db:query_to_json(?MODULE, fetch_for_ent, [EntName]).

'#insert_fields'() ->
    [enterprise_id, name].

'#update_fields'() ->
    [enterprise_id, name].

'#statements'() ->
    [default,
     {fetch_for_ent,
      "SELECT organizations.name, count(projects.id) AS project_count "
      "FROM organizations "
      "JOIN enterprises "
      "ON organizations.enterprise_id = enterprises.id "
      "LEFT JOIN projects "
      "ON projects.organization_id = organizations.id "
      "WHERE enterprises.name = $1 "
      "GROUP BY organizations.id "
      "ORDER BY organizations.name"},
     {changes_for_review,
      %% These are all the changes across all projects in the given
      %% organization that have passed the verify stage for their
      %% latest patchset, but have yet to be merged. These are things
      %% you might want to review and not feel like you're wasting
      %% your time (on code that doesn't even pass its tests).
      %%
      %% $1 = enterprise name
      %% $2 = organization name
      %%
      %% Note that we hard-code the status and ordering; allowing to
      %% be user-specifiable is work for Future Us.
      %%
      %% TODO: the WHERE clause for 'status' may (or may not) need to
      %% be changed once we finally get some constraints set up on
      %% stage statuses. If that's already been done and this still
      %% works, feel free to remove this TODO :)
      <<"SELECT change_id AS id,
                project,
                pipeline AS target,
                feature_branch AS topic,
                created_at,
                last_code_activity_at AS updated_at,
                status AS verify_status
         FROM changes_in_verify($1, $2)
         WHERE status = 'passed'
         ORDER BY last_code_activity_at DESC">>
     }
    | deliv_sqerl_rec:gen_scoped_sqerl_statements(?MODULE)].

'#table_name'() ->
    "organizations".

-spec scoping_parent() -> {atom(), atom()}.
scoping_parent() ->
    {deliv_enterprise, enterprise_id}.

-spec scoping_column_name() -> name.
scoping_column_name() -> name.

-spec scoping_index_constraint() -> none.
scoping_index_constraint() -> none.

-spec rename(d_organization(), binary()) -> {error, conflict | any()} | {ok, d_organization()}.
rename(Org, NewName) ->
    deliv_db:rename(?MODULE, Org, NewName).

%% @doc Return proplists describing all currently open unmerged
%% changes across an organization that have passed the Verify stage
%% for their latest patchsets.
%%
%% For the structure of the proplists, see the structure of the
%% `deliv_organization_changes_for_review` query.
-spec get_changes_for_review(binary(), binary()) -> {ok, [{atom(), term()}]} |
                                                    {error, term()}.
get_changes_for_review(EntName, OrgName) ->
    case sqerl:execute(deliv_organization_changes_for_review, [EntName, OrgName]) of
        {ok, Results} ->
            %% We got some results; since they don't really correspond
            %% to a record now (it's still kind of provisional /
            %% experimental code), we're just going to return
            %% proplists.
            %%
            %% Note that it is OK if Results is an empty list; that
            %% just means there aren't any unmerged, verify-passing
            %% changes in the org right now.
            {ok, Results};
        {error, _} = Error ->
            Error
    end.
