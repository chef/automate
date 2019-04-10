%% Copyright 2016 Chef, Inc. All Rights Reserved

-module(deliv_team).
-behaviour(deliv_sqerl_rec).

-include("deliv_types.hrl").

%% DB operations
-export([
         fetch_names/1,
         insert/2,
         fetch/2,
         fetch_members/2,
         add_member/2
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
-export([ ]).

-compile({parse_transform, sqerl_gobot}).

-record(deliv_team, {
          id                :: db_id(),
          enterprise_id     :: db_id(),
          name              :: binary(),
          description       :: binary(),
          creator_id        :: db_id(),
          updater_id        :: db_id(),
          updated_at        :: calendar:datetime()
         }).

-spec fetch_names(binary()) -> db_op_result(binary()).
fetch_names(EntName) ->
    deliv_db:fetch_names(?MODULE, [EntName]).

-spec insert(binary(),  proplist() | d_team()) ->  db_op_result(d_team()).
insert(EntName, PropList) when erlang:is_list(PropList) ->
    insert(EntName, fromlist(PropList));
insert(EntName, #deliv_team{} = TeamRec) ->
    deliv_db:insert(?MODULE, [EntName], TeamRec).

-spec fetch(binary(), binary()) -> { ok, d_team() } | { error, _Why }.
fetch(EntName, TeamName) ->
    deliv_db:fetch(?MODULE, [EntName], TeamName).

-spec fetch_members(binary(), binary()) -> [json()] | {error, _Why}.
fetch_members(EntName, TeamName) ->
    deliv_db:query_to_json(?MODULE, fetch_members, [EntName, TeamName]).

-spec add_member(non_neg_integer(), non_neg_integer()) -> db_op_result(any()).
add_member(TeamId, MemberId) ->
    sqerl_rec:cquery(?MODULE, add_member_to_team, [TeamId, MemberId]).

'#insert_fields'() ->
    [enterprise_id, name, description, creator_id, updater_id, updated_at].

'#update_fields'() ->
    [enterprise_id, name, description, creator_id, updater_id, updated_at].

'#statements'() ->
    [default,
     {fetch_names,
      deliv_sqerl_rec:gen_fetch_names_by_scoping_names(?MODULE)},
     {fetch_members,
      <<"SELECT d.name,
                COALESCE(d.first_name, '') || ' ' || COALESCE(d.last_name, '') AS full_name,
                string_agg(e.role::TEXT, ', ' ORDER BY e.role::TEXT) AS role
           FROM enterprises a
          INNER JOIN teams b ON a.id = b.enterprise_id
          INNER JOIN team_members c ON b.id = c.team_id
          INNER JOIN users d ON c.user_id = d.id
           LEFT OUTER JOIN enterprise_user_roles e ON d.id = e.user_id
          WHERE a.name = $1
            AND b.name = $2
          GROUP BY d.name, d.first_name, d.last_name
          ORDER BY d.name">>},
     {add_member_to_team, <<"INSERT INTO team_members (team_id, user_id)
                             VALUES ($1, $2)">>}
     | deliv_sqerl_rec:gen_scoped_sqerl_statements(?MODULE)].

'#table_name'() ->
    "teams".

-spec scoping_parent() -> {atom(), atom()}.
scoping_parent() ->
    {deliv_enterprise, enterprise_id}.

-spec scoping_column_name() -> name.
scoping_column_name() -> name.

-spec scoping_index_constraint() -> none.
scoping_index_constraint() -> none.
