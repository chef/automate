-module(deliv_enterprise).
-behaviour(deliv_sqerl_rec).

-include("deliv_types.hrl").

%% DB operations
-export([
         delete/1,
         fetch/1,
         fetch_by_id/1,
         insert/1,
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
         from_name/1,
         list_all/0,
         pipelines_with_stats/1,
         rename/2,
         get_canonical_enterprise/0
        ]).

-compile({parse_transform, sqerl_gobot}).

-record(deliv_enterprise, {
          id    :: db_id(),
          name  :: binary()
         }).

'#insert_fields'() ->
    [name].

'#update_fields'() ->
    [name].

'#statements'() ->
    [default,
     {fetch_all, sqerl_rec:gen_fetch_all(?MODULE, name)},
     {fetch_by_name, sqerl_rec:gen_fetch(?MODULE, name)},
     {delete_by_name, sqerl_rec:gen_delete(?MODULE, name)},
     {get_pipelines_with_stats,
      <<"SELECT * FROM current_pipelines_stats_for_enterprise($1)">>},
     {get_canonical_enterprise,
      ["SELECT name FROM ",
       '#table_name'(),
       " LIMIT 2"]}
    ].

'#table_name'() ->
    "enterprises".

-spec scoping_parent() -> none.
scoping_parent() -> none.

-spec scoping_column_name() -> name.
scoping_column_name() -> name.

-spec scoping_index_constraint() -> none.
scoping_index_constraint() -> none.

-spec delete(binary() | d_enterprise()) -> ok | {error, any()}.
delete(Name) when erlang:is_binary(Name) ->
    Obj = from_name(Name),
    deliv_db:delete2(Obj, name);
delete(Ent) ->
    deliv_db:delete(Ent).

%% @doc Fetch by name
-spec fetch(binary()) -> {error, not_found | _Why} | {ok, d_enterprise()}.
fetch(EntName) ->
    deliv_db:fetch(?MODULE, EntName).

%% @doc Fetch by Id
-spec fetch_by_id(db_id()) -> {error, not_found} | {ok, d_enterprise()}.
fetch_by_id(EnterpriseId) ->
    deliv_db:fetch_by_id(?MODULE, EnterpriseId).

-spec insert(binary()) -> db_op_result(d_enterprise()).
insert(EntName) ->
    deliv_db:insert(?MODULE, EntName).

-spec update(d_enterprise()) -> {error, conflict | any()} | {ok, d_enterprise()}.
update(Ent) ->
    deliv_db:update(Ent).

-spec list_all() -> db_op_result(d_enterprise()).
list_all() ->
    deliv_db:list_all(?MODULE).

-spec rename(binary(), binary()) -> {error, conflict | not_found | any()}
                                        | {ok, d_enterprise()}.
rename(OldName, NewName) ->
    deliv_db:rename(?MODULE, OldName, NewName).

-spec from_name(binary()) -> deliv_sqerl_record().
from_name(Name) ->
    fromlist([{name, Name}]).

%% @doc Fetches scoped pipelines given the enterprise name as a binary.
%% Returns and erro if the enterprise is not found, and empty list if no
%% pipelines are found, and a list of records like like the following if
%% there are pipelines on the enterprise.
%%
%% [{<<"id">>,23},
%%  {<<"org_name">>,<<"sandbox">>},
%%  {<<"proj_name">>,<<"game_of_thrones">>},
%%  {<<"name">>,<<"master">>},
%%  {<<"build_count">>, 3},
%%  {<<"build_status">>, <<"passed">>},
%%  {<<"acceptance_count">>, 3},
%%  {<<"acceptance_status">>, <<"passed">>},
%%  {<<"last_deployed">>, <<"2015-02-11 23:55:55.525589+00">>},
%%  {<<"last_delivered">>, <<"2015-02-11 23:55:55.525589+00">>}]
-spec pipelines_with_stats(binary()) -> {ok, [[deliv_sqerl_record()]]} | db_op_result().
pipelines_with_stats(EntName) ->
    case sqerl:execute(deliv_enterprise_get_pipelines_with_stats, [EntName]) of
        {ok, Result} ->
            %% TODO need to process into something easier to manage
            {ok, Result};
        {error, Error} ->
            %% TODO translate possible error cases
            {error, Error}
    end.

%% @doc If there exists exactly one enterprise, this returns its name
-spec get_canonical_enterprise()
        -> {ok, binary()} | {error, term() | no_canonical_enterprise}.
get_canonical_enterprise() ->
    case sqerl_rec:scalar_fetch(?MODULE, get_canonical_enterprise, []) of
        [EntName] ->
            {ok, EntName};
        X when is_list(X) ->
            {error, no_canonical_enterprise};
        {error, Why} = Error ->
            chef_log:error("Error getting canonical enterprise: ~p", [Why]),
            Error
    end.
