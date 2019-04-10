%% Copyright 2018 CHEF Software, Inc. All Rights Reserved.

%% @doc Represents an a2 user
-module(deliv_a2_user).
-behaviour(deliv_sqerl_rec).

-include("deliv_types.hrl").

%% DB operations
-export([
         delete/1,
         delete/2,
         fetch/2,
         insert/2,
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
         import_from_tsv_file/1,
         import_from_tsv_string/1
        ]).

-compile({parse_transform, sqerl_gobot}).

-record(deliv_a2_user,
        {
          id                :: db_id(),
          enterprise_id     :: db_id(),
          name              :: binary(),
          ssh_pub_key       :: binary(),
          first_name        :: binary(),
          last_name         :: binary(),
          email             :: binary(),
          user_type         :: binary()
        }).

'#insert_fields'() ->
    [enterprise_id,
     name,
     ssh_pub_key,
     first_name,
     last_name,
     email,
     user_type].

'#update_fields'() ->
    [name,
     ssh_pub_key,
     first_name,
     last_name,
     email,
     user_type].

'#statements'() ->
    [default | deliv_sqerl_rec:gen_scoped_sqerl_statements(?MODULE)].

-spec scoping_parent() -> {atom(), atom()}.
scoping_parent() ->
    {deliv_enterprise, enterprise_id}.

-spec scoping_column_name() -> name.
scoping_column_name() -> name.

-spec scoping_index_constraint() -> none.
scoping_index_constraint() -> none.

'#table_name'() ->
    "users".

-spec delete(d_a2_user()) -> ok | {error, _}.
delete(User) ->
    deliv_db:delete(User).

-spec delete(binary(), binary()) -> ok | {error, _}.
delete(EntName, UserName) ->
    deliv_db:delete(?MODULE, [EntName], UserName).

-spec fetch(binary(), binary()) -> {ok, d_a2_user()} | {error, any()}.
fetch(EntName, UserName) ->
    deliv_db:fetch(?MODULE, [EntName], UserName).

-spec insert(binary(), d_a2_user() | proplist(atom(), any())) -> db_op_result(d_a2_user()).
insert(EntName, Data) ->
    deliv_db:insert(?MODULE, [EntName], Data).

-spec update(d_a2_user()) -> db_op_result(d_a2_user()).
update(User) ->
    deliv_db:update(User).

-spec import_from_tsv_file(str_or_binary()) -> [db_op_result(d_a2_user())].
import_from_tsv_file(FileName) ->
    {ok, Input} = file:read_file(FileName),
    import_from_tsv_string(Input).

-spec import_from_tsv_string(str_or_binary())
        -> [db_op_result(d_a2_user()) | {error, {invalid_a2_user_input, [binary() | undefined]}}].
import_from_tsv_string(Input) ->
    InternUsers = user_import_parser:intern_users_from_tsv(Input),
    chef_log:info("importing ~p users", [length(InternUsers)]),
    lists:map(
        fun({error, _} = Invalid) -> Invalid;
           (InternUser) ->
                [{enterprise_name, EntName}|_] = InternUser,
                insert(EntName, InternUser)
        end,
        InternUsers
    ).
