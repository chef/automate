%% Copyright 2014 CHEF Software, Inc. All Rights Reserved.

%% @doc Represents an internal user
-module(deliv_intern_user).
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
         reset_password/3,
         unset_password/2,
         disable_login_and_generate_password_reset_token/2,
         import_from_tsv_file/1,
         import_from_tsv_string/1
        ]).

-compile({parse_transform, sqerl_gobot}).

-record(deliv_intern_user,
        {
          id                :: db_id(),
          enterprise_id     :: db_id(),
          name              :: binary(),
          ssh_pub_key       :: binary(),
          first_name        :: binary(),
          last_name         :: binary(),
          email             :: binary(),
          hashed_pass       :: binary(),
          hash_type         :: binary()
        }).

'#insert_fields'() ->
    [enterprise_id,
     name,
     ssh_pub_key,
     first_name,
     last_name,
     email,
     hashed_pass,
     hash_type].

'#update_fields'() ->
    [name,
     ssh_pub_key,
     first_name,
     last_name,
     email,
     hashed_pass,
     hash_type].

'#statements'() ->
    [default,
     {assign_password_reset_token,
      <<"SELECT assign_password_reset_token($1, $2, $3, $4)">>},
     {use_password_reset_token,
      <<"SELECT use_password_reset_token($1, $2, $3)">>},
     {invalidate_passwords,
      <<"SELECT invalidate('password', $1, $2);">>}
     | deliv_sqerl_rec:gen_scoped_sqerl_statements(?MODULE)].

-spec scoping_parent() -> {atom(), atom()}.
scoping_parent() ->
    {deliv_enterprise, enterprise_id}.

-spec scoping_column_name() -> name.
scoping_column_name() -> name.

-spec scoping_index_constraint() -> none.
scoping_index_constraint() -> none.

%% Note that this is actually an updatable view, but with the triggers
%% in place, it may as well be a table!
'#table_name'() ->
    "internal_users".

-spec delete(d_intern_user()) -> ok | {error, _}.
delete(User) ->
    deliv_db:delete(User).

-spec delete(binary(), binary()) -> ok | {error, _}.
delete(EntName, UserName) ->
    deliv_db:delete(?MODULE, [EntName], UserName).

-spec fetch(binary(), binary()) -> {ok, d_intern_user()} | {error, any()}.
fetch(EntName, UserName) ->
    deliv_db:fetch(?MODULE, [EntName], UserName).

-spec insert(binary(), d_intern_user() | proplist(atom(), any())) -> db_op_result(d_intern_user()).
insert(EntName, Data) ->
    deliv_db:insert(?MODULE, [EntName], Data).

-spec update(d_intern_user()) -> db_op_result(d_intern_user()).
update(User) ->
    deliv_db:update(User).

-spec reset_password(binary(), binary(), binary()) -> db_op_result(d_intern_user())
                                                      | {error, bad_password, atom(), binary()}.
reset_password(EntName, UserName, Password) ->
    chef_log:info("reset password request for user ~s/~s", [EntName, UserName]),
    case fetch(EntName, UserName) of
        {error, not_found} -> {error, user_not_found};
        {ok, User} ->
            case user_password:hash(Password) of
                {error, Reason, Message} ->
                    {error, bad_password, Reason, Message};
                HashedPass ->
                    UpdatedUser = setvals([{hashed_pass, HashedPass},
                                           {hash_type, <<"bcrypt">>}],
                                          User),
                    update(UpdatedUser)
            end
    end.

-spec unset_password(binary(), binary()) -> ok | {error, atom()}.
unset_password(EntName, UserName) ->
    case sqerl_rec:scalar_fetch(?MODULE, invalidate_passwords, [EntName, UserName]) of
        {error, _} = Error -> Error;
        _ -> ok
    end.

-spec import_from_tsv_file(str_or_binary()) -> [db_op_result(d_intern_user())].
import_from_tsv_file(FileName) ->
    {ok, Input} = file:read_file(FileName),
    import_from_tsv_string(Input).

-spec import_from_tsv_string(str_or_binary())
        -> [db_op_result(d_intern_user()) | {error, {invalid_intern_user_input, [binary() | undefined]}}].
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

-spec disable_login_and_generate_password_reset_token(binary(), binary()) ->
    {ok, Token} | {error, atom()} when Token :: binary().
disable_login_and_generate_password_reset_token(EntName, UserName) ->
    case deliv_reset_password:generate_reset_token(EntName, UserName) of
        {error, _} = Error -> Error;
        {ok, Token} ->
            case unset_password(EntName, UserName) of
                ok -> {ok, Token};
                {error, _} = Error -> Error
            end
    end.
