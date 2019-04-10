%% Copyright 2014 Opscode, Inc. All Rights Reserved.

%% @doc Represents a user

-module(deliv_user).
-behaviour(deliv_sqerl_rec).

-include("deliv_types.hrl").

%% DB operations
-export([
        delete/1,
        delete/2,
        fetch/1,
        fetch/2,
        fetch_by_alias/2,
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
        is_internal/1,
        new_external_user/2,
        fetch_user_names/1,
        revoke_token/3,
        verify_password/4,
        get_token/5,
        commenters_for_change/1,
        scoping_names_by_id/1,
        scoping_names/1,
        email_user/3,
        user_description_for_pr_comment/2
       ]).

%% Authorization Exports
-export([
         edit_roles/3,
         effective_roles/2,
         scoped_roles/2
        ]).

-compile({parse_transform, sqerl_gobot}).

-record(deliv_user, {
          id             :: db_id(),
          enterprise_id  :: db_id(),
          name           :: binary(),
          ssh_pub_key    :: binary(),
          first_name     :: binary(),
          last_name      :: binary(),
          email          :: binary(),
          user_type      :: binary(),
          telemetry_enabled :: binary()
         }).

'#insert_fields'() ->
    [enterprise_id,
     name,
     ssh_pub_key,
     first_name,
     last_name,
     email,
     user_type,
     telemetry_enabled].

'#update_fields'() ->
    [name,
     ssh_pub_key,
     first_name,
     last_name,
     user_type,
     email,
     telemetry_enabled].

'#statements'() ->
    [default,
     {fetch_user_names,
      <<"SELECT u.name
         FROM users AS u
         JOIN enterprises AS e
           ON u.enterprise_id = e.id
         WHERE e.name = $1
         ORDER BY name">>},
     {candidate_tokens,
      <<"SELECT token, birthday
         FROM candidate_tokens($1, $2, $3)">>},
     {assign_token,
      <<"SELECT assign_token($1, $2, $3)">>},
     {revoke_token,
      <<"SELECT revoke_token($1, $2, $3)">>},
     {assign_roles_at_enterprise,
      <<"SELECT assign_roles_at_enterprise($1, $2, $3)">>},
     {scoping_names_by_id,
        <<"SELECT e.name AS ent_name
            FROM users AS u
            JOIN enterprises AS e
            ON u.enterprise_id = e.id
            WHERE u.id = $1">>},

     %% Authorization Queries
     %%
     %% Checking authorization roles for a user at any scope uses the
     %% same underlying database call, just parameterized
     %% differently. When querying for "scoped roles" (i.e., all the
     %% roles a user has, along with all the scopes at which each role
     %% is granted), we return a "role" and "scopes" column from the
     %% query. When simply asking what the effective roles are (i.e.,
     %% no scope information, just show what roles are in effect, as
     %% for basic API authorization checks), we just return the
     %% "roles" column.
     %%
     %% The underlying "scoped_roles" function takes 5 arguments:
     %%   - $1: enterprise_name
     %%   - $2: user_name
     %%   - $3: organization_name
     %%   - $4: project_name
     %%   - $5: pipeline_name
     %%
     %% When an argument is not needed (e.g., when
     %% asking for roles at the organization scope, the project and
     %% pipeline names are not required), a NULL should be passed
     %% instead; the prepared queries below handle this for you.
     {effective_roles_at_enterprise,
      <<"SELECT role from scoped_roles($1, $2, NULL, NULL, NULL)">>},

     {effective_roles_at_organization,
      <<"SELECT role from scoped_roles($1, $2, $3, NULL, NULL)">>},

     {effective_roles_at_project,
      <<"SELECT role from scoped_roles($1, $2, $3, $4, NULL)">>},

     {effective_roles_at_pipeline,
      <<"SELECT role from scoped_roles($1, $2, $3, $4, $5)">>},

     {scoped_enterprise_roles,
      <<"SELECT role, scopes from scoped_roles($1, $2, NULL, NULL, NULL)">>},

     {scoped_organization_roles,
      <<"SELECT role, scopes from scoped_roles($1, $2, $3, NULL, NULL)">>},

     {scoped_project_roles,
      <<"SELECT role, scopes from scoped_roles($1, $2, $3, $4, NULL)">>},

     {scoped_pipeline_roles,
      <<"SELECT role, scopes from scoped_roles($1, $2, $3, $4, $5)">>},

     {grant_enterprise_roles,
      <<"SELECT role, scopes FROM grant_roles($1, $2, NULL, NULL, NULL, $3)">>},
     {grant_organization_roles,
      <<"SELECT role, scopes FROM grant_roles($1, $2, $3, NULL, NULL, $4)">>},
     {grant_project_roles,
      <<"SELECT role, scopes FROM grant_roles($1, $2, $3, $4, NULL, $5)">>},
     {grant_pipeline_roles,
      <<"SELECT role, scopes FROM grant_roles($1, $2, $3, $4, $5, $6)">>},

     {revoke_enterprise_roles,
      <<"SELECT role, scopes FROM revoke_roles($1, $2, NULL, NULL, NULL, $3)">>},
     {revoke_organization_roles,
      <<"SELECT role, scopes FROM revoke_roles($1, $2, $3, NULL, NULL, $4)">>},
     {revoke_project_roles,
      <<"SELECT role, scopes FROM revoke_roles($1, $2, $3, $4, NULL, $5)">>},
     {revoke_pipeline_roles,
      <<"SELECT role, scopes FROM revoke_roles($1, $2, $3, $4, $5, $6)">>},

     {set_enterprise_roles,
      <<"SELECT role, scopes FROM set_roles($1, $2, NULL, NULL, NULL, $3)">>},
     {set_organization_roles,
      <<"SELECT role, scopes FROM set_roles($1, $2, $3, NULL, NULL, $4)">>},
     {set_project_roles,
      <<"SELECT role, scopes FROM set_roles($1, $2, $3, $4, NULL, $5)">>},
     {set_pipeline_roles,
      <<"SELECT role, scopes FROM set_roles($1, $2, $3, $4, $5, $6)">>},

     %% this query is used by `deliv_change:commenters/1'
     {commenters_for_change,
      ["SELECT DISTINCT u.* "
       "FROM comments AS co "
       "JOIN patchsets AS p "
       "ON co.patchset_id = p.id "
       "JOIN users AS u "
       "ON co.submitter_id = u.id "
       "WHERE p.change_id = $1"]},

      {fetch_by_app_and_alias,
        <<"SELECT u.id,"
                " u.enterprise_id, "
                " u.name, "
                " u.ssh_pub_key, "
                " u.first_name, "
                " u.last_name, "
                " u.email, "
                " u.user_type, "
                " u.telemetry_enabled "
          "FROM users as u, oauth_user_aliases as oua "
          "WHERE oua.oauth_app_id=$1 "
          "AND oua.alias=$2 "
          "AND oua.user_id = u.id">>}

    | deliv_sqerl_rec:gen_scoped_sqerl_statements(?MODULE)].

-spec scoping_parent() -> {atom(), atom()}.
scoping_parent() ->
    {deliv_enterprise, enterprise_id}.

-spec scoping_column_name() -> name.
scoping_column_name() -> name.

-spec scoping_index_constraint() -> none.
scoping_index_constraint() -> none.

'#table_name'() ->
    "users".

-spec is_internal(d_user()) -> boolean().
is_internal(User) ->
    deliv_user:getval(user_type, User) =:= <<"internal">>.

%% @doc Returns the names of all users of an enterprise (whether they
%% are "internal" users, or "cached" users). Names are returned sorted.
-spec fetch_user_names(binary()) -> db_op_result(binary()).
fetch_user_names(EntName) ->
    sqerl_rec:scalar_fetch(?MODULE, fetch_user_names, [EntName]).

-spec delete(d_user()) -> {ok, integer()} | {error, _}.
delete(User) ->
    deliv_db:delete(User).

-spec delete(binary(), binary()) -> ok | {error, _}.
delete(EntName, UserName) ->
    deliv_db:delete(?MODULE, [EntName], UserName).

-spec fetch(non_neg_integer()) -> {error, not_found, _Why} | {ok, d_user()}.
fetch(UserId) ->
    deliv_db:fetch_by_id(?MODULE, UserId).

-spec fetch(binary(), binary()) -> {ok, d_user()} | {error, not_found | _Why}.
fetch(EntName, UserName) ->
    deliv_db:fetch(?MODULE, [EntName], UserName).

-spec fetch_by_alias(d_oauth_application(), binary()) -> {error, atom()} | {ok, d_user()}.
fetch_by_alias(OauthApp, AliasName) ->
    OauthAppId = deliv_oauth_application:getval(id, OauthApp),
    case deliv_db:qfetch(?MODULE, fetch_by_app_and_alias, [OauthAppId, AliasName]) of
        [] ->
            AppName = deliv_oauth_application:getval(name, OauthApp),
            chef_log:error("Failed to find user associated for OAuth application ~s with alias ~s",
                            [AppName, AliasName]),
            {error, not_found};
        [User] ->
            {ok, User};
        {error, Why} = Err ->
            AppName = deliv_oauth_application:getval(name, OauthApp),
            chef_log:error("Failed to find user associated for OAuth application ~s with alias ~s - ~p",
                            [AppName, AliasName, Why]),
            Err
    end.

%% @doc Inserts the data as-is; use `new_external_user/2'
%% to insert a new external user
-spec insert(binary(), d_user() | proplist(atom(), any())) -> db_op_result(d_user()).
insert(EntName, Data) ->
    deliv_db:insert(?MODULE, [EntName], Data).

%% @doc Fetches the user's data from the LDAP server,
%% then inserts everything in the DB
-spec new_external_user(binary(), proplist(atom(), any())) -> db_op_result(d_user()).
new_external_user(EntName, UserProplist) ->
    MergedUserAttrs = fetch_and_merge_ldap_attrs(UserProplist),
    insert(EntName, MergedUserAttrs).

%% @private
%% @doc Fetches an external user's data from the LDAP server and merges theme
%% merges them with the passed in UserAttrs. The merge prefers the passed in
%% values since they correspond to data entered by the user.
-spec fetch_and_merge_ldap_attrs(proplist(atom(), any())) -> proplist(atom(), any()).
fetch_and_merge_ldap_attrs(UserAttrs) ->
    UserName = proplists:get_value(name, UserAttrs),
    LdapAttrs = deliv_ldap:mapped_attrs_for(iolist_to_binary(UserName)),
    chef_utils:merge_proplists(UserAttrs, LdapAttrs).

-spec update(d_user()) -> db_op_single_result(d_user()).
update(User) ->
    deliv_db:update(User).

-spec scoping_names(d_user()) -> {binary()} | {error, atom()}.
scoping_names(User) ->
    UserId = getval(id, User),
    scoping_names_by_id(UserId).

-spec scoping_names_by_id(non_neg_integer()) -> {binary()} | {error, atom()}.
scoping_names_by_id(UserId) ->
    case deliv_db:select(?MODULE, scoping_names_by_id, [UserId]) of
        {ok, [Row]} ->
            {proplists:get_value(<<"ent_name">>, Row)};
        {error, _} = Error ->
            Error
    end.

%% @doc Revokes user's `Token' in the given enterprise.
-spec revoke_token(EntName::binary(),
                   UserName::binary(),
                   Token::binary()) -> ok | not_found | {error, term()}.
revoke_token(EntName, UserName, Token) ->
    case deliv_db:select(?MODULE, revoke_token,
                         [EntName, UserName, Token]) of
        {ok,[[{<<"revoke_token">>, Token}]]} ->
            ok;
        {ok,[[{<<"revoke_token">>, null}]]} ->
            not_found;
        Error ->
            Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Scoped Roles

%% @doc Return an EJson object mapping a role name to a list of scopes
%% at which they have been granted to a user.
%%
%% (Currently this is only used to create REST API responses, so we
%% just process into EJson here.)
%%
%% Example:
%%
%%   {
%%     [
%%      {<<"committer">>, [<<"enterprise">>, <<"organization">>]},
%%      {<<"reviewer">>, [<<"project">>]}
%%     ]
%%   }
%%
%% For a description of `Parameters', see the query documentation
%% above in `'#statements'/0`.
-spec scoped_roles(deliv_scope(), [binary()]) -> json() | {error, term()}.
scoped_roles(Scope, Parameters) ->
    case deliv_db:select(?MODULE, scoped_roles_query(Scope),
                         %% TODO: rename the handler key to something
                         %% like 'requesting_user'
                         Parameters) of
        {ok, Roles} ->
            process_scoped_roles(Roles);
        {error, Why} ->
            {error, deliv_pg:translate_error(Why)}
    end.

%% @doc Select an appropriate user authorization function based on the
%% requested scope.
scoped_roles_query(enterprise)   -> scoped_enterprise_roles;
scoped_roles_query(organization) -> scoped_organization_roles;
scoped_roles_query(project)      -> scoped_project_roles;
scoped_roles_query(pipeline)     -> scoped_pipeline_roles.

%% @doc Process the raw proplists from sqerl for scoped roles into an
%% EJson structure.
%%
%% The table has `role' (`binary()') and `scopes' (`[binary()]') columns,
%% and must be turned into a hash, mapping `role' to `scopes'.
process_scoped_roles(Rows) ->
    {[{proplists:get_value(<<"role">>, Row),
       proplists:get_value(<<"scopes">>, Row)} || Row <- Rows]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Effective Roles (Individual User Authorization)

%% @doc Query the database to find the user's effective roles at the
%% given scope. This list of roles eventually will be stored in the
%% handler state for use elsewhere in the request processing.
%%
%% For a description of `Parameters', see the query documentation
%% above in `'#statements'/0`.
-spec effective_roles(deliv_scope(), [binary()]) -> {ok, [deliv_role()]} |
                                                    {error, _Why}.
effective_roles(Scope, Parameters) ->
    deliv_db:select(?MODULE, effective_roles_query(Scope),
                    Parameters, rows_as_scalars, [role]).

effective_roles_query(enterprise)   -> effective_roles_at_enterprise;
effective_roles_query(organization) -> effective_roles_at_organization;
effective_roles_query(project)      -> effective_roles_at_project;
effective_roles_query(pipeline)     -> effective_roles_at_pipeline.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Grant, Revoke, Set Roles

-spec edit_roles(Op, Scope, Parameters) -> json() | {error, _Why} when
    Op :: grant | revoke | set,
    Scope :: deliv_scope(),
    Parameters :: list().
edit_roles(Op, Scope, Parameters) ->
    case deliv_db:select(?MODULE, edit_roles_query(Op, Scope),
                         Parameters) of
        {ok, Roles} ->
            process_scoped_roles(Roles);
        {error, Why} ->
            {error, deliv_pg:translate_error(Why)}
    end.

%% Yeah, a little gross, but whatever...
edit_roles_query(grant, enterprise)    -> grant_enterprise_roles;
edit_roles_query(grant, organization)  -> grant_organization_roles;
edit_roles_query(grant, project)       -> grant_project_roles;
edit_roles_query(grant, pipeline)      -> grant_pipeline_roles;
edit_roles_query(revoke, enterprise)   -> revoke_enterprise_roles;
edit_roles_query(revoke, organization) -> revoke_organization_roles;
edit_roles_query(revoke, project)      -> revoke_project_roles;
edit_roles_query(revoke, pipeline)     -> revoke_pipeline_roles;
edit_roles_query(set, enterprise)      -> set_enterprise_roles;
edit_roles_query(set, organization)    -> set_organization_roles;
edit_roles_query(set, project)         -> set_project_roles;
edit_roles_query(set, pipeline)        -> set_pipeline_roles.

%verify password, token
-spec verify_password(Type, UserName, EntName, Password)  -> verified | {error, term()} when
      Type :: binary(),
      UserName :: binary(),
      EntName :: binary(),
      Password :: binary().
verify_password(<<"internal">>, UserName, EntName, Password) ->
    deliv_intern_user_authn:verify_password(EntName, UserName, Password);
verify_password(<<"saml">>, _, _, _) ->
    {error, saml_password_authn_not_supported};
verify_password(<<"external">>, UserName, _EntName, Password) ->
    case deliv_ldap:verify_password(UserName, Password) of
        {verified, _} -> verified;
        Other -> Other
    end.

-spec get_token(Resp, EntName, UserName, Req, State) ->  {true|halt, OutputReq, State} when
      Resp :: denied | {error, saml_password_authn_not_supported | binary()} | verified,
      EntName :: binary(),
      UserName :: binary(),
      Req :: cowboy_req(),
      OutputReq :: cowboy_req(),
      State :: any().
get_token(denied, _EntName, _UserName, Req, State) ->
    deliv_web_utils:reply_unauthorized(Req, State);
get_token({error, saml_password_authn_not_supported}, _, _, Req, State) ->
    deliv_web_utils:error_response(400,
                                   bad_request, <<"Password authentication for SAML user is not supported.">>, Req, State);
get_token({error, Reason}, EntName, UserName, Req, State) ->
    chef_log:error("Unable to verify password for ~s/~s -> ~p", [EntName, UserName, Reason]),
    deliv_web_utils:error_response(500,
                                   internal_server_error, Req, State);
get_token(verified, EntName, UserName, Req, State = #handler{read_ttl = TTL}) ->
    case deliv_token:assign_token(EntName, UserName) of
        {ok, Token} ->
            BodyEjson = {[{<<"token">>, Token}, {<<"ttl">>, TTL}]},
            Req1 = deliv_web_utils:set_json_body(BodyEjson, Req),
            {true, Req1, State};
        {error, Reason} ->
            chef_log:error("Unable to get a token for ~p/~p -> ~p", [EntName, UserName, Reason]),
            deliv_web_utils:error_response(500,
                                           internal_server_error, Req, State)
    end.

%% @doc Returns the list of all users who commented on a given change
-spec commenters_for_change(d_change() | binary()) -> db_op_result(d_user()).
commenters_for_change(Change) when erlang:is_tuple(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    commenters_for_change(ChangeId);
commenters_for_change(ChangeId) when erlang:is_binary(ChangeId) ->
    deliv_db:qfetch(?MODULE, commenters_for_change, [ChangeId]).

-spec email_user(binary(), binary(), fun((binary()) -> any())) -> {error, user_not_found | no_user_email} | {error, user_not_found, any()} | any().
email_user(EntName, UserName, SendEmailFun) ->
    email_user(deliv_user:fetch(EntName, UserName), SendEmailFun).

email_user({ok, User}, SendEmailFun) ->
    send(deliv_user:getval(email, User), SendEmailFun);
email_user({error, not_found}, _SendEmailFun) ->
    {error, user_not_found};
email_user({error, Why}, _SendEmailFun) ->
    {error, user_not_found, Why}.

send(undefined, _SendEmailFun) ->
    {error, no_user_email};
send(UserEmail, SendEmailFun) ->
    SendEmailFun(UserEmail).

-spec user_description_for_pr_comment(binary(), binary()) -> binary().
user_description_for_pr_comment(EntName, UserName) ->
     case deliv_user:fetch(EntName, UserName) of
        {error, _Why} ->
             <<"This Pull Request was opened by Chef Automate user ", UserName/binary, "\n\n">>;
        {ok, UserRecord} ->
             <<"This Pull Request was opened by Chef Automate user ", (render_names_for_ui(UserRecord))/binary, "\n\n">>
     end.

render_names_for_ui(UserRecord) ->
    case {deliv_user:getval(first_name, UserRecord), deliv_user:getval(last_name, UserRecord)} of
            {undefined, undefined} -> deliv_user:getval(name, UserRecord);
            {FirstName, undefined} -> FirstName;
            {undefined, LastName} -> LastName;
            {FirstName, LastName} -> <<FirstName/binary, " ", LastName/binary>>
    end.
