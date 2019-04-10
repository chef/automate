%% @doc Interface to manage oauth tokens
%% When you are trying to insert/get a token to/from the database, you should
%% use this module.
-module(deliv_oauth_token).

-include("deliv_types.hrl").

-compile({parse_transform, sqerl_gobot}).

%% api
-export([
         initialize/3,
         authorize_url/1,
         fetch_by_state/1,
         fetch/3,
         fetch_token/2,
         fetch_by_enterprise/2,
         save_token/2,
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

-record(deliv_oauth_token, {
          id            :: db_id(),
          oauth_app_id  :: db_id(),
          scope         :: deliv_scope(),
          scope_id      :: non_neg_integer(),
          state         :: binary(),
          token         :: binary()
         }).

'#insert_fields'() -> [oauth_app_id, scope, scope_id, state].
'#update_fields'() -> [oauth_app_id, scope, scope_id, state, token].

'#table_name'() -> "oauth_tokens".

'#statements'() ->
    [default,
     {fetch_by_state, sqerl_rec:gen_fetch(?MODULE, [state])},
     {fetch_by_enterprise,
      <<"SELECT o.token_id AS id, "
              " o.app_id AS oauth_app_id, "
              " o.scope, "
              " o.scope_id, "
              " o.state, "
              " o.token "
          "FROM oauth_integrations AS o "
          "JOIN enterprises AS e "
            "ON e.id = o.scope_id "
         "WHERE o.scope='enterprise' "
           "AND e.name=$1 "
           "AND o.module=$2">>},
     {fetch_by_scope_and_module,
      <<"SELECT o.token_id AS id, "
              " o.app_id AS oauth_app_id, "
              " o.scope, "
              " o.scope_id, "
              " o.state, "
              " o.token "
          "FROM oauth_integrations AS o "
         "WHERE scope=$1 "
           "AND scope_id=$2 "
           "AND module=$3">>}
    ].


%% @doc Create a partially hydrated oauth_token record with state (but no token).
%% TODO: initialize should return the value from authorize_url
-spec initialize(binary(), deliv_scope(), integer()) -> db_op_single_result(d_oauth_token()).
initialize(AppName, Scope, ScopeId) ->
    case deliv_oauth_application:fetch(AppName) of
        {ok, Application} ->
            OauthAppId = deliv_oauth_application:getval(id, Application),
            State = chef_utils:random_string(128),
            insert(#deliv_oauth_token{oauth_app_id = OauthAppId,
                                      scope = Scope,
                                      scope_id = ScopeId,
                                      state = State});
        {error, _Why} = Err ->
            Err
    end.

%% @doc Insert the token record into the database.
-spec insert(d_oauth_token()) -> db_op_single_result(d_oauth_token()).
insert(Record = #deliv_oauth_token{}) ->
    case deliv_db:insert(Record) of
        [InsertedRecord] ->
            {ok, InsertedRecord};
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, insert, [Record], Why),
            Err
    end.

%% @doc Update the database
-spec update(d_oauth_token()) -> db_op_single_result(d_oauth_token()).
update(Record) ->
    deliv_db:update(Record).

%% @doc Return the URL that should be used to request a token.
-spec authorize_url(d_oauth_token()) -> string() | {error, atom()}.
authorize_url(OauthRecord) ->
    OauthAppId = deliv_oauth_token:getval(oauth_app_id, OauthRecord),
    {ok, Application} = deliv_oauth_application:fetch_by_id(OauthAppId),
    State = deliv_oauth_token:getval(state, OauthRecord),
    OauthModule = deliv_oauth_application:get_module(Application),
    OauthModule:authorize_url(State, Application).

%% @doc Fetch a token record by its state value.
-spec fetch_by_state(binary()) -> db_op_single_result(d_oauth_token()).
fetch_by_state(State) ->
    case deliv_db:qfetch(?MODULE, fetch_by_state, [State]) of
        [] ->
            {error, not_found};
        [Record] ->
            {ok, Record};
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, fetch_by_state, [State], Why),
            Err
    end.

%% @doc Fetch a token record by the records scope (ent/org/proj/pipe and ID) and
%% type as defined by the module.
-spec fetch(atom(), integer(), atom()) -> db_op_single_result(d_oauth_token()).
fetch(Scope, ScopeId, Module) ->
    case deliv_db:qfetch(?MODULE, fetch_by_scope_and_module, [Scope, ScopeId, Module]) of
        [] ->
            {error, not_found};
        [Record] ->
            {ok, Record};
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, fetch, [Scope, ScopeId, Module], Why),
            Err
    end.

%% @doc Fetch a token record by the enterprise name and module.
-spec fetch_by_enterprise(binary(), atom()) -> db_op_single_result(d_oauth_token()).
fetch_by_enterprise(EntName, Module) ->
    case deliv_db:qfetch(?MODULE, fetch_by_enterprise, [EntName, Module]) of
        [] ->
            {error, not_found};
        [Record] ->
            {ok, Record};
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, fetch_by_enterprise, [EntName, Module], Why),
            Err
    end.

%% @doc Call out to the Oauth module to fetch the token
-spec fetch_token(binary(), d_oauth_token()) -> {ok, binary()} | {error, {integer(), binary()}}.
fetch_token(Code, TokenRecord=#deliv_oauth_token{oauth_app_id = OauthAppId}) ->
    {ok, Application} = deliv_oauth_application:fetch_by_id(OauthAppId),
    OauthModule = deliv_oauth_application:get_module(Application),
    case OauthModule:fetch_token(Code, Application) of
        {ok, _Token} = Result ->
            Result;
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, fetch_token, [Code, TokenRecord], Why),
            Err
    end.

%% @doc Call out to the Oauth module to save the token into the database.
-spec save_token(binary(), d_oauth_token()) -> db_op_single_result(d_oauth_token()).
save_token(Token, Record = #deliv_oauth_token{oauth_app_id = OauthAppId}) ->
    OauthModule = deliv_oauth_application:get_module_by_id(OauthAppId),
    case OauthModule:save_token(Token, Record) of
        {ok, _UpdatedRecord} = Result ->
            Result;
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, save_token, [Token, Record], Why),
            Err
    end.
