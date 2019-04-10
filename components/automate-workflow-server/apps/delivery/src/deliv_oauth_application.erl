%% @doc Interface to manage oauth applications.
-module(deliv_oauth_application).

-include("deliv_types.hrl").

-compile({parse_transform, sqerl_gobot}).

%% API
-export([
         list/0,
         insert/6,
         fetch/1,
         fetch_by_id/1,
         fetch_by_token_record/1,
         fetch_by_token_details/3,
         delete/1,
         get_module/1,
         get_module_by_id/1
        ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-record(deliv_oauth_application, {
          id            :: db_id(),
          name          :: binary(),
          module        :: binary(),
          root_url      :: binary(),
          root_api_url  :: binary(),
          client_id     :: binary(),
          client_secret :: binary()
         }).

%%
%% Callbacks
%%

%% @doc return the URL used to request authorization code.
-callback authorize_url(State :: binary(), Application :: d_oauth_application()) -> iolist().

%% @doc return the URL used to request access token.
-callback access_token_url(Code :: binary(), Application :: d_oauth_application()) -> iolist().

%% @doc call out to the OAuth API and return the token.
-callback fetch_token(Code :: binary(), Application :: d_oauth_application()) -> {ok, binary()} | {error, {integer(), binary()}}.

%% @doc save the token into the database
-callback save_token(Token :: binary(), OauthRecord :: d_oauth_token()) -> {ok, d_oauth_token()} | {error, atom()}.

%% Those are not actually used since we do
%% all the insert/update operations through custom DB funs
'#insert_fields'() -> [name, module, root_url, root_api_url, client_id, client_secret].
'#update_fields'() -> [].

'#table_name'() -> "external_oauth_applications".

'#statements'() ->
    [default,
     {fetch_by_name, sqerl_rec:gen_fetch(?MODULE, [name])},
     {fetch_oauth_applications,
      <<"SELECT * FROM external_oauth_applications">>},
     {fetch_by_scope_and_module,
      <<"SELECT o.app_id AS id, "
              " o.app_name AS name, "
              " o.module, "
              " o.root_url, "
              " o.root_api_url, "
              " o.client_id, "
              " o.client_secret "
          "FROM oauth_integrations AS o "
         "WHERE scope=$1 "
           "AND scope_id=$2 "
           "AND module=$3">>}
    ].

%% @doc Insert a new application
-spec insert(binary(), binary(), binary(), binary(), binary(), binary()) -> db_op_result(d_oauth_application()).
insert(Name, Module, RootUrl, RootApiUrl, ClientId, ClientSecret) ->
    deliv_db:insert(#deliv_oauth_application{name = Name,
                                             module = Module,
                                             root_url = RootUrl,
                                             root_api_url = RootApiUrl,
                                             client_id = ClientId,
                                             client_secret = ClientSecret}).

%% @doc Fetch the list of OAuth Applications configured in the system
-spec list() -> {ok, [d_oauth_application()]} | {error, atom()}.
list() ->
    case deliv_db:qfetch(?MODULE, fetch_oauth_applications, []) of
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, fetch_oauth_applications, [], Why),
            Err;
        [] ->
            {ok, empty_list};
        [App] ->
            {ok, [App]};
        ListApplications ->
            {ok, ListApplications}
     end.

%% @doc Fetch the OAuth application (by name)
-spec fetch(binary()) -> {ok, d_oauth_application()} | {error, atom()}.
fetch(AppName) ->
    case deliv_db:qfetch(?MODULE, fetch_by_name, [AppName]) of
        [] ->
            chef_log:error("Failed to fetch OAuth application ~s - not_found", [AppName]),
            {error, not_found};
        [Application] ->
            {ok, Application};
        {error, Why} = Err ->
            chef_log:error("Failed to fetch OAuth application ~s - ~p", [AppName, Why]),
            Err
    end.

%% @doc Fetch the application by ID
-spec fetch_by_id(non_neg_integer()) -> {ok, d_oauth_application()} | {error, atom()}.
fetch_by_id(AppId) ->
    deliv_db:fetch_by_id(?MODULE, AppId).

%% @doc Fetch the application by an Oauth Record
-spec fetch_by_token_record(d_oauth_token()) -> {ok, d_oauth_application()} | {error, atom()}.
fetch_by_token_record(OauthTokenRecord) ->
    AppId = deliv_oauth_token:getval(oauth_app_id, OauthTokenRecord),
    fetch_by_id(AppId).

-spec fetch_by_token_details(atom(), non_neg_integer(), atom()) -> {ok, d_oauth_application()} | {error, atom()}.
fetch_by_token_details(Scope, ScopeId, Module) ->
    case deliv_db:qfetch(?MODULE, fetch_by_scope_and_module, [Scope, ScopeId, Module]) of
        [] ->
            chef_log:error("Could not find an Oauth Application associated with ~p ~p ~p",
                            [Scope, ScopeId, Module]),
            {error, not_found};
        [Application] ->
            {ok, Application};
        {error, Why} = Err ->
            chef_log:error("Failed to fetch Oauth Application - ~p", [Why]),
            Err
    end.


%% @doc Delete the OAuth application (by name)
-spec delete(binary()) -> ok | {error, atom()}.
delete(Name) ->
    case fetch(Name) of
        {ok, Record} ->
            deliv_db:delete(Record);
        {error, _Why} = Err ->
            Err
    end.

%% @doc Get the module (in atom form) for the specified application.
-spec get_module(d_oauth_application()) -> atom().
get_module(App) ->
    chef_utils:to_atom(getval(module, App)).

%% @doc Get the module (in atom form) for the application (by id).
-spec get_module_by_id(integer()) -> atom().
get_module_by_id(AppId) ->
    {ok, Application} = fetch_by_id(AppId),
    get_module(Application).
