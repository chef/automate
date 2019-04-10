-module(deliv_basic_auth_application).

-include("deliv_types.hrl").

%% API functions
-export([
         delete_by_enterprise_name_url_app_name/3,
         encrypt_and_insert/5,
         encrypt_and_update/5,
         fetch_by_enterprise_name_app_name_and_decrypt/2,
         fetch_by_enterprise_name_url_app_name_and_decrypt/3
        ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-compile({parse_transform, sqerl_gobot}).

-ifdef(TEST).
-compile(export_all).
-endif.

-record(deliv_basic_auth_application, {
          name          :: binary(),
          root_api_url  :: binary(),
          user_id       :: binary(),
          password      :: binary(),
          ent_id        :: db_id()
         }).

 '#insert_fields'() -> [name, root_api_url, user_id, password, ent_id].
 '#update_fields'() -> [].

 '#table_name'() -> "external_basic_auth_applications".

 '#statements'() -> [default,
                     {delete_by_enterprise_name_url_app_name,
                      <<"DELETE
                           FROM external_basic_auth_applications
                          WHERE ent_id = (SELECT id FROM enterprises WHERE name = $1)
                            AND name = $2
                            AND root_api_url = $3">>
                     },
                     {fetch_by_enterprise_name_app_name,
                      <<"SELECT baa.*
                           FROM external_basic_auth_applications baa,
                                enterprises ent
                          WHERE baa.ent_id = ent.id
                            AND ent.name = $1
                            AND baa.name = $2">>
                     },
                     {fetch_by_enterprise_name_url_app_name,
                      <<"SELECT baa.*
                           FROM external_basic_auth_applications baa,
                                enterprises ent
                          WHERE baa.ent_id = ent.id
                            AND ent.name = $1
                            AND baa.name = $2
                            AND baa.root_api_url = $3">>
                     },
                     {update_by_ent_name_url,
                      <<"UPDATE external_basic_auth_applications baa
                            SET root_api_url = $2,
                                user_id = $3,
                                password = $4
                          WHERE baa.ent_id = $5
                            AND baa.name = $1
                      RETURNING baa.*">>
                     }
                    ].

%% @doc Delete application given enterprise name, app name, and url
-spec delete_by_enterprise_name_url_app_name(binary(), binary(), binary()) -> ok | {error, term()}.
delete_by_enterprise_name_url_app_name(EntName, Name, RootApiUrl) ->
    case sqerl_rec:cquery(?MODULE,
                          delete_by_enterprise_name_url_app_name,
                          [EntName, Name, RootApiUrl]) of
        {ok, 0} ->
            chef_log:debug("Could not delete deliv_basic_auth_application "
                            ++ " ~s:~s for enterprise ~s : not found",
                            [Name, RootApiUrl, EntName]),
            {error, not_found};
        {ok, 1} ->
            chef_log:debug("Deleted deliv_basic_auth_application "
                            ++ " ~s:~s for enterprise ~s",
                            [Name, RootApiUrl, EntName]),
            ok;
        {error, _Why} = Error ->
            chef_log:debug("Failed to delete deliv_basic_auth_application "
                            ++ " ~s:~s for enterprise ~s",
                            [Name, RootApiUrl, EntName]),
            Error
    end.

%% @doc Insert a new application encrypting the password
-spec encrypt_and_insert(binary(), binary(), binary(), binary(), db_id()) -> {ok, d_basic_auth_application()} | {error, atom()}.
encrypt_and_insert(Name, RootApiUrl, UserId, Password, EnterpriseId) ->
    ToBeInserted = #deliv_basic_auth_application{
                      name = Name,
                      root_api_url = RootApiUrl,
                      user_id = UserId,
                      password = base64:encode(Password),
                      ent_id = EnterpriseId
                     },

    case deliv_db:insert(ToBeInserted) of
        [DBResult] -> {ok, decrypt_password(DBResult)};
        {error, {conflict, _}} -> {error, conflict};
        DBResult -> DBResult
    end.

%% @doc Update an application encrypting the password
-spec encrypt_and_update(binary(), binary(), binary(), binary(), db_id()) -> {ok, d_basic_auth_application()} | {error, atom()}.
encrypt_and_update(Name, RootApiUrl, UserId, Password, EnterpriseId) ->
    case sqerl_rec:qfetch(?MODULE, update_by_ent_name_url,
                          [Name, RootApiUrl, UserId,
                           base64:encode(Password), EnterpriseId]) of
        [DBResult] -> {ok, decrypt_password(DBResult)};
        {error, {{ok, 0}, _}} -> chef_log:info("DBOP"), {error, not_found};
        DBResult -> DBResult
    end.

%% @doc Fetches the basic auth application by name and decrypts the password.
-spec fetch_by_enterprise_name_app_name_and_decrypt(binary(), binary()) -> {ok, d_basic_auth_application()} | {error, atom()}.
fetch_by_enterprise_name_app_name_and_decrypt(EntName, Name) ->
    case sqerl_rec:qfetch(?MODULE, fetch_by_enterprise_name_app_name, [EntName, Name]) of
        [EncryptedRec] -> {ok, decrypt_password(EncryptedRec)};
        [] -> {error, not_found};
        DBResult -> DBResult
    end.

-spec fetch_by_enterprise_name_url_app_name_and_decrypt(binary(), binary(), binary()) -> {ok, d_basic_auth_application()} | {error, atom()}.
fetch_by_enterprise_name_url_app_name_and_decrypt(EntName, Name, Url) ->
    case sqerl_rec:qfetch(?MODULE, fetch_by_enterprise_name_url_app_name, [EntName, Name, Url]) of
        [BasicAuth] -> {ok, decrypt_password(BasicAuth)};
        [] -> {error, not_found};
        DBResult -> DBResult
    end.

%% @doc Decrypts a password, using base64 for now
%% See note above on encryption thoughts.
-spec decrypt_password(d_basic_auth_application()) -> d_basic_auth_application().
decrypt_password(EncryptedBasicAuth
                  = #deliv_basic_auth_application{password=EncryptedPassword}) ->
  EncryptedBasicAuth#deliv_basic_auth_application{password=base64:decode(EncryptedPassword)}.
