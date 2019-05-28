-module(auth_saml_config).

-include_lib("auth_types.hrl").
-include_lib("esaml/include/esaml.hrl").

-compile({parse_transform, sqerl_gobot}).

-export([
         fetch/1,
         identity_provider/1,
         service_provider/2,
         service_provider_metadata/1,
         periodic_metadata_refresh/0,
         refresh_metadata/1,
         refresh_metadata_with_retry/3,
         upsert/1,
         delete/1,
         from_json/2,
         to_json/1
        ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-record(auth_saml_config, {id            :: db_id(),
                           enterprise_id :: db_id(),
                           sso_login_url :: binary(),
                           sso_binding   :: binary(),
                           idp_url       :: binary(),
                           cert          :: binary(),
                           name_id       :: binary(),
                           metadata_url  :: binary(),
                           metadata_xml  :: binary(),
                           default_roles :: [binary()]
                          }).

'#insert_fields'() ->
    [enterprise_id,
     sso_login_url,
     sso_binding,
     idp_url,
     cert,
     name_id].
'#update_fields'() ->
    [sso_login_url,
     sso_binding,
     idp_url,
     cert,
     name_id].
'#statements'() ->
    [default,
    {fetch_by_enterprise_name,
     <<"SELECT sc.* FROM saml_config sc
                    JOIN enterprises e
                      ON sc.enterprise_id = e.id
                   WHERE e.name = $1">>},

    {delete_by_enterprise_name,
        <<"DELETE FROM saml_config sc
                  USING enterprises e
                  WHERE sc.enterprise_id = e.id
                  AND e.name = $1">>},

    {upsert,
     <<"SELECT *
          FROM upsert_saml_config($1, $2, $3, $4, $5, $6, $7, $8, $9)">>}].

'#table_name'() ->
    "saml_config".

-spec fetch(binary()) -> {ok, d_saml_config()} | {error, not_found | term()}.
fetch(EntName) ->
    handle_return(
        deliv_db:qfetch(?MODULE, fetch_by_enterprise_name, [EntName]), EntName).

-spec upsert(#saml_config{}) -> {ok, #saml_config{}} | {error, not_found | term()}.
upsert(#saml_config{ent_name = EntName,
                    sso_login_url = SSOLoginUrl,
                    sso_binding = SSOBinding,
                    idp_url = IdPUrl,
                    cert = Cert,
                    name_id = NameId,
                    metadata_url = MetadataUrl,
                    metadata_xml = MetadataXml,
                    default_roles = DefaultRoles}) ->
    handle_return(
        deliv_db:qfetch(?MODULE,
                        upsert,
                          [EntName, SSOLoginUrl, SSOBinding, IdPUrl, Cert,
                           NameId, MetadataUrl, MetadataXml, DefaultRoles]),
        EntName).

-spec delete(binary()) -> ok | {error, term()}.
delete(EntName) ->
    case sqerl_rec:cquery(?MODULE, delete_by_enterprise_name, [EntName]) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

%% @doc Constructs an service provider record for an enterprise
-spec service_provider(binary(), binary() | [binary()]) -> {ok, #esaml_sp{}} | {error, any()}.
service_provider(EntName, Cert) when is_binary(Cert) ->
    service_provider(EntName, [Cert]);
service_provider(EntName, Certs) ->
    %% esaml_sp:setup/1 just calls error/1 if it's unhappy with its input, wrap that
    ConsumeUri = deliv_web_utils:api_url_for(saml_consume, [EntName]),
    MetadataUri = entity_id(EntName), % esaml uses metadata_uri as EntityId
    FingerPrints = lists:map(fun auth_saml_utils:calculate_fingerprint/1, Certs),
    try esaml_sp:setup(#esaml_sp{consume_uri = erlang:binary_to_list(ConsumeUri),
                                 metadata_uri = MetadataUri,
                                 idp_signs_envelopes = false,
                                 idp_signs_assertions = true,
                                 trusted_fingerprints = FingerPrints
                                }) of
        ServiceProvider -> {ok, ServiceProvider}
    catch
        error:Error ->
            % don't log FingerPrints even though it is a param
            chef_log:failed_call(esaml_sp, setup, [ConsumeUri, MetadataUri, false, true], Error),
            {error, Error}
    end.

%% @doc Constructs an service provider metadata record for an enterprise
-spec service_provider_metadata(binary()) -> #esaml_sp_metadata{}.
service_provider_metadata(EntName) ->
    ConsumeUri = deliv_web_utils:api_url_for(saml_consume, [EntName]),
    EntityId = entity_id(EntName),
    #esaml_sp_metadata{consumer_location = erlang:binary_to_list(ConsumeUri),
                       entity_id = EntityId,
                       signed_requests = false,
                       signed_assertions = true,
                       org = #esaml_org{
                                name = "Chef Automate",
                                displayname = "Chef Automate",
                                url = "https://www.chef.io/automate/"
                               }
                      }.

%% @doc Retrieves enterprise SAML config and returns an idp metadata record +
%% binding type + name-id
-spec identity_provider(binary()) -> {ok, #esaml_idp_metadata{}, binary(), binary()} | {error, any()}.
identity_provider(EntName) ->
    case fetch(EntName) of
        {ok, #saml_config{sso_binding = SSOBinding,
                          name_id = NameId} = Config} ->
            handle_metadata(build_metadata(Config), SSOBinding, NameId);
        {error, _} = Error -> Error
    end.

build_metadata(#saml_config{metadata_xml = MetadataXML})
  when is_binary(MetadataXML) ->
    %TODO: move this dependency to esaml?
    {Xml, _Rest} = xmerl_scan:string(erlang:binary_to_list(MetadataXML), [{namespace_conformant, true}]),
    case esaml:decode_idp_metadata(Xml) of
      {ok, _Metadata} = Success -> Success;
      {error, Why} = Error -> chef_log:error("eSAML XML decode failure: ~n~p", [Why]),
                              Error
    end;
build_metadata(#saml_config{sso_login_url = SSOLoginUrl, idp_url = IdPUrl, cert = Cert})
  when is_binary(SSOLoginUrl) andalso is_binary(IdPUrl)->
    {ok, #esaml_idp_metadata{certificates = [Cert],
                             entity_id = erlang:binary_to_list(IdPUrl),
                             login_location_redirect = erlang:binary_to_list(SSOLoginUrl)}};
build_metadata(_) ->
    chef_log:error("SAML configuration needs either manual configuration or XML metadata."),
    {error, malformed_config}.

handle_metadata({error, _} = Error, _, _) ->
    Error;
handle_metadata({ok, IdPMetadata}, SSOBinding, NameId) ->
    {ok, IdPMetadata, SSOBinding, NameId}.

%% @doc Translates the db_op_result into a handier error format (and the same
%% format that deliv_db:update/1 uses)
-spec handle_return(db_op_result(Type), binary()) -> {ok, Type} | {error, not_found | term()}.
handle_return([], _) -> {error, not_found};
handle_return([Config], EntName) -> {ok, deserialize(Config, EntName)};
handle_return({error, Why}, _) ->
    chef_log:error("An internal server error occurred in trying to perform a db operation on SAML configuration. ~n~p", [Why]),
    {error, Why}.

-spec deserialize(#auth_saml_config{}, binary()) -> #saml_config{}.
deserialize(#auth_saml_config{enterprise_id = _, % fetched this already
                              sso_login_url = SsoLoginUrl,
                              sso_binding = SsoBinding,
                              idp_url = IdPUrl,
                              cert = Cert,
                              name_id = NameId,
                              metadata_url = MetadataUrl,
                              metadata_xml = MetadataXml,
                              default_roles = DefaultRoles}, EntName) ->
    TranslatedDefaultRoles = case DefaultRoles of
        undefined -> ?DEFAULT_USER_ROLES;
        Roles -> Roles
    end,
    #saml_config{ent_name = EntName,
                 sso_login_url = SsoLoginUrl,
                 sso_binding = SsoBinding,
                 idp_url = IdPUrl,
                 cert = Cert,
                 name_id = NameId,
                 metadata_url = MetadataUrl,
                 metadata_xml = MetadataXml,
                 default_roles = TranslatedDefaultRoles}.

%% @doc Returns EntityId from application config
-spec entity_id(binary()) -> string().
entity_id(EntName) ->
    case  application:get_env(auth, saml_entity_id) of
        {ok, EntityId} -> EntityId;
        undefined -> binary_to_list(deliv_web_utils:api_url_for(saml_metadata, [EntName]))
    end.

-spec refresh_metadata(binary() | #saml_config{}) -> {ok, noop} |
                                                     {ok, #saml_config{}} |
                                                     {error, not_found | too_big | term()} |
                                                     {error, status, any()}.
refresh_metadata(EntName) when is_binary(EntName) ->
    case fetch(EntName) of
        {ok, Config} -> refresh_metadata(Config);
        _ -> {ok, noop}
    end;
refresh_metadata(#saml_config{metadata_url = undefined} = Config) ->
    {ok, Config};
refresh_metadata(#saml_config{metadata_url = Url} = Config) ->
    case deliv_http:req(Url) of
        {ok, 200, _, Xml} when erlang:byte_size(Xml) < ?MAXSIZE_METADATA_XML ->
            chef_log:info("Retrieved SAML metadata from ~p successfully.",
                           [Url]),
            {ok, Config#saml_config{metadata_xml = Xml}};
        {ok, 200, _, _} ->
            chef_log:error("Failed to retrieve metadata from ~p: ~s",
                           [Url, auth_saml_utils:metadata_too_big_error_message()]),
            {error, too_big};
        {error, Status} ->
            chef_log:error("Failed to retrieve metadata from ~p: received status ~p",
                           [Url, Status]),
            {error, status, Status};
        {_, Status, _, _} ->
            chef_log:error("Failed to retrieve metadata from ~p: received status ~p",
                           [Url, Status]),
            {error, status, Status}
    end.

-spec to_json(#saml_config{}) -> json().
to_json(#saml_config{sso_login_url = SsoLoginUrl,
                     sso_binding = SsoBinding,
                     idp_url = IdpUrl,
                     cert = Cert,
                     name_id = NameId,
                     metadata_url = MetadataUrl,
                     default_roles = DefaultRoles
                    }) ->
    {[{<<"sso_login_url">>, SsoLoginUrl},
      {<<"sso_binding">>, SsoBinding},
      {<<"idp_url">>, IdpUrl},
      {<<"cert">>, Cert},
      {<<"name_id">>, NameId},
      {<<"metadata_url">>, MetadataUrl},
      {<<"default_roles">>, DefaultRoles}
     ]}.

-spec from_json(json(), binary()) -> #saml_config{}.
from_json(Ejson, EntName) ->
    SSOLoginUrl = ej:get([<<"sso_login_url">>], Ejson),
    SSOBinding = ej:get([<<"sso_binding">>], Ejson),
    IdPUrl = ej:get([<<"idp_url">>], Ejson),
    Cert = ej:get([<<"cert">>], Ejson),
    NameId = ej:get([<<"name_id">>], Ejson),
    MetadataUrl = ej:get([<<"metadata_url">>], Ejson),
    DefaultRoles = ej:get([<<"default_roles">>], Ejson),


    #saml_config{ent_name = EntName,
                 sso_login_url = SSOLoginUrl,
                 sso_binding = SSOBinding,
                 idp_url = IdPUrl,
                 cert = Cert,
                 name_id = NameId,
                 metadata_url = MetadataUrl,
                 default_roles = DefaultRoles}.

-spec periodic_metadata_refresh() -> {ok | error, term()}.
periodic_metadata_refresh() ->
    [Interval, RetryInterval] = refresh_intervals(),
    case deliv_enterprise:get_canonical_enterprise() of
        {ok, EntName} when Interval > 0 ->
            chef_log:info("Init SAML metadata auto-refresh, refresh timer: ~p ms, retry timer: ~p ms", [Interval, RetryInterval]),
            timer:apply_interval(Interval, ?MODULE, refresh_metadata_with_retry, [EntName, RetryInterval, 0]);
        {ok, _} ->
            chef_log:info("Init SAML metadata auto-refresh disabled (no refresh interval configured)"),
            {error, not_configured};
        {error, Why} = Error ->
            chef_log:error("Init SAML metadata auto-refresh disabled (unable to get canonical enterprise: ~p)", [Why]),
            Error
    end.

-spec refresh_metadata_with_retry(binary(), integer(), integer()) -> ok.
refresh_metadata_with_retry(EntName, RetryInterval, RetriesAttempted) ->
    case save_metadata(refresh_metadata(EntName)) of
      ok -> chef_log:info("Auto refresh SAML metadata: success");
      error when RetryInterval =< 0 ->
          chef_log:error("Auto refresh SAML metadata: retries disabled (retry interval not configured)");
      error when RetriesAttempted < ?SAML_METADATA_MAX_RETRIES ->
          chef_log:info("Auto refresh SAML metadata: retry (attempt ~p)", [RetriesAttempted + 1]),
          timer:apply_after(RetryInterval, ?MODULE, refresh_metadata_with_retry, [EntName, RetryInterval, RetriesAttempted + 1]);
      _ ->
          chef_log:info("Auto refresh SAML metadata: giving up (attempt ~p)", [RetriesAttempted + 1])
    end, ok.

save_metadata({ok, noop}) -> ok;
save_metadata({ok, Config}) ->
    case upsert(Config) of
        {ok, _} -> ok;
        {error, _} -> error
    end;
save_metadata({error, status, _}) -> error;
save_metadata({error, _}) -> error.

refresh_intervals() ->
    [
        timer:seconds(
            case application:get_env(auth, saml_metadata_refresh_interval) of
               undefined -> 0;
               {ok, Val} -> Val
            end),
        timer:seconds(
            case application:get_env(auth, saml_metadata_retry_interval) of
                undefined -> 0;
                {ok, Val} -> Val
            end)
    ].
