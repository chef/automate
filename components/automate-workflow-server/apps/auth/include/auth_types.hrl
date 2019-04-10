-include_lib("delivery/include/deliv_types.hrl").

-record(saml_config, {ent_name :: binary(),
                      sso_login_url :: binary(),
                      sso_binding :: binary(),
                      idp_url :: binary(),
                      cert :: binary(),
                      name_id :: binary(),
                      metadata_url :: binary(),
                      metadata_xml :: binary(),
                      default_roles :: [binary()]}).

-define(MAXSIZE_METADATA_XML, 1000000).
-define(SAML_METADATA_MAX_RETRIES, 5). % five retries
-define(LOGIN_TIMEOUT_MILLIS, 900000). %15 minutes
-define(DEFAULT_USER_ROLES, [<<"observer">>]).
