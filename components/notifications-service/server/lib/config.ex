defmodule Notifications.Config do
  @moduledoc """
  All runtime configuration settings are managed via environment variables and are
  documented below.

  This module provides consolidation location to retrieve - and sometimes set - these values,
  so that the callers don't need to care about how the config settings are implemented.
  """
  # Port on which GRPC listens
  @grpc_port_var "PORT"
  # Used to construct URLs in certain webhook/slack payloads
  @automate_fqdn_var "EXTERNAL_FQDN"
  # Enable capture of all inbound protobuf payloads to logs/ directory.
  @capture_payload_var "DEV_PAYLOAD_CAPTURE"
  # Cache refresh interval for `Store.get_rules_for_event` which is executed
  # for any inbound notifications that is not dropped in pre-filter.
  @event_rule_cache_timeout_var "CACHE_TIMEOUT_MS_EVENT_RULE"

  # How long in seconds must pass before we consider
  # an inbound event with the same id and type as not a duplicate
  @dedup_window_var "EVENT_DEDUP_WINDOW_SECONDS"

  def grpc_port do
    String.to_integer(get_value(@grpc_port_var, "4001"))
  end

  def automate_fqdn do
    get_value(@automate_fqdn_var, "http://localhost")
  end

  def dev do
    get_value("CHEF_DEV_ENVIRONMENT", "false")
  end

  def proxy_settings do
    proxy_port = get_value("PROXY_PORT", "")
    %{host: get_value("PROXY_HOST", ""),
      port: proxy_port,
      port_int: Integer.parse(proxy_port),
      user: get_value("PROXY_USER", ""),
      password: get_value("PROXY_PASSWORD", ""),
      no_proxy: get_value("NO_PROXY", "")}
  end

  def capture_payloads? do
    value = get_value(@capture_payload_var, "off")
    flag_enabled?(value)
  end

  def capture_payloads! do
    set(@capture_payload_var, "on")
  end

  def dedup_window_seconds() do
     String.to_integer(get_value(@dedup_window_var, "300"))
  end

  def http_pool_config() do
    # TODO - if testing finds a need for it, expose these
    #        as tunables
    [timeout: 15_000, max_connections: 10]
  end

  def event_rule_cache_timeout_ms() do
    String.to_integer(get_value(@event_rule_cache_timeout_var, "5000"))
  end

  def sqerl_settings do
    [db_name: 'notifications_service',
     db_host: to_charlist(get_value("SQERL_DBHOST", "localhost")),
     db_port: String.to_integer(get_value("SQERL_DBPORT", nil)),
     db_user: to_charlist(get_value("SQERL_DBUSER", "notifications")),
     db_pass: to_charlist(get_value("SQERL_DBPASSWORD", "")),
     db_options: db_options(),
     idle_check: String.to_integer(get_value("SQERL_IDLE_CHECK", "1000")),
     pooler_timeout: String.to_integer(get_value("SQERL_POOLER_TIMEOUT", "100")),
     column_transforms: [],
     prepared_statements: {Notifications.Data.Store, :statements, []}
    ]
  end

  def db_options do
    cond do
      sqerl_no_ssl?() -> 
        [ssl: false]
      sqerl_no_ssl_auth()? ->
        [ssl: true, ssl_opts: Keyword.get(ssl_options_no_auth(), :ssl)]
      true ->
        [ssl: true, ssl_opts: Keyword.get(ssl_options(), :ssl)]
    end
  end

  def ssl_options do
    [ssl: [
      certfile: get_value("SERVICE_CERT", nil),
      keyfile: get_value("SERVICE_KEY", nil),
      cacertfile: get_value("ROOT_CA_CERT", nil),
      fail_if_no_peer_cert: true,
      verify: :verify_peer,
      versions: [:'tlsv1.2'],
    ]]
  end

  def ssl_options_no_auth do
    [ssl: [
      cacertfile: get_value("EXTERNAL_PG_ROOT_CA_CERT", nil),
      verify: :verify_peer,
      versions: [:'tlsv1.2'],
    ]]
  end

  def sqerl_no_ssl? do
    get_value("SQERL_NO_SSL", "") == "true"
  end

  def sqerl_no_ssl_auth? do
    get_value("SQERL_NO_SSL_AUTH", "") == "true"
  end

  def secrets_ssl_options do
    [
      certfile: get_value("SECRETS_SERVICE_CERT", nil),
      keyfile: get_value("SECRETS_SERVICE_KEY", nil),
      cacertfile: get_value("SECRETS_ROOT_CA_CERT", nil),
      versions: [:'tlsv1.2'],
    ]
  end

  def secrets_url do
    get_value("SECRETS_URL", nil)
  end

  def rule_store_for_migration do
    get_value("MIGRATION_RULE_STORE_FILE", "")
  end

  def migration_status_file do
    get_value("MIGRATION_STATUS_FILE", "")
  end

  def set(var, value) do
    System.put_env(var, value)
  end

  def get_value(var, default_value) do
    {:ok, value} = get(var, default_value)
    value
  end

  def get(var, default_value \\ nil) do
    case System.get_env(var) do
      nil ->
        case default_value do
          nil ->
            {:error, {:missing_env_var, var}}
          _ ->
            {:ok, default_value}
        end
      value ->
        {:ok, value}
    end
  end

  defp flag_enabled?("on"), do: true
  defp flag_enabled?(_),  do: false
end
