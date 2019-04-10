defmodule Notifications.Application do
  @moduledoc false
  require Logger

  use Application
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    enable_sync_if_available()

    # Before we start accepting requests, ensure the latest schema and updates are applied
    :ok = Notifications.Data.Migrator.run()
    :ok = Notifications.Data.Store.startup()
    Notifications.Data.A1Migrator.run()

    port = Notifications.Config.grpc_port
    Logger.info("GRPC will be listening on port #{port}")
    # HTTPoison ships with with integrated pooling support.
    # The child_spec returns a worker spec for managing the pool.
    # We could also use the :default pool and skip this step, but we
    # can't control pool options in that case.
    pool_spec = :hackney_pool.child_spec(:webhook,
                                         Notifications.Config.http_pool_config)

    cred = GRPC.Credential.new(Notifications.Config.ssl_options)

    secrets_channel = create_secrets_channel()

    children = [
      supervisor(GRPC.Server.Supervisor,
                 [{Notifications.Service, port, cred: cred}]),
      worker(Notifications.Dispatcher, [{Notifications.Dispatcher, []}]),
      worker(Notifications.WebhookSender, [{Notifications.WebhookSender, []}]),
      worker(Notifications.Data.TimedCache, [{Notifications.Data.TimedCache, []}]),
      worker(Notifications.Data.SecretStore, [secrets_channel], []),
      pool_spec
    ]
    opts = [strategy: :one_for_one, name: Notifications.Supervisor,
            restart: :permanent]
    Supervisor.start_link(children, opts)
  end

  defp create_secrets_channel() do
    secrets_service_cred = GRPC.Credential.new(ssl: Notifications.Config.secrets_ssl_options)
    secrets_url = Notifications.Config.secrets_url
    {:ok, channel} = GRPC.Stub.connect(secrets_url, [cred: secrets_service_cred])

    channel
  end

  # If ExSync (dev code syncing) is present,
  # we'll go ahead and start it. It will not be present
  # in non-dev builds
  defp enable_sync_if_available()  do
    case Code.ensure_loaded(ExSync) do
      {:module, ExSync = name} ->
        # By using 'name' instead of ExSync here, we avoid a compile warning
        # in non-dev mode that ExSync does not exist.
        name.start()
      {:error, :nofile} ->
        :ok
      {:error, error} ->
        Logger.warn("Could not start ExSync: #{inspect(error)}.")
        :ok
      error ->
        Logger.warn("Unknown error in starting ExSync: #{inspect(error)}.")
    end
  end
end
