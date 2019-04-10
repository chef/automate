defmodule Notifications.Data.SecretStore do
  use GenServer
  alias Chef.Automate.Api.Secrets
  require Logger

  def start_link(channel) do
    GenServer.start_link(__MODULE__, channel, name: __MODULE__)
  end

  def init(channel) do
    {:ok,  %{channel: channel}}
  end

  def delete_secret(secret_id) do
    Logger.debug fn() -> "delete secret with id '#{secret_id}'" end

    GenServer.call(__MODULE__, {:delete, secret_id})
  end

  def get_target_username_password(secret_id) do
    Logger.debug fn() -> "get secret with id '#{secret_id}'" end

    GenServer.call(__MODULE__, {:read, secret_id})
  end

  def handle_call({:delete, secret_id}, _sender, %{channel: channel} = state)  do
    Logger.debug fn() -> "call delete secret with id '#{secret_id}'" end

    response = case Secrets.SecretsService.Stub.delete(channel, %Secrets.Id{id: secret_id}) do
      {:ok, _} -> {:ok}
      {:error, _} = error -> error
    end

    {:reply, response, state}
  end
  def handle_call({:read, id}, _sender, %{channel: channel} = state)  do
    Logger.debug fn() -> "retrieving username and password value for id: '#{id}'" end

    results = case Secrets.SecretsService.Stub.read(channel, %Secrets.Id{id: id}) do
      {:ok, secret} ->
        username = find_kv_value(secret.data, "username", id)
        password = find_kv_value(secret.data, "password", id)

        {:ok, {username, password}}
      {:error, _} = error -> error
    end

    {:reply, results, state}
  end

  # This prevents the below error when calls are made to the secrets service
  # Notifications.Data.SecretStore Notifications.Data.SecretStore received unexpected message in handle_info/2: {:RECV_DATA, 5, <<0, 0, 0, 0, 0>>}
  def handle_info({:RECV_DATA, _, _}, state) do
    {:noreply, state}
  end

  defp find_kv_value(kvs, key, secret_id) do
    case Enum.find(kvs, fn(kv) -> kv.key == key end) do
      %{value: found_value} -> found_value
      nil ->
        Logger.error fn() -> "error finding key '#{key}' with secret ID: '#{secret_id}'" end
        ""
    end
  end
end
